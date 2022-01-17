open Base
open Cil
open Utils

(* ========================================================================== *)
(* 1 - Collect all the loops. *)
class loopCollector (m_loops : (int, stmt) Hashtbl.t) =
  object
    inherit nopCilVisitor

    method! vstmt (s : stmt) =
      match s.skind with
      | Loop (_, _, _, _) ->
        Hashtbl.set m_loops ~key:s.sid ~data:s;
        (* Skip children: nested loops will be handled after. *)
        SkipChildren
      | _ -> DoChildren
  end

let block_of_loop (s : stmt) =
  match s.skind with
  | Loop (b, _, _, _) -> Some b
  | _ -> None
;;

let find_sid (fd : fundec) (sid : int) =
  match List.filter fd.sallstmts ~f:(fun stmt -> stmt.sid = sid) with
  | hd :: _ -> Some hd
  | [] -> None
;;

let get_stmt_rds (_fd : fundec) (dominates : stmt -> bool) (sid : int) =
  let open Reachingdefs in
  let extract_defs (ios : IOS.t) =
    IOS.fold
      (fun def_id defs ->
        match def_id with
        | Some def_id ->
          (try
             let rhs = getSimpRhs def_id in
             let def_stmt =
               try Some (Inthash.find ReachingDef.defIdStmtHash def_id) with
               | Caml.Not_found -> None
             in
             let is_pre_loop =
               match def_stmt with
               | Some stmt -> dominates stmt
               | None -> false
             in
             if is_pre_loop then Map.set defs ~key:def_id ~data:rhs else defs
           with
          | _ -> defs)
        | None -> defs)
      ios
      (Map.empty (module Int))
  in
  let f (_, _, ios_ih) =
    IH.fold
      (fun vid ios vid_map -> Map.set vid_map ~key:vid ~data:(extract_defs ios))
      ios_ih
      (Map.empty (module Int))
  in
  match getRDs sid with
  | Some data -> Some (f data)
  | None -> None
;;

(* ========================================================================== *)

(** Loop information summary. *)
type loop_info =
  { lfd : fundec
  ; lid : int
  ; ldefs : Usedef.VS.t
  ; lused : Usedef.VS.t
  ; lrds :
      ( int
      , (int, Reachingdefs.rhs option, Int.comparator_witness) Map.t
      , Int.comparator_witness )
      Map.t
      option
  ; lstmt : stmt (*  *)
  }

let dump_rds vars rds =
  Map.iteri rds ~f:(fun ~key:vid ~data:defs ->
      (match Usedef.VS.(max_elt_opt (filter (fun vi -> vi.vid = vid) vars)) with
      | Some vi -> Fmt.(pf stdout "Variable %s@." vi.vname)
      | None -> Fmt.(pf stdout "Variable id %i@." vid));
      Map.iter defs ~f:(fun def_rhs ->
          match def_rhs with
          | Some (Reachingdefs.RDExp exp) -> Errormsg.log "%a\n" d_exp exp
          | Some (Reachingdefs.RDCall call) -> Errormsg.log "%a\n" d_instr call
          | None -> ()))
;;

let dump_loop_info (li : loop_info) =
  let vars = Usedef.VS.union li.lused li.ldefs in
  Fmt.(pf stdout "Loop %i in %s.@." li.lid li.lfd.svar.vname);
  Fmt.(pf stdout "Reaching defs:@.");
  let _ = Option.map ~f:(dump_rds vars) li.lrds in
  Fmt.(pf stdout "Loop:@.");
  Errormsg.log "%a" d_stmt li.lstmt
;;

let collect_loops fd =
  let loop_table = Hashtbl.create (module Int) in
  let loop_collector_instance = new loopCollector loop_table in
  (* Compute reaching definitions. *)
  Reachingdefs.computeRDs fd;
  (* Compute dominators. *)
  let _ = visitCilFunction loop_collector_instance fd in
  let stmt_hash, _dom_tree = Dominators.computeDomTree ~doCFG:false fd in
  let loops = Hashtbl.to_alist loop_table in
  List.map loops ~f:(fun (sid, loop_stmt) ->
      let used, defd = Usedef.computeDeepUseDefStmtKind loop_stmt.skind in
      let dom_query x = Dominators.dominates stmt_hash x loop_stmt in
      { lfd = fd
      ; lid = sid
      ; ldefs = defd
      ; lused = used
      ; lrds = get_stmt_rds fd dom_query sid
      ; lstmt = loop_stmt
      })
;;

let collect_in_global (g : global) =
  match g with
  | GFun (fd, _) -> Some (collect_loops fd)
  | _ -> None
;;

let functionalize_c (filename : string) =
  Log.debug_msg Fmt.(str "Opening %s" filename);
  initCIL ();
  let cfile : file = Frontc.parse filename () in
  Cfg.computeFileCFG cfile;
  Log.debug_msg Fmt.(str "Parsed %s!" cfile.fileName);
  let all_loops_with_fundecs = List.filter_map cfile.globals ~f:collect_in_global in
  List.iter all_loops_with_fundecs ~f:(fun loops ->
      List.iter loops ~f:(fun loop -> dump_loop_info loop));
  Some ("target", "spec", "repr"), Map.empty (module String)
;;
