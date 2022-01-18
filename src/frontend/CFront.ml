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

let extract_cond_nobreak (if_stmt : stmt) : exp option =
  match if_stmt.skind with
  | If (cond, true_block, false_block, _) ->
    (match true_block.bstmts, false_block.bstmts with
    | [ { skind = Break _; _ } ], [] -> Some (UnOp (BNot, cond, Cil.intType))
    | [], [ { skind = Break _; _ } ] -> Some cond
    | _ -> None)
  | _ -> None
;;

let rec stmt_increments (stmt : stmt) (v : varinfo) =
  match stmt.skind with
  | Instr instrs ->
    List.fold_until
      instrs
      ~init:0
      ~finish:(fun c -> Some c)
      ~f:
        (fun accum -> function
          | Call _ -> Stop None
          | Set (lv, exp, _) ->
            (match lv with
            | Var v', NoOffset when v'.vid = v.vid ->
              (match exp with
              | BinOp (PlusA, Lval (Var v', NoOffset), Const (CInt64 (1L, _, _)), _)
                when v'.vid = v.vid -> Continue (accum + 1)
              | _ -> Stop None)
            | _ -> Continue accum)
          | VarDecl (v', _) -> if v'.vid = v.vid then Stop None else Continue accum
          | Asm _ -> Continue accum)
  | Block b ->
    let l = List.map ~f:(fun s -> stmt_increments s v) b.bstmts in
    if List.fold ~f:( + ) (List.filter_opt l) ~init:0 = 1 then Some 1 else None
  | _ -> None
;;

let is_only_incremented_in (l : stmt list) (v : varinfo) =
  let b =
    List.fold_until
      l
      ~init:true
      ~finish:(fun _ -> true)
      ~f:(fun _ stmt ->
        let _, defs = Usedef.computeUseDefStmtKind stmt.skind in
        if not (Usedef.VS.mem v defs)
        then Continue true
        else (
          let incrs =
            match stmt_increments stmt v with
            | Some _ -> true
            | None -> false
          in
          (Errormsg.log "%a\n" d_stmt stmt;
           Fmt.(pf stdout "%b@." incrs));
          if incrs then Continue true else Stop false))
  in
  Fmt.(pf stdout "%s only incr? %b@." v.vname b);
  b
;;

let vars_of_cond (tl : stmt list) (cond : exp) =
  Errormsg.log "%a\n" d_exp cond;
  let cond_uses = Usedef.computeUseExp cond in
  Option.(
    Usedef.VS.(max_elt_opt (filter (is_only_incremented_in tl) cond_uses))
    >>| fun elt -> cond, elt)
;;

let extract_index (loop : stmt) =
  match loop.skind with
  | Loop (b, _, _, _) ->
    (match b.bstmts with
    | maybe_if :: stmts ->
      Option.(
        extract_cond_nobreak maybe_if
        >>= vars_of_cond b.bstmts
        >>| fun (cond, index) -> cond, index, stmts)
    | _ -> None)
  | _ -> None
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
  ; lstmt : stmt (* Info about iterator and guard *)
  ; lbody : stmt list
  ; lindex : varinfo
  ; lcond : exp
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
  Fmt.(pf stdout "Index var: %s@." li.lindex.vname);
  Fmt.(pf stdout "Reaching defs:@.");
  let _ = Option.map ~f:(dump_rds vars) li.lrds in
  Fmt.(pf stdout "Body:@.");
  List.iter ~f:(fun stmt -> Errormsg.log "%a\n" d_stmt stmt) li.lbody
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
      match extract_index loop_stmt with
      | Some (cond, index, stmts) ->
        Some
          { lfd = fd
          ; lid = sid
          ; ldefs = defd
          ; lused = used
          ; lrds = get_stmt_rds fd dom_query sid
          ; lstmt = loop_stmt
          ; lbody = stmts
          ; lindex = index
          ; lcond = cond
          }
      | None -> None)
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
      List.iter loops ~f:(fun loop ->
          let _ = Option.map ~f:dump_loop_info loop in
          ()));
  Some ("target", "spec", "repr"), Map.empty (module String)
;;
