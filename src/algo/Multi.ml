open AState
open Base
open Domainslib

type 'a answer =
  | Definite of 'a
  | Maybe of 'a
  | Unknown

type 'a task = ThreadContext.t -> 'a answer Chan.t -> unit -> unit

let first_definite (ctx : ThreadContext.t) (tasks : 'a task list) =
  let rec await_first_resp channels =
    let check (chan, sub) =
      match Chan.recv_poll chan with
      | Some (Definite x) ->
        ThreadContext.remove_sub ctx ~sub;
        `Fst x
      | Some (Maybe x) ->
        ThreadContext.remove_sub ctx ~sub;
        `Snd (Maybe x)
      | Some Unknown ->
        ThreadContext.remove_sub ctx ~sub;
        `Snd Unknown
      | None -> `Trd (chan, sub)
    in
    let term_def, term_maybe, running = List.partition3_map ~f:check channels in
    if List.length term_def > 0
    then (
      List.iter ~f:(fun (_, sub) -> ThreadContext.subkill sub) running;
      term_def)
    else (
      match running with
      | [] ->
        List.filter_map
          ~f:(function
            | Maybe x | Definite x -> Some x
            | _ -> None)
          term_maybe
      | _ -> await_first_resp running)
  in
  let start_task i t =
    let sub_ctx = ThreadContext.subctx ctx (Fmt.str "task_%i" i) in
    let channel = Chan.make_unbounded () in
    let _ = Task.async ctx.c_pool (t sub_ctx channel) in
    channel, sub_ctx
  in
  await_first_resp (List.mapi ~f:start_task tasks)
;;
