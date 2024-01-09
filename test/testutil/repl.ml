open Util
open Dim
open Core
open Readback
open Reporter
open Parser
open Unparse
open Print
open Norm
open Check
open Syntax
open Term
open Value
open Inst
open Raw
open Hctx
open Parse
open Asai.Range

let parse_term (tm : string) : N.zero check located =
  let p = Parse_term.parse (`String { content = tm; title = Some "user-supplied term" }) in
  let (Term tm) = Parse_term.final p in
  Postprocess.process Emp tm

module Terminal = Asai.Tty.Make (Core.Reporter.Code)

let check_type (rty : N.zero check located) : emp term =
  Reporter.trace "when checking type" @@ fun () -> check Ctx.empty rty (universe D.zero)

let check_term (rtm : N.zero check located) (ety : value) : emp term =
  Reporter.trace "when checking term" @@ fun () -> check Ctx.empty rtm ety

let assume (name : string) (ty : string) : unit =
  let p = Parse_term.parse (`String { title = Some "constant name"; content = name }) in
  match Parse_term.final p with
  | Term { value = Ident (name, _); _ } ->
      if Option.is_some (Scope.lookup name) then
        emit (Constant_already_defined (String.concat "." name));
      let const = Scope.define name in
      let rty = parse_term ty in
      let cty = check_type rty in
      Hashtbl.add Global.types const cty;
      Hashtbl.add Global.constants const Axiom
  | _ -> fatal (Invalid_constant_name name)

let def (name : string) (ty : string) (tm : string) : unit =
  let p = Parse_term.parse (`String { title = Some "constant name"; content = name }) in
  match Parse_term.final p with
  | Term { value = Ident (name, _); _ } ->
      Reporter.tracef "when defining %s" (String.concat "." name) @@ fun () ->
      if Option.is_some (Scope.lookup name) then
        emit (Constant_already_defined (String.concat "." name));
      let const = Scope.define name in

      let rty = parse_term ty in
      let rtm = parse_term tm in
      let cty = check_type rty in
      let ety = eval (Emp D.zero) cty in
      Hashtbl.add Global.types const cty;
      let tree = ref Case.Empty in
      Hashtbl.add Global.constants const (Defined tree);
      let hd = eval (Emp D.zero) (Const const) in
      Reporter.try_with ~fatal:(fun d ->
          Hashtbl.remove Global.types const;
          Hashtbl.remove Global.constants const;
          Reporter.fatal_diagnostic d)
      @@ fun () ->
      Reporter.trace "when checking case tree" @@ fun () -> check_tree Ctx.empty rtm ety hd tree
  | _ -> fatal (Invalid_constant_name name)

let undef (name : string) : unit =
  match Scope.lookup [ name ] with
  | Some const ->
      Hashtbl.remove Global.types const;
      Hashtbl.remove Global.constants const
  | None -> raise (Failure ("Can't undefine undefined constant " ^ name))

let equal_at (tm1 : string) (tm2 : string) (ty : string) : unit =
  let rty = parse_term ty in
  let rtm1 = parse_term tm1 in
  let rtm2 = parse_term tm2 in
  let cty = check_type rty in
  let ety = eval (Emp D.zero) cty in
  let ctm1 = check_term rtm1 ety in
  let ctm2 = check_term rtm2 ety in
  let etm1 = eval (Emp D.zero) ctm1 in
  let etm2 = eval (Emp D.zero) ctm2 in
  match Equal.equal_at 0 etm1 etm2 ety with
  | None -> raise (Failure "Unequal terms")
  | Some () -> ()

let unequal_at (tm1 : string) (tm2 : string) (ty : string) : unit =
  let rty = parse_term ty in
  let rtm1 = parse_term tm1 in
  let rtm2 = parse_term tm2 in
  let cty = check_type rty in
  let ety = eval (Emp D.zero) cty in
  let ctm1 = check_term rtm1 ety in
  let ctm2 = check_term rtm2 ety in
  let etm1 = eval (Emp D.zero) ctm1 in
  let etm2 = eval (Emp D.zero) ctm2 in
  match Equal.equal_at 0 etm1 etm2 ety with
  | None -> ()
  | Some () -> raise (Failure "Equal terms")

let print (tm : string) : unit =
  let rtm = parse_term tm in
  match rtm with
  | { value = Synth rtm; loc } ->
      let ctm, ety = synth Ctx.empty { value = rtm; loc } in
      let etm = eval (Emp D.zero) ctm in
      let btm = readback_at Ctx.empty etm ety in
      let utm = unparse Names.empty btm Interval.entire Interval.entire in
      pp_term `None Format.std_formatter (Term utm);
      Format.pp_print_newline Format.std_formatter ()
  | _ -> fatal (Nonsynthesizing "argument of print")

let rec run f =
  Reporter.run ~emit:Terminal.display ~fatal:(fun d ->
      run @@ fun () ->
      Terminal.display d;
      raise (Failure "Fatal error"))
  @@ fun () ->
  Printconfig.run ~env:{ style = `Compact; state = `Term; chars = `Unicode } @@ fun () ->
  Builtins.run @@ fun () -> Scope.run f
