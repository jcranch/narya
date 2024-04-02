open Util
open Dim
open Core
open Syntax
open Value
open Parser
open Asai.Range

let unlocated (value : 'a) : 'a located = { value; loc = None }

(* Poor man's parser, reusing the OCaml parser to make a vaguely usable syntax *)

(* Abstract syntax terms with variable names *)
type pmt =
  | Var : string -> pmt
  | Const : string -> pmt
  | Field : pmt * string -> pmt
  | UU : pmt
  | Pi : string * pmt * pmt -> pmt
  | App : pmt * pmt -> pmt
  | Deg : pmt * string -> pmt
  | Asc : pmt * pmt -> pmt
  | Lam : string * pmt -> pmt
  | Struct : (string * pmt) list -> pmt
  | Constr : string -> pmt

(* Using a Bwv of variable names, to turn them into De Bruijn indices, we can parse such a term into a synth/checkable one. *)
let rec parse_chk : type n. (string, n) Bwv.t -> pmt -> n Raw.check located =
 fun ctx -> function
  | Lam (x, body) ->
      unlocated (Raw.Lam ({ value = Some x; loc = None }, `Normal, parse_chk (Snoc (ctx, x)) body))
  | Struct tms ->
      unlocated
        (Raw.Struct
           ( Eta,
             List.fold_left
               (fun acc (fld, tm) ->
                 Abwd.add (Some (Field.intern fld Pbij_strings.empty)) (parse_chk ctx tm) acc)
               Abwd.empty tms ))
  | Constr c -> unlocated (Raw.Constr (unlocated (Constr.intern c), Emp))
  | App (fn, arg) as tm -> (
      match (parse_chk ctx fn).value with
      | Constr (c, args) -> unlocated (Raw.Constr (c, Snoc (args, parse_chk ctx arg)))
      | _ -> unlocated (Raw.Synth (parse_syn ctx tm).value))
  | tm -> unlocated (Raw.Synth (parse_syn ctx tm).value)

and parse_syn : type n. (string, n) Bwv.t -> pmt -> n Raw.synth located =
 fun ctx -> function
  | Var x -> (
      match Bwv.find x ctx with
      | Some v -> unlocated (Raw.Var (v, None))
      | None -> Reporter.fatal (Unbound_variable x))
  | Const x -> (
      match Scope.lookup [ x ] with
      | Some c -> unlocated (Raw.Const c)
      | None -> Reporter.fatal (Unbound_variable x))
  | UU -> unlocated Raw.UU
  | Field (x, fld) ->
      unlocated (Raw.Field (parse_syn ctx x, Option.get (Field.intern_ori fld Pbij_strings.empty)))
  | Pi (x, dom, cod) ->
      unlocated (Raw.Pi (Some x, parse_chk ctx dom, parse_chk (Snoc (ctx, x)) cod))
  | App (fn, arg) -> unlocated (Raw.App (parse_syn ctx fn, parse_chk ctx arg))
  | Deg (x, str) -> (
      match (parse_chk ctx x).value with
      | Synth x -> (
          match Dim.deg_of_name str with
          | Some (Any s) -> unlocated (Raw.Act (str, s, unlocated x))
          | None -> (
              match Dim.deg_of_string str with
              | Some (Any s) -> unlocated (Raw.Act (str, s, unlocated x))
              | None -> raise (Failure "unknown degeneracy")))
      | _ -> raise (Failure "Non-synthesizing"))
  | Asc (tm, ty) -> unlocated (Raw.Asc (parse_chk ctx tm, parse_chk ctx ty))
  | _ -> raise (Failure "Non-synthesizing")

(* Nicer syntax, with a prefix operator for using a variable by name, and infix operators for abstraction, application, and ascription. *)
let ( !! ) x = Var x
let ( !~ ) x = Const x
let ( !. ) x = Constr x

(* let pi x dom cod = Pi (x, dom, cod) *)
let ( @=> ) (x, dom) cod = Pi (x, dom, cod)
let ( $ ) fn arg = App (fn, arg) (* Left-associative *)
let id a x y = App (App (Deg (a, "refl"), x), y)
let refl x = Deg (x, "refl")
let sym x = Deg (x, "sym")
let ( <:> ) tm ty = Asc (tm, ty)
let ( @-> ) x body = Lam (x, body) (* Right-associative *)
let ( $. ) x fld = Field (x, fld)
let struc tms = Struct tms
let ( $^ ) x s = Deg (x, s)

module Terminal = Asai.Tty.Make (Core.Reporter.Code)

(* The current context of assumptions, including names. *)
type ctx = Ctx : ('n, 'b) Ctx.t * (string, 'n) Bwv.t -> ctx

let ectx = Ctx (Ctx.empty, Emp)
let context = ref ectx

(* Functions to synth and check terms *)

let synth (tm : pmt) : kinetic value * kinetic value =
  let (Ctx (ctx, names)) = !context in
  Reporter.run ~emit:Terminal.display ~fatal:(fun d ->
      Terminal.display d;
      raise (Failure "Failed to synthesize"))
  @@ fun () ->
  let raw = parse_syn names tm in
  let syn, ty = Check.synth ctx raw in
  let esyn = Ctx.eval_term ctx syn in
  (esyn, ty)

let check (tm : pmt) (ty : kinetic value) : kinetic value =
  let (Ctx (ctx, names)) = !context in
  Reporter.run ~emit:Terminal.display ~fatal:(fun d ->
      Terminal.display d;
      raise (Failure "Failed to check"))
  @@ fun () ->
  let raw = parse_chk names tm in
  let chk = Check.check Kinetic ctx raw ty in
  Ctx.eval_term ctx chk

(* Assert that a term *doesn't* synthesize or check *)

let unsynth (tm : pmt) : unit =
  let (Ctx (ctx, names)) = !context in
  Reporter.run ~emit:Terminal.display ~fatal:(fun _ -> ()) @@ fun () ->
  let raw = parse_syn names tm in
  let _ = Check.synth ctx raw in
  raise (Failure "Synthesis success")

let uncheck (tm : pmt) (ty : kinetic value) : unit =
  let (Ctx (ctx, names)) = !context in
  Reporter.run ~emit:Terminal.display ~fatal:(fun _ -> ()) @@ fun () ->
  let raw = parse_chk names tm in
  let _ = Check.check Kinetic ctx raw ty in
  raise (Failure "Checking success")

(* Add to the context of assumptions *)

let assume (x : string) (ty : kinetic value) : kinetic value =
  let (Ctx (ctx, names)) = !context in
  context := Ctx (Ctx.ext ctx (Some x) ty, Snoc (names, x));
  fst (synth !!x)

(* Check that two terms are, or aren't, equal, at a type or synthesizing *)

let equal_at (tm1 : kinetic value) (tm2 : kinetic value) (ty : kinetic value) : unit =
  let (Ctx (ctx, _)) = !context in
  if Option.is_some (Equal.equal_at (Ctx.length ctx) tm1 tm2 ty) then ()
  else raise (Failure "Unequal terms")

let unequal_at (tm1 : kinetic value) (tm2 : kinetic value) (ty : kinetic value) : unit =
  let (Ctx (ctx, _)) = !context in
  if Option.is_none (Equal.equal_at (Ctx.length ctx) tm1 tm2 ty) then ()
  else raise (Failure "Equal terms")

let equal (tm1 : kinetic value) (tm2 : kinetic value) : unit =
  let (Ctx (ctx, _)) = !context in
  if Option.is_some (Equal.equal_val (Ctx.length ctx) tm1 tm2) then ()
  else raise (Failure "Unequal terms")

let unequal (tm1 : kinetic value) (tm2 : kinetic value) : unit =
  let (Ctx (ctx, _)) = !context in
  if Option.is_none (Equal.equal_val (Ctx.length ctx) tm1 tm2) then ()
  else raise (Failure "Equal terms")

(* Infix notation for applying values *)

let ( $$ ) (fn : kinetic value) (arg : kinetic value) : kinetic value =
  Norm.apply_term fn (Dim.CubeOf.singleton arg)

let run f =
  Reporter.run ~emit:Terminal.display ~fatal:(fun d ->
      Terminal.display d;
      raise (Failure "Fatal error"))
  @@ fun () ->
  Parser.Builtins.run @@ fun () ->
  Global.run_empty @@ fun () -> Scope.run f
