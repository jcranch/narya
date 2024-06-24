(* This module should not be opened, but used qualified. *)

open Util
open Tbwd
open Reporter
open Syntax
open Term
open Status

(* The global environment of constants and definition-local metavariables. *)

(* Each global constant either is an axiom or has a definition (a case tree).  The latter includes canonical types.  An axiom can be either parametric, which means it is always accessible, or nonparametric, which means it is not accessible behind context locks for external parametricity.  (In the future, this should be customizable on a per-direction basis.) *)
type definition = Axiom of [ `Parametric | `Nonparametric ] | Defined of (emp, potential) term

module Metamap = Meta.Map.Make (struct
  type ('x, 'bs) t = ('x, 'bs) Metadef.t
end)

type data = {
  constants : ((emp, kinetic) term * definition, Code.t) Result.t Constant.Map.t;
  metas : Metadef.def Metamap.t;
  locked : bool;
}

let empty : data = { constants = Constant.Map.empty; metas = Metamap.empty; locked = false }

module S = Algaeff.State.Make (struct
  type t = data
end)

let () =
  S.register_printer (function
    | `Get -> Some "unhandled Global get effect"
    | `Set _ -> Some "unhandled Global set effect")

(* We also store some data that are relevant only to the current command, such as the number of holes that have been created. *)

type current_data = { holes : int }

module Current = Algaeff.State.Make (struct
  type t = current_data
end)

let () =
  Current.register_printer (function
    | `Get -> Some "unhandled Global.Current get effect"
    | `Set _ -> Some "unhandled Global.Current set effect")

let find c =
  let d = S.get () in
  match (Constant.Map.find_opt c d.constants, d.locked) with
  | Some (Ok (_, Axiom `Nonparametric)), true -> fatal (Locked_axiom (PConstant c))
  | Some (Ok (ty, tm)), _ -> (ty, tm)
  | Some (Error e), _ -> fatal e
  | None, _ -> fatal (Undefined_constant (PConstant c))

(* We need to make some calls to Eternity, which isn't defined until lib/parser, so we supply a ref here for Eternity to insert its callbacks. *)
type eternity = {
  find_opt : 'b 's. ('b, 's) Meta.t -> ('b, 's) Metadef.wrapped option;
  add :
    'a 'b 's.
    ('b, 's) Meta.t ->
    (string option, 'a) Bwv.t ->
    ('a, 'b) Termctx.t ->
    ('b, kinetic) term ->
    ('b, 's) status ->
    unit;
}

let eternity : eternity ref =
  ref
    {
      find_opt = (fun _ -> raise (Failure "eternity not set"));
      add = (fun _ _ _ _ _ -> raise (Failure "eternity not set"));
    }

(* When looking up a metavariable, we check both Eternity and the Globals. *)
let find_meta m =
  match !eternity.find_opt m with
  | Some d -> d
  | None -> (
      match Metamap.find_opt (MetaKey m) (S.get ()).metas with
      | Some d -> Metadef.Wrap d
      | None -> fatal (Anomaly "undefined metavariable"))

(* Marshal and unmarshal the data from a single compilation unit. *)

let to_channel_unit chan i flags =
  let d = S.get () in
  Constant.Map.to_channel_unit chan i d.constants flags;
  Metamap.to_channel_unit chan i d.metas flags

let from_channel_unit f chan i =
  let d = S.get () in
  let constants =
    Constant.Map.from_channel_unit chan
      (Result.map (fun (tm, df) -> (Link.term f tm, df)))
      i d.constants in
  let metas = Metamap.from_channel_unit chan { map = (fun df -> Link.metadef f df) } i d.metas in
  S.set { d with constants; metas }

let locked () = (S.get ()).locked

(* Add a new constant, or a new definition to an old one. *)
let add c ty df =
  S.modify @@ fun d -> { d with constants = d.constants |> Constant.Map.add c (Ok (ty, df)) }

(* Add a new constant, but make it an error to access it. *)
let add_error c e =
  S.modify @@ fun d -> { d with constants = d.constants |> Constant.Map.add c (Error e) }

(* Add a new Global metavariable (e.g. local let-definition). *)
let add_meta m ~termctx ~ty ~tm ~energy =
  S.modify @@ fun d ->
  {
    d with
    metas =
      d.metas |> Metamap.add (MetaKey m) (Metadef { data = Def_meta { tm; energy }; termctx; ty });
  }

(* Add a new hole.  This is an eternal metavariable, so we pass off to Eternity, and increment our counter of the number of holes generated by the current command. *)
let add_hole m ~vars ~termctx ~ty ~status =
  !eternity.add m vars termctx ty status;
  Current.modify (fun { holes } -> { holes = holes + 1 })

(* Now some functions that temporarily set the value of a definition, a global metavariable, or the locked state.  Note that these do a new S.run, but not a new Current.run, since they are still within the same command. *)

let with_definition c df f =
  let d = S.get () in
  S.run
    ~init:
      {
        d with
        constants =
          d.constants |> Constant.Map.update c (Option.map (Result.map (fun (ty, _) -> (ty, df))));
      }
    f

let with_meta_definition m tm f =
  let d = S.get () in
  S.run
    ~init:{ d with metas = d.metas |> Metamap.update (MetaKey m) (Option.map (Metadef.define tm)) }
    f

let with_locked f =
  let d = S.get () in
  S.run ~init:{ d with locked = true } f

(* Get the entire global state, for saving with eternal metavariables. *)
let get = S.get

(* Unlike the 'with' functions, the 'run' functions also start a new Current state. *)
let run ~init f = S.run ~init @@ fun () -> Current.run ~init:{ holes = 0 } f
let run_empty f = run ~init:empty f

(* At the end of a command, we get the number of holes that were created by that command to display it to the user, and reset the counter to 0 for the next command. *)
let end_command () =
  let data = Current.get () in
  let current = data.holes in
  Current.set { holes = 0 };
  current
