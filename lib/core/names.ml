open Util
open Tbwd
open Dim
open Syntax
open Term
open Reporter
module StringMap = Map.Make (String)

(* Track the used variable names, to generate fresh ones for bound variables if needed. *)

(* We store a parametrized list like a context, and also a map that counts how many like-named variables already exist, so that we can create a new one with an unused number. *)
type 'b ctx = Emp : emp ctx | Snoc : 'b ctx * 'n variables -> ('b, 'n) snoc ctx
type 'b t = { ctx : 'b ctx; used : int StringMap.t }

let empty : emp t = { ctx = Emp; used = StringMap.empty }

let cubevar x fa : string list =
  let fa = string_of_sface fa in
  if fa = "" then [ x ] else [ x; fa ]

let lookup : type n. n t -> n index -> string list =
 fun { ctx; used = _ } x ->
  let rec lookup : type n. n ctx -> n index -> string list =
   fun ctx x ->
    match (ctx, x) with
    | Emp, _ -> .
    | Snoc (ctx, _), Index (Later x, fa) -> lookup ctx (Index (x, fa))
    | Snoc (_, Variables (_, mn, xs)), Index (Now, fa) -> (
        let (SFace_of_plus (_, fb, fc)) = sface_of_plus mn fa in
        match CubeOf.find xs fc with
        | Some x -> cubevar x fb
        | None -> fatal (Anomaly "reference to anonymous variable")) in
  lookup ctx x

(* Make a variable name unique, adding the new one to the list of used variables and returning it. *)

let uniquify : string option -> int StringMap.t -> string option * int StringMap.t =
 fun name used ->
  match name with
  | None -> (None, used)
  | Some name -> (
      match StringMap.find_opt name used with
      | None -> (Some name, used |> StringMap.add name 0)
      | Some n ->
          (* The tentative new name is the original one suffixed by that number.  But the user might already have created a variable with that name, so we have to increment the number until we find an unused name.  *)
          let rec until_unique k =
            let namek = name ^ string_of_int k in
            match StringMap.find_opt namek used with
            | None -> (namek, k)
            | Some _ -> until_unique (k + 1) in
          let namen, n = until_unique n in
          (Some namen, used |> StringMap.add namen 0 |> StringMap.add name (n + 1)))

(* Do the same thing to a whole cube of variable names. *)
let uniquifies :
    type n.
    (n, string option) CubeOf.t -> int StringMap.t -> (n, string option) CubeOf.t * int StringMap.t
    =
 fun names used ->
  let module M = Monad.State (struct
    type t = int StringMap.t
  end) in
  let open CubeOf.Monadic (M) in
  mmapM { map = (fun _ [ name ] used -> uniquify name used) } [ names ] used

let add_cube : type n b. n D.t -> b t -> string option -> string option * (b, n) snoc t =
 fun n { ctx; used } name ->
  let name, used = uniquify name used in
  (name, { ctx = Snoc (ctx, Variables (n, D.plus_zero n, CubeOf.singleton name)); used })

let add_normals :
    type n b. b t -> (n, string option) CubeOf.t -> (n, string option) CubeOf.t * (b, n) snoc t =
 fun { ctx; used } names ->
  let names, used = uniquifies names used in
  let n = CubeOf.dim names in
  (names, { ctx = Snoc (ctx, Variables (D.zero, D.zero_plus n, names)); used })

let add : 'b t -> 'n variables -> 'n variables * ('b, 'n) snoc t =
 fun { ctx; used } (Variables (m, mn, names)) ->
  let names, used = uniquifies names used in
  let vars = Variables (m, mn, names) in
  (vars, { ctx = Snoc (ctx, vars); used })

let pp_variables : type n. Format.formatter -> n variables -> unit =
 fun ppf (Variables (_, _, x)) ->
  let open Format in
  fprintf ppf "@[<hv 2>(";
  CubeOf.miter
    {
      it =
        (fun fa [ x ] ->
          if Option.is_some (is_id_sface fa) then pp_print_string ppf (Option.value x ~default:"_")
          else fprintf ppf "%s,@ " (Option.value x ~default:"_"));
    }
    [ x ];
  fprintf ppf ")@]"

let pp_names : type b. Format.formatter -> b t -> unit =
 fun ppf vars ->
  let open Format in
  let rec pp : type b. bool -> formatter -> b ctx -> unit =
   fun comma ppf vars ->
    match vars with
    | Emp -> ()
    | Snoc (vars, x) ->
        fprintf ppf "%a%a" (pp true) vars pp_variables x;
        if comma then fprintf ppf ",@ " in
  fprintf ppf "@[<hv 2>(%a)@]" (pp false) vars.ctx
