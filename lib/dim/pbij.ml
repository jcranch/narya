open Util
open Deg

(* Partial bijections. *)

(* An element of ('a, 'ax, 'by, 'b) pbij is a partial bijection from 'ax to 'by, where 'a is the subset of 'ax that is omitted, and 'b is the subset of 'by that is omitted. *)
type (_, _, _, _) pbij =
  | Zero : 'a D.t -> ('a, 'a, D.zero, D.zero) pbij
  | Suc : ('a, 'ax, 'by, 'b) pbij * 'ax D.suc D.index -> ('a, 'ax D.suc, 'by D.suc, 'b) pbij
  | Skip : ('a, 'ax, 'by, 'b) pbij -> ('a, 'ax, 'by D.suc, 'b D.suc) pbij

let zero : type a. a D.t -> (a, a, D.zero, D.zero) pbij = fun a -> Zero a

let rec cozero : type b. b D.t -> (D.zero, D.zero, b, b) pbij = function
  | Nat Zero -> Zero D.zero
  | Nat (Suc b) -> Skip (cozero (Nat b))

type (_, _) any_pbij = Any : ('a, 'ax, 'by, 'b) pbij -> ('ax, 'by) any_pbij

let rec unused_pbij : type x kx ky y. (x, kx, ky, y) pbij -> x D.t = function
  | Zero a -> a
  | Suc (p, _) -> unused_pbij p
  | Skip p -> unused_pbij p

let rec intrinsic_pbij : type x kx ky y. (x, kx, ky, y) pbij -> kx D.t = function
  | Zero a -> a
  | Suc (p, _) -> D.suc (intrinsic_pbij p)
  | Skip p -> intrinsic_pbij p

let rec ambient_pbij : type x kx ky y. (x, kx, ky, y) pbij -> ky D.t = function
  | Zero _ -> D.zero
  | Suc (p, _) -> D.suc (ambient_pbij p)
  | Skip p -> D.suc (ambient_pbij p)

let rec remaining_pbij : type x kx ky y. (x, kx, ky, y) pbij -> y D.t = function
  | Zero _ -> D.zero
  | Suc (p, _) -> remaining_pbij p
  | Skip p -> D.suc (remaining_pbij p)

(* List all the partial bijections from ax to by. *)
let rec pbijs : type ax by. ax D.t -> by D.t -> (ax, by) any_pbij list =
 fun ax by ->
  match (ax, by) with
  | _, Nat Zero -> [ Any (Zero ax) ]
  | Nat Zero, _ -> [ Any (cozero by) ]
  | Nat (Suc ax'), Nat (Suc by) ->
      let skips = pbijs ax (Nat by) in
      let sucs = pbijs (Nat ax') (Nat by) in
      List.map (fun (Any p) -> Any (Skip p)) skips
      @ List.flatten
          (List.map (fun (Any p) -> List.map (fun i -> Any (Suc (p, i))) (D.indices ax)) sucs)

(* A partial bijection is represented by a list (here a Bwv) of positive integers and strings, corresponding to the generating dimensions in 'ax.  Each integer specifies a generator to correspond to in 'by, the strings represent elements of 'ax that don't appear, while the elements of 'by that don't appear are unlisted. *)
module Pbij_strings = struct
  type t = [ `Int of int | `Deg ] list

  let empty : t = []
  let is_empty (xs : t) : bool = xs = []
  let compare : t -> t -> int = fun x y -> compare x y

  (* Unlike for degeneracies, the list representation has to be visible outside Dim, since it is the result of parsing, while we can't make it into a partial bijection until typechecking time since that's when we know 'by.  This operation only fails if there are invalid direction names.  In addition, if we were to allow concatenated representations like "123", there would be technical ambiguity (though, I think, almost never in practice) since that *could* be just a single number 123 with all the other directions missing.  Thus, we require the user to always separate them, and by periods in the field name, so the input to this function is already a list.  *)
  let of_strings : string list -> t option =
   fun strs ->
    let open Mlist.Monadic (Monad.Maybe) in
    mmapM
      (fun [ x ] ->
        match int_of_string_opt x with
        | Some i -> Some (`Int i)
        | None -> if x = Endpoints.refl_string () then Some `Deg else None)
      [ strs ]

  let to_strings : t -> string list =
    List.map (function
      | `Int i -> string_of_int i
      | `Deg -> Endpoints.refl_string ())
end

let pbij_of_strings : type ax by. Pbij_strings.t -> ax D.t -> by D.t -> (ax, by) any_pbij option =
 fun xs ax by ->
  let rec go : type ax by. ([ `Int of int | `Deg ], ax) Bwv.t -> by D.t -> (ax, by) any_pbij option
      =
   fun xs by ->
    (* If 'by is 0, then all the remaining generating dimensions must be degeneracies, and the partial bijection is a Zero. *)
    match by with
    | Nat Zero ->
        if Bwv.fold_right (fun x b -> x = `Deg && b) xs true then Some (Any (Zero (Bwv.length xs)))
        else None
    | Nat (Suc by') -> (
        (* Otherwise, if 'ax is 0, the partial bijection consists of skips, i.e. it is a cozero. *)
        match xs with
        | Emp -> Some (Any (cozero by))
        | Snoc _ -> (
            (* If both 'ax and 'by are positive, we find where the last generating dimension of 'by occurs in the list and remove it, remembering its index to supply to Suc. *)
            match Bwv.find_remove (`Int (N.to_int by)) xs with
            | Some (xs, j) -> (
                (* Then what's left we can recurse into with a smaller expected codomain. *)
                match go xs (Nat by') with
                | None -> None
                | Some (Any s) -> Some (Any (Suc (s, j))))
            (* If it's missing, we skip that element of 'by. *)
            | None -> (
                match go xs (Nat by') with
                | None -> None
                | Some (Any s) -> Some (Any (Skip s))))) in
  match Bwv.of_list ax xs with
  | Some (xs, []) -> go xs by
  | _ -> None

let strings_of_pbij : type a ax by b. (a, ax, by, b) pbij -> Pbij_strings.t = fun _ -> Sorry.e ()

(* By "residual" of a partial bijection, given an element of its 'ambient, we mean the image of that element in its 'intrinsic together with the partial bijection obtained by removing those two elements, or if that element has no image then just the partial bijection obtained by removing it from the 'ambient together with the corresponding index in the 'remaining. *)

type (_, _, _) pbij_residual =
  | Residual :
      ('x, 'kx, 'ky, 'y) pbij * 'kx D.suc D.index
      -> ('kx D.suc, 'ky D.suc, 'y) pbij_residual
  | Nonresidual :
      ('x, 'kx, 'ky, 'y) pbij * 'y D.suc D.index
      -> ('kx, 'ky D.suc, 'y D.suc) pbij_residual

let rec pbij_residual :
    type x kx ky y. (x, kx, ky, y) pbij -> ky D.index -> (kx, ky, y) pbij_residual =
 fun s k ->
  match (k, s) with
  | Top, Suc (s, i) -> Residual (s, i)
  | Pop k, Suc (s, i) -> (
      match pbij_residual s k with
      | Residual (s, j) ->
          let i, j = D.swap_indices i j in
          Residual (Suc (s, j), i)
      | Nonresidual (s, j) -> Nonresidual (Suc (s, i), j))
  | Top, Skip s -> Nonresidual (s, Top)
  | Pop k, Skip s -> (
      match pbij_residual s k with
      | Residual (s, j) -> Residual (Skip s, j)
      | Nonresidual (s, j) -> Nonresidual (Skip s, Pop j))

type (_, _, _) comp_deg_pbij =
  | Comp_deg_pbij : ('x, 'kx, 'kz, 'z) pbij * ('y, 'z) deg -> ('kx, 'kz, 'y) comp_deg_pbij

let rec comp_deg_pbij :
    type x kx ky y n. (ky, n) deg -> (x, kx, ky, y) pbij -> (kx, n, y) comp_deg_pbij =
 fun s p ->
  match s with
  | Zero _ -> Comp_deg_pbij (Zero (intrinsic_pbij p), Zero (remaining_pbij p))
  | Suc (s, k) -> (
      match pbij_residual p k with
      | Residual (q, i) ->
          let (Comp_deg_pbij (r, t)) = comp_deg_pbij s q in
          Comp_deg_pbij (Suc (r, i), t)
      | Nonresidual (q, i) ->
          let (Comp_deg_pbij (r, t)) = comp_deg_pbij s q in
          Comp_deg_pbij (Skip r, Suc (t, i)))

type (_, _, _) comp_pbij_deg =
  | Comp_pbij_deg : ('x, 'kx, 'ky, 'y) pbij -> ('x, 'kx, 'ky) comp_pbij_deg

let comp_pbij_deg : type x kx ky y m. (x, kx, ky, y) pbij -> (m, ky) deg -> (x, kx, m) comp_pbij_deg
    =
 fun _p _s -> Sorry.e ()

(* Split a partial bijection along a decomposition of the ambient as a sum. *)
type (_, _, _, _, _) pbij_of_plus =
  | Pbij_of_plus :
      ('unused, 'i, 'm, 'mrem) pbij
      * ('i, 'intrinsic, 'k, 'krem) pbij
      * ('mrem, 'krem, 'remaining) D.plus
      -> ('unused, 'intrinsic, 'm, 'k, 'remaining) pbij_of_plus

let rec pbij_of_plus :
    type unused intrinsic m k mk remaining.
    (m, k, mk) D.plus ->
    (unused, intrinsic, mk, remaining) pbij ->
    (unused, intrinsic, m, k, remaining) pbij_of_plus =
 fun mk p ->
  match (mk, p) with
  | Zero, _ -> Pbij_of_plus (p, Zero (intrinsic_pbij p), Zero)
  | Suc mk, Suc (p, i) ->
      let (Pbij_of_plus (r, q, x)) = pbij_of_plus mk p in
      Pbij_of_plus (r, Suc (q, i), x)
  | Suc mk, Skip p ->
      let (Pbij_of_plus (r, q, x)) = pbij_of_plus mk p in
      Pbij_of_plus (r, Skip q, Suc x)
