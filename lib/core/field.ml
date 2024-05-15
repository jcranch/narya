open Dim
open Util
open Energy

module Raw = struct
  type t = { name : string; pbij : Pbij_strings.t }

  let compare (x : t) (y : t) =
    let c = String.compare x.name y.name in
    if c = 0 then Pbij_strings.compare x.pbij y.pbij else c
end

type raw = Raw.t

let intern (name : string) (pbij : Pbij_strings.t) : raw = { name; pbij }

module RawSet = Set.Make (Raw)

let string_of_raw (x : raw) : string = String.concat "." (x.name :: Pbij_strings.to_strings x.pbij)

type raw_or_index = [ `Raw of raw | `Index of int ]

let string_of_raw_ori (x : raw_or_index) : string =
  match x with
  | `Raw x -> string_of_raw x
  | `Index n -> string_of_int n

let intern_ori (str : string) (pbij : Pbij_strings.t) : raw_or_index option =
  match int_of_string_opt str with
  | Some n -> if Pbij_strings.is_empty pbij then Some (`Index n) else None
  | None -> Some (`Raw (intern str pbij))

type t = string

let to_string x = x

type _ base =
  | Lower_base : t -> 's base
  | Higher_base : { name : t; intrinsic : 'n D.pos } -> potential base

(* 'intrinsic = intrinsic dimension of the field
   'unused = the unused part of 'intrinsic, still needing to be taken up by a substitution
   'ambient = the ambient dimension to which we've been substituted
   'remaining = the dimension remaining of that substitution after taking up some of 'intrinsic
   thus 'intrinsic - 'unused = 'ambient - 'remaining are the dimensions related bijectively.
*)
type ('unused, 'intrinsic, 'ambient, 'remaining) checked = {
  name : t;
  pbij : ('unused, 'intrinsic, 'ambient, 'remaining) pbij;
}

let make_checked :
    type unused intrinsic ambient remaining.
    t ->
    (unused, intrinsic, ambient, remaining) pbij ->
    (unused, intrinsic, ambient, remaining) checked =
 fun name pbij -> { name; pbij }

let equal :
    type x1 kx1 ky1 y1 x2 kx2 ky2 y2.
    (x1, kx1, ky1, y1) checked -> (x2, kx2, ky2, y2) checked -> (x1, x2) Eq.compare =
 fun _ _ -> Util.Sorry.e ()

let is_equal :
    type x1 kx1 ky1 y1 x2 kx2 ky2 y2.
    (x1, kx1, ky1, y1) checked -> (x2, kx2, ky2, y2) checked -> bool =
 fun x y ->
  match equal x y with
  | Eq -> true
  | Neq -> false

let strings_of_checked (fld : ('a, 'ax, 'by, 'b) checked) : string * string list =
  (fld.name, Pbij_strings.to_strings (strings_of_pbij fld.pbij))

let string_of_checked (fld : ('a, 'ax, 'by, 'b) checked) : string =
  let name, pbij = strings_of_checked fld in
  String.concat "." (name :: pbij)

type any = Raw : raw -> any | Checked : (D.zero, 'ax, 'by, 'b) checked -> any | Index : int -> any

let any_of_raw_ori : raw_or_index -> any = function
  | `Raw fld -> Raw fld
  | `Index n -> Index n

let string_of_any : any -> string = function
  | Raw fld -> string_of_raw fld
  | Checked fld -> string_of_checked fld
  | Index i -> string_of_int i

type wrap_checked = Wrap : (D.zero, 'kx, 'ky, 'y) checked -> wrap_checked

(* Check that a raw field can appear in a codata declaration, hence that its pbij can have by=0 (textually, that there are no numbers in it).  Currently we also require ax=0, since we don't have higher fields.  TODO. *)
type check_zero = Check_zero : ('a, 'ax, 'by, 'b) checked -> check_zero | Uncheck

let check_zero : raw -> check_zero =
 fun fld ->
  if Pbij_strings.is_empty fld.pbij then Check_zero { name = fld.name; pbij = zero D.zero }
  else Uncheck

(* Check that a raw field matches a checked field. TODO *)
let checks_to : raw -> ('a, 'ax, 'by, 'b) checked -> ('a, D.zero) Eq.compare =
 fun _rfld _cfld -> Util.Sorry.e () (* Pbij_strings.is_empty rfld.pbij && rfld.name = cfld.name *)
