open Dim

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

(* 'ax = intrinsic dimension of the field
   'a = the part of 'ax remaining to be taken up by a substitution
   'by = the dimension to which we've been substituted
   'b = the dimension remaining of that substitution after taking up some of 'ax
   thus 'ax - 'a = 'by - 'b are the dimensions related bijectively.
*)
type ('a, 'ax, 'by, 'b) checked = { name : string; pbij : ('a, 'ax, 'by, 'b) pbij }

let equal :
    type x1 kx1 ky1 y1 x2 kx2 ky2 y2.
    (x1, kx1, ky1, y1) checked -> (x2, kx2, ky2, y2) checked -> bool =
 fun _ _ -> Util.Sorry.e ()

let strings_of_checked (fld : ('a, 'ax, 'by, 'b) checked) : string * string list =
  (fld.name, Pbij_strings.to_strings (strings_of_pbij fld.pbij))

let string_of_checked (fld : ('a, 'ax, 'by, 'b) checked) : string =
  let name, pbij = strings_of_checked fld in
  String.concat "." (name :: pbij)

type any = Raw : raw -> any | Checked : ('a, 'ax, 'by, 'b) checked -> any | Index : int -> any

let any_of_raw_ori : raw_or_index -> any = function
  | `Raw fld -> Raw fld
  | `Index n -> Index n

let string_of_any : any -> string = function
  | Raw fld -> string_of_raw fld
  | Checked fld -> string_of_checked fld
  | Index i -> string_of_int i

(* Check that a raw field can appear in a codata declaration, hence that its pbij can have by=0 (textually, that there are no numbers in it).  Currently we also require ax=0, since we don't have higher fields.  TODO. *)
type check_zero = Check_zero : ('a, 'ax, 'by, 'b) checked -> check_zero | Uncheck

let check_zero : raw -> check_zero =
 fun fld ->
  if Pbij_strings.is_empty fld.pbij then Check_zero { name = fld.name; pbij = zero } else Uncheck

(* Check that a raw field matches a checked field. TODO *)
let checks_to : raw -> ('a, 'ax, 'by, 'b) checked -> bool =
 fun rfld cfld -> Pbij_strings.is_empty rfld.pbij && rfld.name = cfld.name

type (_, _, _) acted = Acted : ('x, 'ky, 'ky, 'y) checked -> ('x, 'kx, 'ky) acted

let act : type m x kx ky y. (x, kx, ky, y) checked -> (m, ky) deg -> (x, kx, m) acted =
 fun fld fa ->
  let (Comp_pbij_deg pbij) = comp_pbij_deg fld.pbij fa in
  Acted { fld with pbij }
