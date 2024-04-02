open Bwd
open Util
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

type checked = string

let string_of_checked (fld : checked) : string = fld
let strings_of_checked (fld : checked) : string * string list = (fld, [])

(* Note that raw_or_index can be coerced into this type. *)
type any = [ `Raw of raw | `Checked of checked | `Index of int ]

let string_of_any : any -> string = function
  | `Raw fld -> string_of_raw fld
  | `Checked fld -> string_of_checked fld
  | `Index i -> string_of_int i

(* Check that a raw field can appear in a codata declaration, hence that its pbij can have by=0 (textually, that there are no numbers in it).  Currently we also require ax=0, since we don't have higher fields. *)
let check_zero : raw -> checked option =
 fun fld -> if Pbij_strings.is_empty fld.pbij then Some fld.name else None

(* Check that a raw field matches a checked field. *)
let checks_to : raw -> checked -> bool =
 fun rfld cfld -> Pbij_strings.is_empty rfld.pbij && rfld.name = cfld

let find (fields : (checked, 'a) Abwd.t) (fld : any) : (checked * 'a) option =
  match fld with
  | `Checked fld -> Bwd.find_opt (fun (cfld, _) -> fld = cfld) fields
  | `Index n -> Mbwd.fwd_nth_opt fields n
  | `Raw fld -> Bwd.find_opt (fun (cfld, _) -> checks_to fld cfld) fields
