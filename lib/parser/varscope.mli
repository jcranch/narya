open Util
open Core

type 'n t

val empty : N.zero t
val ext : 'n t -> string option -> 'n N.suc t
val ext_fields : 'n t -> string option -> (string, Field.raw) Abwd.t -> 'n N.suc t
val find : string -> 'n t -> [ `Var of 'n N.index | `Field of 'n N.index * Field.raw | `None ]
val top : 'n N.suc t -> 'n t * string option

type (_, _) append_plus =
  | Append_plus :
      ('nm, 'k, 'nmk) Fwn.bplus * ('n, 'mk, 'nmk) Fwn.bplus * (string option, 'mk) Vec.t * 'nm t
      -> ('n, 'k) append_plus

val append_plus : (string option, 'k) Vec.t -> 'n t -> string option list -> ('n, 'k) append_plus
