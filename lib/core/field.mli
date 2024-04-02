open Util
open Dim

module Raw : sig
  type t

  val compare : t -> t -> int
end

type raw = Raw.t

val intern : string -> Pbij_strings.t -> raw

module RawSet : module type of Set.Make (Raw)

val string_of_raw : raw -> string

type raw_or_index = [ `Raw of raw | `Index of int ]

val string_of_raw_ori : raw_or_index -> string
val intern_ori : string -> Pbij_strings.t -> raw_or_index option

type checked

val string_of_checked : checked -> string
val strings_of_checked : checked -> string * string list

type any = [ `Raw of raw | `Checked of checked | `Index of int ]

val string_of_any : any -> string
val check_zero : raw -> checked option
val checks_to : raw -> checked -> bool
val find : (checked, 'a) Abwd.t -> any -> (checked * 'a) option
