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

type ('a, 'ax, 'by, 'b) checked

val strings_of_checked : ('a, 'ax, 'by, 'b) checked -> string * string list
val string_of_checked : ('a, 'ax, 'by, 'b) checked -> string

type any = Raw : raw -> any | Checked : ('a, 'ax, 'by, 'b) checked -> any | Index : int -> any

val any_of_raw_ori : raw_or_index -> any
val string_of_any : any -> string

type check_zero = Check_zero : ('a, 'ax, 'by, 'b) checked -> check_zero | Uncheck

val check_zero : raw -> check_zero
val checks_to : raw -> ('a, 'ax, 'by, 'b) checked -> bool
