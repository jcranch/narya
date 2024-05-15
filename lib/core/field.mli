open Dim
open Util
open Energy

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

type t

val to_string : t -> string

type _ base =
  | Lower_base : t -> 's base
  | Higher_base : { name : t; intrinsic : 'n D.pos } -> potential base

type ('unused, 'intrinsic, 'ambient, 'remaining) checked = {
  name : t;
  pbij : ('unused, 'intrinsic, 'ambient, 'remaining) pbij;
}

val make_checked :
  t ->
  ('unused, 'intrinsic, 'ambient, 'remaining) pbij ->
  ('unused, 'intrinsic, 'ambient, 'remaining) checked

val equal :
  ('x1, 'kx1, 'ky1, 'y1) checked -> ('x2, 'kx2, 'ky2, 'y2) checked -> ('x1, 'x2) Eq.compare

val is_equal : ('x1, 'kx1, 'ky1, 'y1) checked -> ('x2, 'kx2, 'ky2, 'y2) checked -> bool
val strings_of_checked : ('a, 'ax, 'by, 'b) checked -> string * string list
val string_of_checked : ('a, 'ax, 'by, 'b) checked -> string

type any = Raw : raw -> any | Checked : (D.zero, 'ax, 'by, 'b) checked -> any | Index : int -> any

val any_of_raw_ori : raw_or_index -> any
val string_of_any : any -> string

type wrap_checked = Wrap : (D.zero, 'kx, 'ky, 'y) checked -> wrap_checked
type check_zero = Check_zero : ('a, 'ax, 'by, 'b) checked -> check_zero | Uncheck

val check_zero : raw -> check_zero
val checks_to : raw -> ('a, 'ax, 'by, 'b) checked -> ('a, D.zero) Eq.compare
