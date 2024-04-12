open Bwd
open Util
open Tlist
open Tbwd
open Monoid
module D : MonoidPos

val to_int : 'a D.t -> int

type one

val compare : 'm D.t -> 'n D.t -> ('m, 'n) compare

type (_, _) factor = Factor : ('n, 'k, 'nk) D.plus -> ('nk, 'n) factor

val factor : 'nk D.t -> 'n D.t -> ('nk, 'n) factor option
val epi : 'k D.t -> ('k, 'm, 'l) D.plus -> ('k, 'n, 'l) D.plus -> ('m, 'n) Monoid.eq
val zero_uniq : 'm D.t -> ('m, 'z, 'm) D.plus -> ('z, D.zero) Monoid.eq

type (_, _) pushout = Pushout : ('a, 'c, 'p) D.plus * ('b, 'd, 'p) D.plus -> ('a, 'b) pushout

val pushout : 'a D.t -> 'b D.t -> ('a, 'b) pushout

type (_, _) deg

val dom_deg : ('m, 'n) deg -> 'm D.t
val cod_deg : ('m, 'n) deg -> 'n D.t
val id_deg : 'n D.t -> ('n, 'n) deg
val comp_deg : ('b, 'c) deg -> ('a, 'b) deg -> ('a, 'c) deg
val deg_plus : ('m, 'n) deg -> ('n, 'k, 'nk) D.plus -> ('m, 'k, 'mk) D.plus -> ('mk, 'nk) deg
val deg_plus_dom : ('m, 'n) deg -> ('m, 'k, 'mk) D.plus -> ('mk, 'n) deg

val deg_plus_deg :
  ('k, 'm) deg -> ('m, 'n, 'mn) D.plus -> ('k, 'l, 'kl) D.plus -> ('l, 'n) deg -> ('kl, 'mn) deg

val plus_deg :
  'm D.t -> ('m, 'n, 'mn) D.plus -> ('m, 'l, 'ml) D.plus -> ('l, 'n) deg -> ('ml, 'mn) deg

val is_id_deg : ('m, 'n) deg -> unit option
val pos_deg : 'n D.pos -> ('m, 'n) deg -> 'm D.pos
val deg_equiv : ('m, 'n) deg -> ('k, 'l) deg -> unit option
val deg_zero : 'a D.t -> ('a, D.zero) deg

type 'n perm = ('n, 'n) deg

val dim_perm : 'n perm -> 'n D.t
val id_perm : 'n D.t -> 'n perm
val comp_perm : 'a perm -> 'a perm -> 'a perm
val perm_plus : 'm perm -> ('m, 'k, 'mk) D.plus -> 'mk perm
val perm_plus_perm : 'm perm -> ('m, 'n, 'mn) D.plus -> 'n perm -> 'mn perm
val plus_perm : 'm D.t -> ('m, 'n, 'mn) D.plus -> 'n perm -> 'mn perm
val is_id_perm : 'n perm -> unit option
val perm_equiv : 'm perm -> 'n perm -> unit option
val switch_perm : 'm D.t -> ('m, 'n, 'mn) D.plus -> 'mn perm
val perm_inv : 'm perm -> 'm perm

type (_, _, _) deg_perm_of_plus =
  | Deg_perm_of_plus :
      ('m, 'k, 'mk) D.plus * ('m, 'n) deg * 'k perm
      -> ('mk, 'n, 'k) deg_perm_of_plus
  | None_deg_perm_of_plus : ('mk, 'n, 'k) deg_perm_of_plus

val deg_perm_of_plus : ('n, 'k, 'nk) D.plus -> ('mk, 'nk) deg -> ('mk, 'n, 'k) deg_perm_of_plus

type _ deg_of = Of : ('m, 'n) deg -> 'n deg_of
type _ deg_of_plus = Of : ('n, 'k, 'nk) D.plus * ('m, 'nk) deg -> 'n deg_of_plus

val comp_deg_of_plus : ('m, 'n) deg -> 'm deg_of_plus -> 'n deg_of_plus
val reduce_deg_of_plus : 'n deg_of_plus -> 'n deg_of_plus

type (_, _) deg_extending =
  | DegExt : ('k, 'j, 'kj) D.plus * ('n, 'i, 'ni) D.plus * ('kj, 'ni) deg -> ('k, 'n) deg_extending

val comp_deg_extending : ('m, 'n) deg -> ('k, 'l) deg -> ('k, 'n) deg_extending

type any_deg = Any : ('m, 'n) deg -> any_deg

val comp_deg_of_plus_any : 'n deg_of_plus -> any_deg -> 'n deg_of_plus
val any_deg_plus : any_deg -> 'k D.t -> any_deg
val any_of_deg_of_plus : 'n deg_of_plus -> any_deg
val is_id_any_deg : any_deg -> unit option
val string_of_deg : ('a, 'b) deg -> string
val deg_of_string : string -> any_deg option

type (_, _) sface

val id_sface : 'n D.t -> ('n, 'n) sface
val dom_sface : ('m, 'n) sface -> 'm D.t
val cod_sface : ('m, 'n) sface -> 'n D.t
val is_id_sface : ('m, 'n) sface -> unit option
val comp_sface : ('n, 'k) sface -> ('m, 'n) sface -> ('m, 'k) sface
val sface_zero : ('n, D.zero) sface -> ('n, D.zero) Monoid.eq

val sface_plus_sface :
  ('k, 'm) sface ->
  ('m, 'n, 'mn) D.plus ->
  ('k, 'p, 'kp) D.plus ->
  ('p, 'n) sface ->
  ('kp, 'mn) sface

type _ sface_of = SFace_of : ('m, 'n) sface -> 'n sface_of

val sface_plus : ('k, 'm) sface -> ('m, 'n, 'mn) D.plus -> ('k, 'n, 'kn) D.plus -> ('kn, 'mn) sface

val plus_sface :
  'n D.t -> ('n, 'm, 'nm) D.plus -> ('n, 'k, 'nk) D.plus -> ('k, 'm) sface -> ('nk, 'nm) sface

type (_, _, _) sface_of_plus =
  | SFace_of_plus :
      ('m, 'l, 'ml) D.plus * ('m, 'n) sface * ('l, 'k) sface
      -> ('ml, 'n, 'k) sface_of_plus

val sface_of_plus : ('n, 'k, 'nk) D.plus -> ('ml, 'nk) sface -> ('ml, 'n, 'k) sface_of_plus

type ('n, 'f) count_faces
type _ has_faces = Faces : ('n, 'f) count_faces -> 'n has_faces

val count_faces : 'n D.t -> 'n has_faces
val faces_zero : (D.zero, N.one) count_faces
val dim_faces : ('n, 'f) count_faces -> 'n D.t
val faces_pos : ('n, 'f) count_faces -> 'f N.pos
val faces_out : ('n, 'f) count_faces -> 'f N.t
val faces_uniq : ('n, 'f1) count_faces -> ('n, 'f2) count_faces -> ('f1, 'f2) Monoid.eq
val sfaces : ('n, 'f) count_faces -> ('n sface_of, 'f) Bwv.t
(* val sface_int : ('n, 'f) count_faces -> 'n sface_of -> int *)

type _ dbl_sfaces_of =
  | DblOf : ('m, 'f) count_faces * ('m sface_of, 'f) Bwv.t * ('m, 'n) sface -> 'n dbl_sfaces_of

val dbl_sfaces : ('n, 'f) count_faces -> ('n dbl_sfaces_of, 'f) Bwv.t

val sfaces_plus :
  ('m, 'n, 'mn) D.plus ->
  ('m, 'fm) count_faces ->
  ('n, 'fn) count_faces ->
  ('mn, 'fmn) count_faces ->
  ('a -> 'b -> 'c) ->
  ('a, 'fm) Bwv.t ->
  ('b, 'fn) Bwv.t ->
  ('c, 'fmn) Bwv.t

type any_sface = Any_sface : ('n, 'k) sface -> any_sface

val string_of_sface : ('n, 'k) sface -> string
val sface_of_string : string -> any_sface option

module type Fam = sig
  type ('a, 'b) t
end

module Cube (F : Fam) : sig
  type ('m, 'n, 'b) gt
  type ('n, 'b) t = ('n, 'n, 'b) gt

  val dim : ('n, 'b) t -> 'n D.t
  val singleton : (D.zero, 'b) F.t -> (D.zero, 'b) t
  val find : ('n, 'b) t -> ('k, 'n) sface -> ('k, 'b) F.t
  val find_top : ('n, 'b) t -> ('n, 'b) F.t

  module Heter : sig
    type (_, _) hft =
      | [] : ('n, nil) hft
      | ( :: ) : ('n, 'x) F.t * ('n, 'xs) hft -> ('n, ('x, 'xs) cons) hft

    type (_, _, _) hgt =
      | [] : ('m, 'n, nil) hgt
      | ( :: ) : ('m, 'n, 'x) gt * ('m, 'n, 'xs) hgt -> ('m, 'n, ('x, 'xs) cons) hgt

    (* val hft_nil : ('n, Hlist.nil) hft *)
    (* val hft_cons : ('n, 'x) F.t -> ('n, 'xs) hft -> ('n, ('x, 'xs) Hlist.cons) hft *)
  end

  module Infix : sig
    val hnil : ('n, nil) Heter.hft
    val ( @: ) : ('n, 'x) F.t -> ('n, 'xs) Heter.hft -> ('n, ('x, 'xs) cons) Heter.hft
  end

  module Monadic (M : Monad.Plain) : sig
    type ('n, 'bs, 'cs) pmapperM = {
      map : 'm. ('m, 'n) sface -> ('m, 'bs) Heter.hft -> ('m, 'cs) Heter.hft M.t;
    }

    val pmapM :
      ('n, ('b, 'bs) cons, 'cs) pmapperM ->
      ('n, 'n, ('b, 'bs) cons) Heter.hgt ->
      'cs Tlist.t ->
      ('n, 'n, 'cs) Heter.hgt M.t

    type ('n, 'bs, 'c) mmapperM = {
      map : 'm. ('m, 'n) sface -> ('m, 'bs) Heter.hft -> ('m, 'c) F.t M.t;
    }

    val mmapM :
      ('n, ('b, 'bs) cons, 'c) mmapperM -> ('n, 'n, ('b, 'bs) cons) Heter.hgt -> ('n, 'c) t M.t

    type ('n, 'bs) miteratorM = { it : 'm. ('m, 'n) sface -> ('m, 'bs) Heter.hft -> unit M.t }

    val miterM : ('n, ('b, 'bs) cons) miteratorM -> ('n, 'n, ('b, 'bs) cons) Heter.hgt -> unit M.t

    type ('n, 'b) builderM = { build : 'm. ('m, 'n) sface -> ('m, 'b) F.t M.t }

    val buildM : 'n D.t -> ('n, 'b) builderM -> ('n, 'b) t M.t
  end

  module IdM : module type of Monadic (Monad.Identity)

  val pmap :
    ('n, ('b, 'bs) cons, 'cs) IdM.pmapperM ->
    ('n, 'n, ('b, 'bs) cons) Heter.hgt ->
    'cs Tlist.t ->
    ('n, 'n, 'cs) Heter.hgt

  val mmap :
    ('n, ('b, 'bs) cons, 'c) IdM.mmapperM -> ('n, 'n, ('b, 'bs) cons) Heter.hgt -> ('n, 'c) t

  val miter : ('n, ('b, 'bs) cons) IdM.miteratorM -> ('n, 'n, ('b, 'bs) cons) Heter.hgt -> unit
  val build : 'n D.t -> ('n, 'b) IdM.builderM -> ('n, 'b) t

  type ('n, 'c, 'b) fold_lefter = { fold : 'm. 'c -> ('m, 'n) sface -> ('m, 'b) F.t -> 'c }

  val fold_left : ('n, 'c, 'b) fold_lefter -> 'c -> ('n, 'b) t -> 'c
  val subcube : ('m, 'n) sface -> ('n, 'b) t -> ('m, 'b) t
end

module FamOf : sig
  type ('a, 'b) t = 'b
end

module CubeOf : sig
  include module type of Cube (FamOf)

  val flatten_append :
    ('b, 'len) Bwv.t ->
    ('n, 'b) t ->
    ('n, 'f) count_faces ->
    ('len, 'f, 'lenf) N.plus ->
    ('b, 'lenf) Bwv.t

  val flatten : ('n, 'b) t -> ('n, 'f) count_faces -> ('b, 'f) Bwv.t
  val append_bwd : 'a Bwd.t -> ('n, 'a) t -> 'a Bwd.t
end

type (_, _, _, _) tface

val sface_of_tface : ('m, 'n, 'k, 'nk) tface -> ('m, 'nk) sface
val cod_plus_of_tface : ('m, 'n, 'k, 'nk) tface -> ('n, 'k, 'nk) D.plus
val dom_tface : ('m, 'n, 'k, 'nk) tface -> 'm D.t
val codl_tface : ('m, 'n, 'k, 'nk) tface -> 'n D.t
val codr_tface : ('m, 'n, 'k, 'nk) tface -> 'k D.t
val cod_tface : ('m, 'n, 'k, 'nk) tface -> 'nk D.t

val tface_plus :
  ('m, 'n, 'k, 'nk) tface ->
  ('k, 'l, 'kl) D.plus ->
  ('nk, 'l, 'nkl) D.plus ->
  ('m, 'l, 'ml) D.plus ->
  ('ml, 'n, 'kl, 'nkl) tface

type ('m, 'n) pface = ('m, D.zero, 'n, 'n) tface

val sface_plus_tface :
  ('k, 'm) sface ->
  ('m, 'n, 'mn) D.plus ->
  ('m, 'nl, 'mnl) D.plus ->
  ('k, 'p, 'kp) D.plus ->
  ('p, 'n, 'l, 'nl) tface ->
  ('kp, 'mn, 'l, 'mnl) tface

val sface_plus_pface :
  ('k, 'm) sface ->
  ('m, 'n, 'mn) D.plus ->
  ('k, 'p, 'kp) D.plus ->
  ('p, 'n) pface ->
  ('kp, 'm, 'n, 'mn) tface

type (_, _, _, _) tface_of_plus =
  | TFace_of_plus :
      ('p, 'q, 'pq) D.plus * ('p, 'n) sface * ('q, 'k, 'l, 'kl) tface
      -> ('pq, 'n, 'k, 'l) tface_of_plus

val tface_of_plus :
  ('n, 'k, 'nk) D.plus -> ('m, 'nk, 'l, 'nkl) tface -> ('m, 'n, 'k, 'l) tface_of_plus

type (_, _, _) pface_of_plus =
  | PFace_of_plus :
      ('p, 'q, 'pq) D.plus * ('p, 'n) sface * ('q, 'k) pface
      -> ('pq, 'n, 'k) pface_of_plus

val pface_of_plus : ('m, 'n, 'k, 'nk) tface -> ('m, 'n, 'k) pface_of_plus

module Tube (F : Fam) : sig
  module C : module type of Cube (F)

  type ('n, 'k, 'nk, 'm, 'b) gt
  type ('n, 'k, 'nk, 'b) t = ('n, 'k, 'nk, 'nk, 'b) gt

  val find : ('n, 'k, 'nk, 'b) t -> ('m, 'n, 'k, 'nk) tface -> ('m, 'b) F.t
  val boundary : ('n, 'b) C.t -> (D.zero, 'n, 'n, 'b) t

  val pboundary :
    ('m, 'k, 'mk) D.plus -> ('k, 'l, 'kl) D.plus -> ('m, 'kl, 'mkl, 'b) t -> ('mk, 'l, 'mkl, 'b) t

  val plus : ('m, 'k, 'mk, 'b) t -> ('m, 'k, 'mk) D.plus
  val inst : ('m, 'k, 'mk, 'b) t -> 'k D.t
  val uninst : ('m, 'k, 'mk, 'b) t -> 'm D.t
  val out : ('m, 'k, 'mk, 'b) t -> 'mk D.t
  val empty : 'n D.t -> ('n, D.zero, 'n, 'b) t

  module Heter : sig
    type (_, _, _, _, _) hgt =
      | [] : ('m, 'k, 'mk, 'nk, nil) hgt
      | ( :: ) :
          ('m, 'k, 'mk, 'nk, 'x) gt * ('m, 'k, 'mk, 'nk, 'xs) hgt
          -> ('m, 'k, 'mk, 'nk, ('x, 'xs) cons) hgt
  end

  module Infix : module type of C.Infix

  module Monadic (M : Monad.Plain) : sig
    type ('n, 'k, 'nk, 'bs, 'cs) pmapperM = {
      map : 'm. ('m, 'n, 'k, 'nk) tface -> ('m, 'bs) C.Heter.hft -> ('m, 'cs) C.Heter.hft M.t;
    }

    val pmapM :
      ('n, 'k, 'nk, ('b, 'bs) cons, 'cs) pmapperM ->
      ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
      'cs Tlist.t ->
      ('n, 'k, 'nk, 'nk, 'cs) Heter.hgt M.t

    type ('n, 'k, 'nk, 'bs, 'c) mmapperM = {
      map : 'm. ('m, 'n, 'k, 'nk) tface -> ('m, 'bs) C.Heter.hft -> ('m, 'c) F.t M.t;
    }

    val mmapM :
      ('n, 'k, 'nk, ('b, 'bs) cons, 'c) mmapperM ->
      ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
      ('n, 'k, 'nk, 'c) t M.t

    type ('n, 'k, 'nk, 'bs) miteratorM = {
      it : 'm. ('m, 'n, 'k, 'nk) tface -> ('m, 'bs) C.Heter.hft -> unit M.t;
    }

    val miterM :
      ('n, 'k, 'nk, ('b, 'bs) cons) miteratorM ->
      ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
      unit M.t

    type ('n, 'k, 'nk, 'b) builderM = { build : 'm. ('m, 'n, 'k, 'nk) tface -> ('m, 'b) F.t M.t }

    val buildM :
      'n D.t -> ('n, 'k, 'nk) D.plus -> ('n, 'k, 'nk, 'b) builderM -> ('n, 'k, 'nk, 'b) t M.t
  end

  module IdM : module type of Monadic (Monad.Identity)

  val pmap :
    ('n, 'k, 'nk, ('b, 'bs) cons, 'cs) IdM.pmapperM ->
    ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
    'cs Tlist.t ->
    ('n, 'k, 'nk, 'nk, 'cs) Heter.hgt

  val mmap :
    ('n, 'k, 'nk, ('b, 'bs) cons, 'c) IdM.mmapperM ->
    ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
    ('n, 'k, 'nk, 'c) t

  val miter :
    ('n, 'k, 'nk, ('b, 'bs) cons) IdM.miteratorM ->
    ('n, 'k, 'nk, 'nk, ('b, 'bs) cons) Heter.hgt ->
    unit

  val build :
    'n D.t -> ('n, 'k, 'nk) D.plus -> ('n, 'k, 'nk, 'b) IdM.builderM -> ('n, 'k, 'nk, 'b) t
end

module TubeOf : sig
  include module type of Tube (FamOf)

  val plus_cube : ('mk, 'l, 'mkl, 'b) t -> ('mk, 'b) C.t -> ('mkl, 'b) C.t

  val plus_tube :
    ('k, 'l, 'kl) D.plus -> ('mk, 'l, 'mkl, 'b) t -> ('m, 'k, 'mk, 'b) t -> ('m, 'kl, 'mkl, 'b) t

  val middle :
    ('m, 'k, 'mk) D.plus -> ('k, 'l, 'kl) D.plus -> ('m, 'kl, 'mkl, 'b) t -> ('m, 'k, 'mk, 'b) t

  val append_bwd : 'a Bwd.t -> ('m, 'n, 'mn, 'a) t -> 'a Bwd.t
end

type (_, _) face = Face : ('m, 'n) sface * 'm perm -> ('m, 'n) face

val id_face : 'n D.t -> ('n, 'n) face
val perm_sface : 'n perm -> ('m, 'n) sface -> ('m, 'n) face
val comp_face : ('n, 'k) face -> ('m, 'n) face -> ('m, 'k) face
val dom_face : ('m, 'n) face -> 'm D.t
val cod_face : ('m, 'n) face -> 'n D.t
val face_of_sface : ('m, 'n) sface -> ('m, 'n) face
val face_of_perm : 'm perm -> ('m, 'm) face

val face_plus_face :
  ('k, 'm) face -> ('m, 'n, 'mn) D.plus -> ('k, 'l, 'kl) D.plus -> ('l, 'n) face -> ('kl, 'mn) face

type _ face_of = Face_of : ('m, 'n) face -> 'n face_of
type (_, _) op = Op : ('n, 'k) sface * ('m, 'n) deg -> ('m, 'k) op

val id_op : 'n D.t -> ('n, 'n) op
val deg_sface : ('n, 'k) deg -> ('m, 'n) sface -> ('m, 'k) op
val comp_op : ('n, 'k) op -> ('m, 'n) op -> ('m, 'k) op
val dom_op : ('m, 'n) op -> 'm D.t
val cod_op : ('m, 'n) op -> 'n D.t
val op_of_deg : ('m, 'n) deg -> ('m, 'n) op
val op_of_sface : ('m, 'n) sface -> ('m, 'n) op

val op_plus_op :
  ('k, 'm) op -> ('m, 'n, 'mn) D.plus -> ('k, 'l, 'kl) D.plus -> ('l, 'n) op -> ('kl, 'mn) op

type _ op_of = Of : ('m, 'n) op -> 'n op_of
type _ op_of_plus = Of : ('m, 'n) sface * 'm deg_of_plus -> 'n op_of_plus

val comp_op_of : ('m, 'n) op -> 'm op_of -> 'n op_of
val comp_op_deg_of_plus : ('m, 'n) op -> 'm deg_of_plus -> 'n op_of_plus

type (_, _, _) insertion

val ins_zero : 'a D.t -> ('a, 'a, D.zero) insertion
val zero_ins : 'a D.t -> ('a, D.zero, 'a) insertion

type (_, _) id_ins = Id_ins : ('ab, 'a, 'b) insertion -> ('a, 'b) id_ins

val id_ins : 'a D.t -> 'b D.t -> ('a, 'b) id_ins
val dom_ins : ('a, 'b, 'c) insertion -> 'a D.t
val cod_left_ins : ('a, 'b, 'c) insertion -> 'b D.t
val cod_right_ins : ('a, 'b, 'c) insertion -> 'c D.t
val plus_of_ins : ('a, 'b, 'c) insertion -> ('b, 'c, 'a) D.plus
val deg_of_ins : ('a, 'b, 'c) insertion -> ('b, 'c, 'bc) D.plus -> ('a, 'bc) deg
val perm_of_ins : ('a, 'b, 'c) insertion -> 'a perm
val is_id_ins : ('a, 'b, 'c) insertion -> unit option
val deg_of_plus_of_ins : ('a, 'b, 'c) insertion -> 'b deg_of_plus

val plus_ins :
  'a D.t ->
  ('a, 'b, 'ab) D.plus ->
  ('a, 'bc, 'abc) D.plus ->
  ('bc, 'b, 'c) insertion ->
  ('abc, 'ab, 'c) insertion

type (_, _, _) insfact = Insfact : ('a, 'b) deg * ('ac, 'a, 'c) insertion -> ('ac, 'b, 'c) insfact

val comp_insfact : ('ac, 'b, 'c) insfact -> ('b, 'c, 'bc) D.plus -> ('ac, 'bc) deg
val insfact : ('ac, 'bc) deg -> ('b, 'c, 'bc) D.plus -> ('ac, 'b, 'c) insfact

type (_, _, _) insfact_comp =
  | Insfact_comp :
      ('m, 'n) deg * ('ml, 'm, 'l) insertion * ('k, 'j, 'l) D.plus * ('a, 'i, 'ml) D.plus
      -> ('n, 'k, 'a) insfact_comp

val insfact_comp : ('nk, 'n, 'k) insertion -> ('a, 'b) deg -> ('n, 'k, 'a) insfact_comp

module Plusmap : sig
  module OfDom : module type of Tbwd.Of (D)
  module OfCod : module type of Tbwd.Of (D) with type 'a t = 'a OfDom.t

  type ('a, 'b, 'c) t =
    | Map_emp : ('p, emp, emp) t
    | Map_snoc : ('p, 'xs, 'ys) t * ('p, 'x, 'y) D.plus -> ('p, ('xs, 'x) snoc, ('ys, 'y) snoc) t

  type ('a, 'b) exists = Exists : 'ys OfCod.t * ('p, 'xs, 'ys) t -> ('p, 'xs) exists

  val exists : 'p D.t -> 'xs OfDom.t -> ('p, 'xs) exists
  val out : 'p D.t -> 'xs OfDom.t -> ('p, 'xs, 'ys) t -> 'ys OfCod.t
  val uniq : ('p, 'xs, 'ys) t -> ('p, 'xs, 'zs) t -> ('ys, 'zs) Monoid.eq

  type (_, _, _, _) map_insert =
    | Map_insert : ('zs, 'fx, 'ws) Tbwd.insert * ('p, 'ys, 'ws) t -> ('p, 'fx, 'ys, 'zs) map_insert

  val insert :
    ('p, 'x, 'z) D.plus ->
    ('xs, 'x, 'ys) Tbwd.insert ->
    ('p, 'xs, 'zs) t ->
    ('p, 'z, 'ys, 'zs) map_insert

  type (_, _, _, _) unmap_insert =
    | Unmap_insert :
        ('p, 'x, 'z) D.plus * ('xs, 'x, 'ys) Tbwd.insert * ('p, 'xs, 'zs) t
        -> ('p, 'z, 'ys, 'zs) unmap_insert

  val unmap_insert :
    ('zs, 'z, 'ws) Tbwd.insert -> ('p, 'ys, 'ws) t -> ('p, 'z, 'ys, 'zs) unmap_insert

  type (_, _, _) map_permute =
    | Map_permute : ('p, 'zs, 'ws) t * ('ys, 'ws) Tbwd.permute -> ('p, 'zs, 'ys) map_permute

  val permute : ('p, 'xs, 'ys) t -> ('xs, 'zs) Tbwd.permute -> ('p, 'zs, 'ys) map_permute

  val assocl :
    ('a, 'b, 'ab) D.plus -> ('b, 'cs, 'bcs) t -> ('a, 'bcs, 'abcs) t -> ('ab, 'cs, 'abcs) t

  val zerol : 'bs OfDom.t -> (D.zero, 'bs, 'bs) t
  end

type (_, _, _, _) pbij

val zero : 'a D.t -> ('a, 'a, D.zero, D.zero) pbij
val intrinsic_pbij : ('x, 'kx, 'ky, 'y) pbij -> 'kx D.t
val ambient_pbij : ('x, 'kx, 'ky, 'y) pbij -> 'ky D.t

type (_, _) any_pbij = Any : ('a, 'ax, 'by, 'b) pbij -> ('ax, 'by) any_pbij

val pbijs : 'ax D.t -> 'by D.t -> ('ax, 'by) any_pbij list

module Pbij_strings : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val compare : t -> t -> int
  val of_strings : string list -> t option
  val to_strings : t -> string list
end

val pbij_of_strings : Pbij_strings.t -> 'ax D.t -> 'by D.t -> ('ax, 'by) any_pbij option
val strings_of_pbij : ('a, 'ax, 'by, 'b) pbij -> Pbij_strings.t
val comp_deg_pbij : ('ky, 'n) deg -> ('x, 'kx, 'ky, 'y) pbij -> ('kx, 'n) any_pbij

type (_, _, _) comp_pbij_deg =
  | Comp_pbij_deg : ('x, 'kx, 'ky, 'y) pbij -> ('x, 'kx, 'ky) comp_pbij_deg

val comp_pbij_deg : ('x, 'kx, 'ky, 'y) pbij -> ('m, 'ky) deg -> ('x, 'kx, 'm) comp_pbij_deg

(*  *)
val one : one D.t
val refl : (one, D.zero) deg
val zero_sface_one : (D.zero, one) sface
val one_sface_one : (D.zero, one) sface

type _ is_suc = Is_suc : 'n D.t * ('n, one, 'm) D.plus -> 'm is_suc

val suc_pos : 'n D.pos -> 'n is_suc
val deg_of_name : string -> any_deg option
val name_of_deg : ('a, 'b) deg -> string option
