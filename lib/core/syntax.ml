open Bwd
open Util
open Tbwd
open Dim
open Asai.Range
include Energy

(* ******************** Raw (unchecked) terms ******************** *)

module Raw = struct
  (* Raw (unchecked) terms, using intrinsically well-scoped De Bruijn indices, and separated into synthesizing terms and checking terms.  These match the user-facing syntax rather than the internal syntax.  In particular, applications, abstractions, and pi-types are all unary, there is only one universe, and the only operator actions are refl (including Id) and sym. *)

  (* A raw De Bruijn index is a well-scoped natural number together with a possible face.  During typechecking we will verify that the face, if given, is applicable to the variable as a "cube variable", and compile the combination into a more strongly well-scoped kind of index. *)
  type 'a index = 'a N.index * any_sface option

  type _ synth =
    | Var : 'a index -> 'a synth
    | Const : Constant.t -> 'a synth
    | Field : 'a synth located * Field.or_index -> 'a synth
    | Pi : string option * 'a check located * 'a N.suc check located -> 'a synth
    | App : 'a synth located * 'a check located -> 'a synth
    | Asc : 'a check located * 'a check located -> 'a synth
    | UU : 'a synth
    (* A Let can either synthesize or (sometimes) check.  It synthesizes only if its body also synthesizes, but we wait until typechecking type to look for that, so that if it occurs in a checking context the body can also be checking.  Thus, we make it a "synthesizing term".  The term being bound must also synthesize; the shorthand notation "let x : A := M" is expanded during parsing to "let x := M : A". *)
    | Let : string option * 'a synth located * 'a N.suc check located -> 'a synth
    (* An Act can also sometimes check, if its body checks and the degeneracy is a pure permutation.  But otherwise, it synthesizes and so must its body.  *)
    | Act : string * ('m, 'n) deg * 'a check located -> 'a synth
    (* A Match can also sometimes check, but synthesizes if it has an explicit return type or if it is nondependent and its first branch synthesizes. *)
    | Match : {
        tm : 'a synth located;
        (* Implicit means no "return" statement was given, so Narya has to guess what to do.  Explicit means a "return" statement was given with a motive.  "Nondep" means a placeholder return statement like "_ ↦ _" was given, indicating that a non-dependent matching is intended (to silence hints about fallback from the implicit case). *)
        sort : [ `Implicit | `Explicit of 'a check located | `Nondep of int located ];
        branches : (Constr.t, 'a branch) Abwd.t;
        refutables : 'a refutables;
      }
        -> 'a synth

  and _ check =
    | Synth : 'a synth -> 'a check
    | Lam : string option located * [ `Cube | `Normal ] * 'a N.suc check located -> 'a check
    (* A "Struct" is our current name for both tuples and comatches, which share a lot of their implementation even though they are conceptually and syntactically distinct.  Those with eta=`Eta are tuples, those with eta=`Noeta are comatches.  We index them by a "Field.t option" so as to include any unlabeled fields, with their relative order to the labeled ones. *)
    | Struct : 's eta * (Field.t option, 'a check located) Abwd.t -> 'a check
    | Constr : Constr.t located * 'a check located list -> 'a check
    (* "[]", which could be either an empty pattern-matching lambda or an empty comatch *)
    | Empty_co_match : 'a check
    | Data : (Constr.t, 'a dataconstr located) Abwd.t -> 'a check
    (* A codatatype binds one more "self" variable in the types of each of its fields.  For a higher-dimensional codatatype (like a codata version of Gel), this becomes a cube of variables. *)
    | Codata : (Field.t, 'a codatafield) Abwd.t -> 'a check
    (* A record type binds its "self" variable namelessly, exposing it to the user by additional variables that are bound locally to its fields.  This can't be "cubeified" as easily, so we have the user specify a list of ordinary variables to be its boundary.  Thus, in practice below 'c must be a number of faces associated to a dimension, but the parser doesn't know the dimension, so it can't ensure that.  The unnamed internal variable is included as the last one. *)
    | Record :
        ('a, 'c, 'ac) Fwn.bplus located * (string option, 'c) Vec.t * ('ac, 'd, 'acd) tel * opacity
        -> 'a check
    (* A hole must store the entire "state" from when it was entered, so that the user can later go back and fill it with a term that would have been valid in its original position.  This includes the variables in lexical scope, which are available only during parsing, so we store them here at that point.  During typechecking, when the actual metavariable is created, we save the lexical scope along with its other context and type data. *)
    | Hole : (string option, 'a) Bwv.t -> 'a check
    (* Empty match against the first one of the arguments belonging to an empty type. *)
    | Refute : 'a synth located list * [ `Explicit | `Implicit ] -> 'a check

  and _ branch =
    (* The location of the third argument is that of the entire pattern. *)
    | Branch :
        (string option, 'b) Vec.t * ('a, 'b, 'ab) Fwn.bplus located * 'ab check located
        -> 'a branch

  and _ dataconstr = Dataconstr : ('a, 'b, 'ab) tel * 'ab check located option -> 'a dataconstr

  (* A field of a codatatype has a specified dimension as well as a self variable and a type.  At the raw level we don't need any more information about higher fields. *)
  and _ codatafield =
    | Codatafield : string option * 'k D.t * 'a N.suc check located -> 'a codatafield

  (* A raw match stores the information about the pattern variables available from previous matches that could be used to refute missing cases.  But it can't store them as raw terms, since they have to be in the correct context extended by the new pattern variables generated in any such case.  So it stores them as a callback that puts them in any such extended context. *)
  and 'a refutables = { refutables : 'b 'ab. ('a, 'b, 'ab) Fwn.bplus -> 'ab synth located list }

  (* An ('a, 'b, 'ab) tel is a raw telescope of length 'b in context 'a, with 'ab = 'a+'b the extended context. *)
  and (_, _, _) tel =
    | Emp : ('a, Fwn.zero, 'a) tel
    | Ext : string option * 'a check located * ('a N.suc, 'b, 'ab) tel -> ('a, 'b Fwn.suc, 'ab) tel

  let rec dataconstr_of_pi : type a. a check located -> a dataconstr =
   fun ty ->
    match ty.value with
    | Synth (Pi (x, dom, cod)) ->
        let (Dataconstr (tel, out)) = dataconstr_of_pi cod in
        Dataconstr (Ext (x, dom, tel), out)
    | _ -> Dataconstr (Emp, Some ty)

  let rec lams :
      type a b ab.
      (a, b, ab) Fwn.bplus ->
      (string option located, b) Vec.t ->
      ab check located ->
      Asai.Range.t option ->
      a check located =
   fun ab xs tm loc ->
    match (ab, xs) with
    | Zero, [] -> tm
    | Suc ab, x :: xs -> { value = Lam (x, `Normal, lams ab xs tm loc); loc }
end

(* ******************** Names ******************** *)

(* An element of "mn variables" is an mn-dimensional cube of variables where mn = m + n and the user specified names for n dimensions, with the other m dimensions being named with face suffixes.  *)
type _ variables =
  | Variables :
      'm D.t * ('m, 'n, 'mn) D.plus * (N.zero, 'n, string option, 'f) NICubeOf.t
      -> 'mn variables

type any_variables = Any : 'n variables -> any_variables

let dim_variables : type m. m variables -> m D.t = function
  | Variables (m, mn, _) -> D.plus_out m mn

let singleton_variables : type m. m D.t -> string option -> m variables =
 fun m x -> Variables (m, D.plus_zero m, NICubeOf.singleton x)

(* ******************** Typechecked terms ******************** *)

(* Typechecked, but unevaluated, terms.  Use De Bruijn indices that are intrinsically well-scoped by hctxs, but are no longer separated into synthesizing and checking; hence without type ascriptions.  Note that extending an hctx by a dimension 'k means adding a whole cube of new variables, which are indexed by the position of that dimension together with a strict face of it.  (At user-level, those variables may all be accessed as faces of one "cube variable", or they may have independent names, but internally there is no difference.)

   Incorporates information appropriate to the internal syntax that is constructed during typechecking, e.g. applications and abstractions are grouped by a dimension, since this can be inferred during typechecking, from the synthesized type of a function being applied and from the pi-type the lambda is being checked against, respectively.  Similarly, we have instantiations of higher-dimensional types obtained by applying them to a tube of boundary terms.

   Typechecking of user syntax still produces only unary pi-types and zero-dimensional universes, but we include arbitrary-dimensional ones here so that they can also be the output of readback from internal values.  We likewise include arbitrary degeneracy actions, since these can appear in readback. *)

(* The codomain of a higher-dimensional pi-type is a cube of terms with bound variables whose number varies with the face of the cube.  We can enforce this with a parametrized instance of Cube, but it has to be defined recursively with term using a recursive module (like BindCube in Value; see there for more commentary). *)
module rec Term : sig
  module CodFam : sig
    type ('k, 'a) t = (('a, 'k) snoc, kinetic) Term.term
  end

  module CodCube : module type of Cube (CodFam)

  type _ index = Index : ('a, 'n, 'b) Tbwd.insert * ('k, 'n) sface -> 'b index

  type (_, _) term =
    | Var : 'a index -> ('a, kinetic) term
    | Const : Constant.t -> ('a, kinetic) term
    | Meta : ('a, 'l) Meta.t * 's energy -> ('a, 's) term
    | MetaEnv : ('b, 's) Meta.t * ('a, 'n, 'b) env -> ('a, kinetic) term
    | Field : ('a, kinetic) term * Field.t -> ('a, kinetic) term
    | UU : 'n D.t -> ('a, kinetic) term
    | Inst : ('a, kinetic) term * ('m, 'n, 'mn, ('a, kinetic) term) TubeOf.t -> ('a, kinetic) term
    | Pi :
        string option * ('n, ('a, kinetic) term) CubeOf.t * ('n, 'a) CodCube.t
        -> ('a, kinetic) term
    | App : ('a, kinetic) term * ('n, ('a, kinetic) term) CubeOf.t -> ('a, kinetic) term
    | Constr : Constr.t * 'n D.t * ('n, ('a, kinetic) term) CubeOf.t list -> ('a, kinetic) term
    | Act : ('a, kinetic) term * ('m, 'n) deg -> ('a, kinetic) term
    | Let : string option * ('a, kinetic) term * (('a, D.zero) snoc, 's) term -> ('a, 's) term
    | Lam : 'n variables * (('a, 'n) snoc, 's) Term.term -> ('a, 's) term
    | Struct :
        's eta * 'n D.t * (Field.t, ('a, 's) term * [ `Labeled | `Unlabeled ]) Abwd.t
        -> ('a, 's) term
    | Match : {
        tm : ('a, kinetic) term;
        dim : 'n D.t;
        branches : ('a, 'n) branch Constr.Map.t;
      }
        -> ('a, potential) term
    | Realize : ('a, kinetic) term -> ('a, potential) term
    | Canonical : 'a canonical -> ('a, potential) term

  and (_, _) branch =
    | Branch :
        ('a, 'b, 'n, 'ab) Tbwd.snocs * ('c, 'ab) Tbwd.permute * ('c, potential) term
        -> ('a, 'n) branch
    | Refute

  and _ canonical =
    | Data : {
        indices : 'i Fwn.t;
        constrs : (Constr.t, ('a, 'i) dataconstr) Abwd.t;
        discrete : bool;
      }
        -> 'a canonical
    | Codata : {
        eta : potential eta;
        opacity : opacity;
        dim : 'n D.t;
        fields : (Field.t, ('a, 'n) codatafield) Abwd.t;
      }
        -> 'a canonical

  and (_, _) dataconstr =
    | Dataconstr : {
        args : ('p, 'a, 'pa) tel;
        indices : (('pa, kinetic) term, 'i) Vec.t;
      }
        -> ('p, 'i) dataconstr

  and (_, _) codatafield =
    | Lower_codatafield : (('a, 'n) snoc, kinetic) term -> ('a, 'n) codatafield
    | Higher_codatafield :
        'k D.pos * ('k, ('a, D.zero) snoc, 'kan) Plusmap.t * ('kan, kinetic) term
        -> ('a, D.zero) codatafield

  and ('a, 'b, 'ab) tel =
    | Emp : ('a, Fwn.zero, 'a) tel
    | Ext :
        string option * ('a, kinetic) term * (('a, D.zero) snoc, 'b, 'ab) tel
        -> ('a, 'b Fwn.suc, 'ab) tel

  and (_, _, _) env =
    | Emp : 'n D.t -> ('a, 'n, emp) env
    | Ext :
        ('a, 'n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, ('a, kinetic) term) CubeOf.t
        -> ('a, 'n, ('b, 'k) snoc) env
end = struct
  module CodFam = struct
    type ('k, 'a) t = (('a, 'k) snoc, kinetic) Term.term
  end

  module CodCube = Cube (CodFam)

  (* A typechecked De Bruijn index is a well-scoped natural number together with a definite strict face (the top face, if none was supplied explicitly).  Unlike a raw De Bruijn index, the scoping is by a tbwd rather than a type-level nat.  This allows the face to also be well-scoped: its codomain must be the dimension appearing in the hctx at that position.  And since we already have defined Tbwd.insert, we can re-use that instead of re-defining this inductively. *)
  type _ index = Index : ('a, 'n, 'b) Tbwd.insert * ('k, 'n) sface -> 'b index

  type (_, _) term =
    (* Most term-formers only appear in kinetic (ordinary) terms. *)
    | Var : 'a index -> ('a, kinetic) term
    | Const : Constant.t -> ('a, kinetic) term
    | Meta : ('a, 'l) Meta.t * 's energy -> ('a, 's) term
    (* Normally, checked metavariables don't require an environment attached, but they do when they arise by readback from a value metavariable. *)
    | MetaEnv : ('b, 's) Meta.t * ('a, 'n, 'b) env -> ('a, kinetic) term
    | Field : ('a, kinetic) term * Field.t -> ('a, kinetic) term
    | UU : 'n D.t -> ('a, kinetic) term
    | Inst : ('a, kinetic) term * ('m, 'n, 'mn, ('a, kinetic) term) TubeOf.t -> ('a, kinetic) term
    (* Since the user doesn't write higher-dimensional pi-types explicitly, there is always only one variable name in a pi-type. *)
    | Pi :
        string option * ('n, ('a, kinetic) term) CubeOf.t * ('n, 'a) CodCube.t
        -> ('a, kinetic) term
    | App : ('a, kinetic) term * ('n, ('a, kinetic) term) CubeOf.t -> ('a, kinetic) term
    | Constr : Constr.t * 'n D.t * ('n, ('a, kinetic) term) CubeOf.t list -> ('a, kinetic) term
    | Act : ('a, kinetic) term * ('m, 'n) deg -> ('a, kinetic) term
    (* The term being bound in a 'let' is always kinetic.  Thus, if the supplied bound term is potential, the "bound term" here must be the metavariable whose value is set to that term rather than to the (potential) term itself. *)
    | Let : string option * ('a, kinetic) term * (('a, D.zero) snoc, 's) term -> ('a, 's) term
    (* Abstractions and structs can appear in any kind of term.  The dimension 'n is the substitution dimension of the type being checked against (function-type or codata/record).  *)
    | Lam : 'n variables * (('a, 'n) snoc, 's) Term.term -> ('a, 's) term
    | Struct :
        's eta * 'n D.t * (Field.t, ('a, 's) term * [ `Labeled | `Unlabeled ]) Abwd.t
        -> ('a, 's) term
    (* Matches can only appear in non-kinetic terms.  The dimension 'n is the substitution dimension of the type of the variable being matched against. *)
    | Match : {
        tm : ('a, kinetic) term;
        dim : 'n D.t;
        branches : ('a, 'n) branch Constr.Map.t;
      }
        -> ('a, potential) term
    (* A potential term is "realized" by kinetic terms, or canonical types, at its leaves. *)
    | Realize : ('a, kinetic) term -> ('a, potential) term
    | Canonical : 'a canonical -> ('a, potential) term

  (* A branch of a match binds a number of new variables.  If it is a higher-dimensional match, then each of those "variables" is actually a full cube of variables.  In addition, its context must be permuted to put those new variables before the existing variables that are now defined in terms of them. *)
  and (_, _) branch =
    | Branch :
        ('a, 'b, 'n, 'ab) Tbwd.snocs * ('c, 'ab) Tbwd.permute * ('c, potential) term
        -> ('a, 'n) branch
    (* A branch that was refuted during typechecking doesn't need a body to compute with, but we still mark its presence as a signal that it should be stuck (this can occur when normalizing in an inconsistent context). *)
    | Refute

  (* A canonical type is either a datatype or a codatatype/record. *)
  and _ canonical =
    (* A datatype stores its family of constructors, and also its number of indices.  (The former is not determined in the latter if there happen to be zero constructors). *)
    | Data : {
        indices : 'i Fwn.t;
        constrs : (Constr.t, ('a, 'i) dataconstr) Abwd.t;
        discrete : bool;
      }
        -> 'a canonical
    (* A codatatype has an eta flag, an intrinsic dimension (like Gel), and a family of fields, each with a type that depends on one additional variable belonging to the codatatype itself (usually by way of its previous fields).  We retain the order of the fields by storing them in an Abwd rather than a Map so as to enable positional access as well as named access. *)
    | Codata : {
        eta : potential eta;
        opacity : opacity;
        dim : 'n D.t;
        fields : (Field.t, ('a, 'n) codatafield) Abwd.t;
      }
        -> 'a canonical

  (* A datatype constructor has a telescope of arguments and a list of index values depending on those arguments. *)
  and (_, _) dataconstr =
    | Dataconstr : {
        args : ('p, 'a, 'pa) tel;
        indices : (('pa, kinetic) term, 'i) Vec.t;
      }
        -> ('p, 'i) dataconstr

  (* A codatafield has an intrinsic dimension, and a type that depends on one additional variable, but in a degenerated context.  If it is zero-dimensional, the degeneration does nothing, but we store that case separately so we don't need to construct and carry around lots of trivial Plusmaps. *)
  and (_, _) codatafield =
    | Lower_codatafield : (('a, 'n) snoc, kinetic) term -> ('a, 'n) codatafield
    (* For the present, we don't allow higher fields in higher-dimensional codatatypes, just for simplicity. *)
    | Higher_codatafield :
        'k D.pos * ('k, ('a, D.zero) snoc, 'kan) Plusmap.t * ('kan, kinetic) term
        -> ('a, D.zero) codatafield

  (* A telescope is a list of types, each dependent on the previous ones. *)
  and ('a, 'b, 'ab) tel =
    | Emp : ('a, Fwn.zero, 'a) tel
    | Ext :
        string option * ('a, kinetic) term * (('a, D.zero) snoc, 'b, 'ab) tel
        -> ('a, 'b Fwn.suc, 'ab) tel

  (* A version of an environment (see below) that involves terms rather than values.  Used mainly when reading back metavariables. *)
  and (_, _, _) env =
    | Emp : 'n D.t -> ('a, 'n, emp) env
    | Ext :
        ('a, 'n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, ('a, kinetic) term) CubeOf.t
        -> ('a, 'n, ('b, 'k) snoc) env
end

open Term

(* Find the name of the (n+1)st abstracted variable, where n is the length of a supplied argument list.  Doesn't "look through" branches or cobranches or into leaves. *)
let rec nth_var : type a b s. (a, s) term -> b Bwd.t -> any_variables option =
 fun tr args ->
  match tr with
  | Lam (x, body) -> (
      match args with
      | Emp -> Some (Any x)
      | Snoc (args, _) -> nth_var body args)
  | _ -> None

let pi x dom cod = Pi (x, CubeOf.singleton dom, CodCube.singleton cod)
let app fn arg = App (fn, CubeOf.singleton arg)
let apps fn args = List.fold_left app fn args
let constr name args = Constr (name, D.zero, List.map CubeOf.singleton args)

module Telescope = struct
  type ('a, 'b, 'ab) t = ('a, 'b, 'ab) Term.tel

  let rec length : type a b ab. (a, b, ab) t -> b Fwn.t = function
    | Emp -> Zero
    | Ext (_, _, tel) -> Suc (length tel)

  let rec pis : type a b ab. (a, b, ab) t -> (ab, kinetic) term -> (a, kinetic) term =
   fun doms cod ->
    match doms with
    | Emp -> cod
    | Ext (x, dom, doms) -> pi x dom (pis doms cod)

  let rec lams : type a b ab. (a, b, ab) t -> (ab, kinetic) term -> (a, kinetic) term =
   fun doms body ->
    match doms with
    | Emp -> body
    | Ext (x, _, doms) -> Lam (singleton_variables D.zero x, lams doms body)
end

let rec dim_term_env : type a n b. (a, n, b) env -> n D.t = function
  | Emp n -> n
  | Ext (e, _, _) -> dim_term_env e

(* ******************** Values ******************** *)

(* A De Bruijn level is a pair of integers: one for the position (counting in) of the cube-variable-bundle in the context, and one that counts through the faces of that bundle. *)
type level = int * int

(* Internal values, the result of evaluation, with closures for abstractions.  Use De Bruijn *levels*, so that weakening is implicit.  Fully internal unbiased syntax lives here: in addition to higher-dimensional applications and abstractions, we also have higher-dimensional pi-types, higher-dimensional universes, and instantiations of higher-dimensional types.  Separated into neutrals and normals, so that there are no beta-redexes.  Explicit substitutions (environments) are stored on binders, for NBE. *)

(* The codomains of a pi-type are stored as a Cube of binders, and since binders are a type family this dependence has to be specified by applying a module functor (rather than just parametrizing a type).  Since values are defined mutually with binders, we need to "apply the functor Cube" mutually with the definition of these types.  This is possible using a recursive module. *)
module rec Value : sig
  (* A recursive module is required to specify its module type explicitly.  We make this as transparent as possible, so the module type is nearly a copy of the module itself.  For the comments, see the actual definition below. *)
  module BindFam : sig
    type ('k, 'b) t = ('k, kinetic) Value.binder
  end

  module BindCube : module type of Cube (BindFam)

  type head =
    | Var : { level : level; deg : ('m, 'n) deg } -> head
    | Const : { name : Constant.t; ins : ('a, 'b, 'c) insertion } -> head
    | Meta : { meta : ('b, 's) Meta.t; env : ('m, 'b) env; ins : ('mn, 'm, 'n) insertion } -> head

  and 'n arg = Arg of ('n, normal) CubeOf.t | Field of Field.t
  and app = App : 'n arg * ('m, 'n, 'k) insertion -> app

  and (_, _) binder =
    | Bind : {
        env : ('m, 'a) env;
        body : (('a, 'n) snoc, 's) term;
        ins : ('mn, 'm, 'n) insertion;
      }
        -> ('mn, 's) binder

  and alignment =
    | True : alignment
    | Chaotic : potential value -> alignment
    | Lawful : canonical -> alignment

  and uninst =
    | UU : 'n D.t -> uninst
    | Pi : string option * ('k, kinetic value) CubeOf.t * ('k, unit) BindCube.t -> uninst
    | Neu : { head : head; args : app Bwd.t; alignment : alignment } -> uninst

  and _ value =
    | Uninst : uninst * kinetic value Lazy.t -> kinetic value
    | Inst : {
        tm : uninst;
        dim : 'k D.pos;
        args : ('n, 'k, 'nk, normal) TubeOf.t;
        tys : (D.zero, 'n, 'n, kinetic value) TubeOf.t;
      }
        -> kinetic value
    | Constr : Constr.t * 'n D.t * ('n, kinetic value) CubeOf.t list -> kinetic value
    | Lam : 'k variables * ('k, 's) binder -> 's value
    | Struct :
        (Field.t, 's lazy_eval * [ `Labeled | `Unlabeled ]) Abwd.t * ('m, 'n, 'k) insertion
        -> 's value

  and _ evaluation =
    | Val : 's value -> 's evaluation
    | Realize : kinetic value -> potential evaluation
    | Unrealized : potential evaluation
    | Canonical : canonical -> potential evaluation

  and ('m, 'j, 'ij) data_args = {
    dim : 'm D.t;
    indices : (('m, normal) CubeOf.t, 'j, 'ij) Fillvec.t;
    constrs : (Constr.t, ('m, 'ij) dataconstr) Abwd.t;
    discrete : bool;
  }

  and canonical =
    | Data : ('m, 'j, 'ij) data_args -> canonical
    | Codata : {
        eta : potential eta;
        opacity : opacity;
        env : ('m, 'a) env;
        ins : ('mn, 'm, 'n) insertion;
        fields : (Field.t, ('a, 'n) codatafield) Abwd.t;
      }
        -> canonical

  and (_, _) dataconstr =
    | Dataconstr : {
        env : ('m, 'a) env;
        args : ('a, 'p, 'ap) Telescope.t;
        indices : (('ap, kinetic) term, 'ij) Vec.t;
      }
        -> ('m, 'ij) dataconstr

  and (_, _) codatafield = Codatafield : (('a, 'n) snoc, kinetic) term -> ('a, 'n) codatafield
  and normal = { tm : kinetic value; ty : kinetic value }

  and (_, _) env =
    | Emp : 'n D.t -> ('n, emp) env
    | LazyExt :
        ('n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, kinetic lazy_eval) CubeOf.t
        -> ('n, ('b, 'k) snoc) env
    | Ext :
        ('n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, kinetic value) CubeOf.t
        -> ('n, ('b, 'k) snoc) env
    | Act : ('n, 'b) env * ('m, 'n) op -> ('m, 'b) env
    | Permute : ('a, 'b) Tbwd.permute * ('n, 'b) env -> ('n, 'a) env
    | Shift : ('mn, 'b) env * ('m, 'n, 'mn) D.plus * ('n, 'b, 'nb) Plusmap.t -> ('m, 'nb) env

  and 's lazy_state =
    | Deferred_eval : ('m, 'b) env * ('b, 's) term * ('mn, 'm, 'n) insertion -> 's lazy_state
    | Deferred : (unit -> 's evaluation) * ('m, 'n) deg -> 's lazy_state
    | Ready : 's evaluation -> 's lazy_state

  and 's lazy_eval = 's lazy_state ref
end = struct
  (* Here is the recursive application of the functor Cube.  First we define a module to pass as its argument, with type defined to equal the yet-to-be-defined binder, referred to recursively. *)
  module BindFam = struct
    type ('k, 'b) t = ('k, kinetic) Value.binder
  end

  module BindCube = Cube (BindFam)

  (* The head of an elimination spine is either a variable or a constant.  We define this type to be parametrized over a pair of dummy indices indicating which it is, so that most of the time we can treat them equally by parametrizing over the index, but in some places (e.g. alignment) we can specify that only one kind of head is allowed. *)
  type head =
    (* A variable is determined by a De Bruijn LEVEL, and stores a neutral degeneracy applied to it. *)
    | Var : { level : level; deg : ('m, 'n) deg } -> head
    (* A constant also stores a dimension that it is substituted to and a neutral insertion applied to it.  Many constants are zero-dimensional, meaning that 'c' is zero, and hence a=b is just a dimension and the insertion is trivial.  The dimension of a constant is its dimension as a term standing on its own; so in particular if it has any parameters, then it belongs to an ordinary, 0-dimensional, pi-type and therefore is 0-dimensional, even if the eventual codomain of the pi-type is higher-dimensional.  Note also that when nonidentity insertions end up getting stored here, e.g. by Act, the dimension 'c gets extended as necessary; so it is always okay to create a constant with the (0,0,0) insertion to start with, even if you don't know what its actual dimension is. *)
    | Const : { name : Constant.t; ins : ('a, 'b, 'c) insertion } -> head
    (* A metavariable (i.e. flexible) head stores the metavariable along with a delayed substitution applied to it. *)
    | Meta : { meta : ('b, 's) Meta.t; env : ('m, 'b) env; ins : ('mn, 'm, 'n) insertion } -> head

  (* An application contains the data of an n-dimensional argument and its boundary, together with a neutral insertion applied outside that can't be pushed in.  This represents the *argument list* of a single application, not the function.  Thus, an application spine will be a head together with a list of apps. *)
  and 'n arg =
    | Arg of ('n, normal) CubeOf.t
    (* Fields don't store the dimension explicitly; the same field name is used at all dimensions.  But the dimension is implicitly stored in the insertion that appears on an "app". *)
    | Field of Field.t

  and app = App : 'n arg * ('m, 'n, 'k) insertion -> app

  (* Lambdas and Pis both bind a variable, along with its dependencies.  These are recorded as defunctionalized closures.  Since they are produced by higher-dimensional substitutions and operator actions, the dimension of the binder can be different than the dimension of the environment that closes its body.  Accordingly, in addition to the environment and degeneracy to close its body, we store information about how to map the eventual arguments into the bound variables in the body.  *)
  and (_, _) binder =
    | Bind : {
        env : ('m, 'a) env;
        body : (('a, 'n) snoc, 's) term;
        ins : ('mn, 'm, 'n) insertion;
      }
        -> ('mn, 's) binder

  (* A neutral has an "alignment".
     - A True neutral is an ordinary neutral that will never reduce further, such as an application of a variable or axiom, or of a defined constant with a neutral argument in a matching position.
     - A Chaotic neutral has a head defined by a case tree but isn't fully applied, so it might reduce further if it is applied to further arguments or field projections.  Thus it stores a value that should be either an abstraction or a struct, but does not test as equal to that value.
     - A Lawful neutral has a head defined by a case tree that will doesn't reduce, but if it is applied to enough arguments it obtains a specified behavior as a canonical type (datatype, record type, codatatype, function-type, etc.). *)
  and alignment =
    | True : alignment
    | Chaotic : potential value -> alignment
    | Lawful : canonical -> alignment

  (* An (m+n)-dimensional type is "instantiated" by applying it a "boundary tube" to get an m-dimensional type.  This operation is supposed to be functorial in dimensions, so in the normal forms we prevent it from being applied more than once in a row.  We have a separate class of "uninstantiated" values, and then every actual value is instantiated exactly once.  This means that even non-type neutrals must be "instantiated", albeit trivially. *)
  and uninst =
    | UU : 'n D.t -> uninst
    (* Pis must store not just the domain type but all its boundary types.  These domain and boundary types are not fully instantiated.  Note the codomains are stored in a cube of binders. *)
    | Pi : string option * ('k, kinetic value) CubeOf.t * ('k, unit) BindCube.t -> uninst
    (* A neutral is an application spine: a head with a list of applications.  Note that when we inject it into 'value' with Uninst below, it also stores its type (as do all the other uninsts).  It also has an alignment. *)
    | Neu : { head : head; args : app Bwd.t; alignment : alignment } -> uninst

  and _ value =
    (* An uninstantiated term, together with its type.  The 0-dimensional universe is morally an infinite data structure Uninst (UU 0, (Uninst (UU 0, Uninst (UU 0, ... )))), so we make the type lazy. *)
    | Uninst : uninst * kinetic value Lazy.t -> kinetic value
    (* A term with some nonzero instantiation *)
    | Inst : {
        (* The uninstantiated term being instantiated *)
        tm : uninst;
        (* Require at least one dimension to be instantiated *)
        dim : 'k D.pos;
        (* The arguments for a tube of some dimensions *)
        args : ('n, 'k, 'nk, normal) TubeOf.t;
        (* The types of the arguments remaining to be supplied.  In other words, the type *of* this instantiation is "Inst (UU k, tys)". *)
        tys : (D.zero, 'n, 'n, kinetic value) TubeOf.t;
      }
        -> kinetic value
    (* A constructor has a name, a dimension, and a list of arguments of that dimension.  It must always be applied to the correct number of arguments (otherwise it can be eta-expanded).  It doesn't have an outer insertion because a primitive datatype is always 0-dimensional (it has higher-dimensional versions, but degeneracies can always be pushed inside these).  *)
    | Constr : Constr.t * 'n D.t * ('n, kinetic value) CubeOf.t list -> kinetic value
    (* Lambda-abstractions are never types, so they can never be nontrivially instantiated.  Thus we may as well make them values directly. *)
    | Lam : 'k variables * ('k, 's) binder -> 's value
    (* The same is true for anonymous structs.  These have to store an insertion outside, like an application, to deal with higher-dimensional record types like Gel (here 'k would be the Gel dimension).  We also remember which fields are labeled, for readback purposes.  We store the value of each field lazily, so that corecursive definitions don't try to compute an entire infinite structure.  And since in the non-kinetic case, evaluation can produce more data than just a term (e.g. whether a case tree has yet reached a leaf), what we store lazily is the result of evaluation. *)
    | Struct :
        (Field.t, 's lazy_eval * [ `Labeled | `Unlabeled ]) Abwd.t * ('m, 'n, 'k) insertion
        -> 's value

  (* This is the result of evaluating a term with a given kind of energy.  Evaluating a kinetic term just produces a (kinetic) value, whereas evaluating a potential term might be a potential value (waiting for more arguments), or else the information that the case tree has reached a leaf and the resulting kinetic value or canonical type, or else the information that the case tree is permanently stuck.  *)
  and _ evaluation =
    (* When 's = potential, a Val means the case tree is not yet fully applied; while when 's = kinetic, it is the only possible kind of result.  Collapsing these two together seems to unify the code for Lam and Struct as much as possible. *)
    | Val : 's value -> 's evaluation
    | Realize : kinetic value -> potential evaluation
    | Unrealized : potential evaluation
    | Canonical : canonical -> potential evaluation

  (* We define a named record type to encapsulate the arguments of Data, rather than using an inline one, so that we can bind its existential variables (https://discuss.ocaml.org/t/annotating-by-an-existential-type/14721).  A datatype value has a vector of some indices to which it has been applied, the number of remaining indices to which it must be applied, and a family of constructors.  Each constructor stores the telescope of types of its arguments, as a closure, and the index values as function values taking its arguments. *)
  and ('m, 'j, 'ij) data_args = {
    dim : 'm D.t;
    indices : (('m, normal) CubeOf.t, 'j, 'ij) Fillvec.t;
    constrs : (Constr.t, ('m, 'ij) dataconstr) Abwd.t;
    discrete : bool;
  }

  (* A canonical type value is either a datatype or a codatatype/record. *)
  and canonical =
    | Data : ('m, 'j, 'ij) data_args -> canonical
    (* A codatatype value has an eta flag, an environment that it was evaluated at, an insertion that relates its intrinsic dimension (such as for Gel) to the dimension it was evaluated at, and its fields as unevaluted terms that depend on one additional variable belonging to the codatatype itself (usually through its previous fields).  Note that combining env, ins, and any of the field terms produces the data of a binder, so we can think of this as a family of binders,  one for each field, that share the same environment and insertion. *)
    | Codata : {
        eta : potential eta;
        opacity : opacity;
        env : ('m, 'a) env;
        ins : ('mn, 'm, 'n) insertion;
        (* TODO: When it's used, this should really be a forwards list.  But it's naturally constructed backwards, and it has to be used *as* it's being constructed when typechecking the later terms. *)
        fields : (Field.t, ('a, 'n) codatafield) Abwd.t;
      }
        -> canonical

  and (_, _) dataconstr =
    | Dataconstr : {
        env : ('m, 'a) env;
        args : ('a, 'p, 'ap) Telescope.t;
        indices : (('ap, kinetic) term, 'ij) Vec.t;
      }
        -> ('m, 'ij) dataconstr

  and (_, _) codatafield = Codatafield : (('a, 'n) snoc, kinetic) term -> ('a, 'n) codatafield

  (* A "normal form" is a value paired with its type.  The type is used for eta-expansion and equality-checking. *)
  and normal = { tm : kinetic value; ty : kinetic value }

  (* An "environment" is a context morphism *from* a De Bruijn LEVEL context *to* a (typechecked) De Bruijn INDEX context.  Specifically, an ('n, 'a) env is an 'n-dimensional substitution from a level context to an index context indexed by the hctx 'a.  Since the index context could have some variables that are labeled by integers together with faces, the values also have to allow that. *)
  and (_, _) env =
    | Emp : 'n D.t -> ('n, emp) env
    (* The (n+k)-cube here is morally a k-cube of n-cubes, representing a k-dimensional "cube variable" consisting of some number of "real" variables indexed by the faces of a k-cube, each of which has an n-cube of values representing a value and its boundaries.  But this contains the same data as an (n+k)-cube since a strict face of (n+k) decomposes uniquely as a strict face of n plus a strict face of k, and it seems to be more convenient to store it as a single (n+k)-cube. *)
    | LazyExt :
        ('n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, kinetic lazy_eval) CubeOf.t
        -> ('n, ('b, 'k) snoc) env
    | Ext :
        ('n, 'b) env * ('n, 'k, 'nk) D.plus * ('nk, kinetic value) CubeOf.t
        -> ('n, ('b, 'k) snoc) env
    | Act : ('n, 'b) env * ('m, 'n) op -> ('m, 'b) env
    | Permute : ('a, 'b) Tbwd.permute * ('n, 'b) env -> ('n, 'a) env
    | Shift : ('mn, 'b) env * ('m, 'n, 'mn) D.plus * ('n, 'b, 'nb) Plusmap.t -> ('m, 'nb) env

  (* An 's lazy_eval behaves from the outside like an 's evaluation Lazy.t.  But internally, instead of storing an arbitrary thunk, it stores a term and an environment in which to evaluate it (plus an outer insertion that can't be pushed into the environment).  This allows it to accept degeneracy actions and incorporate them into the environment, so that when it's eventually forced the term only has to be traversed once.  If it's already been forced, it can still accept multiple degeneracy actions and wait to traverse the value until it's forced again.  *)
  and 's lazy_state =
    | Deferred_eval : ('m, 'b) env * ('b, 's) term * ('mn, 'm, 'n) insertion -> 's lazy_state
    | Deferred : (unit -> 's evaluation) * ('m, 'n) deg -> 's lazy_state
    | Ready : 's evaluation -> 's lazy_state

  and 's lazy_eval = 's lazy_state ref
end

open Value

(* Given a De Bruijn level and a type, build the variable of that level having that type. *)
let var : level -> kinetic value -> kinetic value =
 fun level ty ->
  Uninst
    ( Neu { head = Var { level; deg = id_deg D.zero }; args = Emp; alignment = True },
      Lazy.from_val ty )

(* Every context morphism has a valid dimension. *)
let rec dim_env : type n b. (n, b) env -> n D.t = function
  | Emp n -> n
  | Ext (e, _, _) -> dim_env e
  | LazyExt (e, _, _) -> dim_env e
  | Act (_, op) -> dom_op op
  | Permute (_, e) -> dim_env e
  | Shift (e, mn, _) -> D.plus_left mn (dim_env e)

(* And likewise every binder *)
let dim_binder : type m s. (m, s) binder -> m D.t = function
  | Bind b -> dom_ins b.ins

(* The length of an environment is a tbwd of dimensions. *)
let rec length_env : type n b. (n, b) env -> b Plusmap.OfDom.t = function
  | Emp _ -> Of_emp
  | Ext (env, nk, _) -> Of_snoc (length_env env, D.plus_right nk)
  | LazyExt (env, nk, _) -> Of_snoc (length_env env, D.plus_right nk)
  | Act (env, _) -> length_env env
  | Permute (p, env) -> Plusmap.OfDom.permute p (length_env env)
  | Shift (env, mn, nb) -> Plusmap.out (D.plus_right mn) (length_env env) nb

(* Smart constructor that composes actions and cancels identities *)
let rec act_env : type m n b. (n, b) env -> (m, n) op -> (m, b) env =
 fun env s ->
  match env with
  | Act (env, s') -> act_env env (comp_op s' s)
  | _ -> (
      match is_id_op s with
      | Some Eq -> env
      | None -> Act (env, s))

(* Create a lazy evaluation *)
let lazy_eval : type n b s. (n, b) env -> (b, s) term -> s lazy_eval =
 fun env tm -> ref (Deferred_eval (env, tm, ins_zero (dim_env env)))

let defer : type s. (unit -> s evaluation) -> s lazy_eval =
 fun tm -> ref (Deferred (tm, id_deg D.zero))

(* Project out a cube or tube of values from a cube or tube of normals *)
let val_of_norm_cube : type n. (n, normal) CubeOf.t -> (n, kinetic value) CubeOf.t =
 fun arg -> CubeOf.mmap { map = (fun _ [ { tm; ty = _ } ] -> tm) } [ arg ]

let val_of_norm_tube :
    type n k nk. (n, k, nk, normal) TubeOf.t -> (n, k, nk, kinetic value) TubeOf.t =
 fun arg -> TubeOf.mmap { map = (fun _ [ { tm; ty = _ } ] -> tm) } [ arg ]

(* Remove an entry from an environment *)
let rec remove_env : type a k b n. (n, b) env -> (a, k, b) Tbwd.insert -> (n, a) env =
 fun env v ->
  match (env, v) with
  | Emp _, _ -> .
  | Act (env, op), _ -> Act (remove_env env v, op)
  | Permute (p, env), _ ->
      let (Permute_insert (v', p')) = Tbwd.permute_insert v p in
      Permute (p', remove_env env v')
  | Ext (env, nk, xs), Later v -> Ext (remove_env env v, nk, xs)
  | LazyExt (env, nk, xs), Later v -> LazyExt (remove_env env v, nk, xs)
  | Ext (env, _, _), Now -> env
  | LazyExt (env, _, _), Now -> env
  | Shift (env, mn, nb), _ ->
      let (Unmap_insert (_, v', na)) = Plusmap.unmap_insert v nb in
      Shift (remove_env env v', mn, na)

module Discreteness = Algaeff.Reader.Make (Bool)

module Discrete = Algaeff.State.Make (struct
  type t = bool Constant.Map.t
end)

let is_discrete : kinetic value -> bool = function
  | Uninst (tm, _) | Inst { tm; _ } -> (
      match tm with
      | Neu { alignment = Lawful (Data { discrete = true; _ }); _ } -> true
      (* TODO: In a mutual block, this is not the correct test: it considers all the mutually defined types to be "discrete" even if they don't later turn out to be.  At present we are disallowing discreteness for all mutual families. *)
      | Neu { head = Const { name; _ }; _ } when Constant.Map.mem name (Discrete.get ()) -> true
      (* In theory, pi-types with discrete codomain, and record types with discrete fields, could also be discrete.  But that would be trickier to check as it would require evaluating their codomain and fields under binders, and eta-conversion for those types should implement direct discreteness automatically.  So the only thing we're missing is that they can't appear as arguments to a constructor of some other discrete datatype. *)
      | _ -> false)
  | _ -> false

let const_head_of : kinetic value -> Constant.t option = function
  | Uninst (tm, _) | Inst { tm; _ } -> (
      match tm with
      | Neu { head = Const { name; _ }; _ } -> Some name
      | _ -> None)
  | _ -> None
