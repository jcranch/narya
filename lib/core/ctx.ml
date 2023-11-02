(* This module should not be opened, but used qualified *)

open Util
open Dim
open Term
open Value
open Hctx
open Reporter

(* To first approximation, a context is a list of variables, each of which has a value that is a normal form.  Often the "value" of a variable will just be ITSELF, represented by a De Bruijn LEVEL, together of course with its type.  This can then appear in the types of later variables.  In particular, the LENGTH of this context, which is its type parameter as a type-level nat, is the current De Bruijn LEVEL for new variables to be added.

   We can look up the INDEX of a TERM VARIABLE into this Bwv to get its type.  This operation is statically guaranteed to succeed, since all De Bruijn indices are intrinsically well-scoped.

   We can also look up the LEVEL of a VALUE VARIABLE to find its corresponding term-variable index (and we do this during readback).  However, this operation is not statically guaranteed to succeed.  Indeed, in some cases it we intend for it to fail, e.g. during an occurs-check.  To enable this operation, we separately store for each index variable its corresponding level, if any, in addition to its value.  (If it is let-bound to a value, then it has no associated level variable.) *)

(* To second approximation, a "context" is actually a WEAKENING SUBSTITUTION from one De Bruijn INDEX context to another.  The index variables that arise from parsing are counted based on the number of textually in-scope variables, but internally there may be variables other than these, for instance if a pattern binds some arguments implicitly.  Thus, an element of (a, b) Ctx.t is actually a length-b context in which only a of the variables are "visible".  We then use b for counting the next De Bruijn LEVEL to create, and for De Bruijn INDICES IN CHECKED TERMS, as well as for readback.  However, since the user can only refer to a of the variables, and the parser doesn't know about invisible variables (they are determined by semantic considerations, e.g. implicit arguments of constructors in match patterns), we use a for De Bruijn INDICES IN RAW TERMS.  This means the index of a variable can change when it is typechecked, but our intrinsically well-scoped indices manage this issue smoothly, ensuring that the correct kind of index is always used in the correct place. *)

(* To third approximation, a context is not a flat list of variables, but a list of "cubes" of variables.  Frequently when introducing variables, we introduce a whole cube of them (e.g. when abstracting into a higher-dimensional pi-type, or pattern-matching against a higher-dimensional datatype), and we avoid "linearizing" these variables as much as possible.  Thus, index variables are not just a (well-scoped) natural number, but are paired with a dimension and a strict face of that dimension, and variables are stored in cubes.

   More precisely, the RAW parameter 'a is a simple type-level natural number, since the parser can't tell what dimensions things have, and a raw index variable is paired with a face of an arbitrary dimension corresponding to how the user used it.  However, the CHECKED parameter 'b is actually a type-level list of dimensions (an "hctx"), and a checked index variable is paired with a face *of the corresponding dimension*.  For level variables we use a pair of integers: one for the position in the context, and the other that counts through the variables in each cube.  (Since levels are only ever compared for equality, the ordering of the latter numbers doesn't matter.) *)

type (_, _) t =
  | Emp : (N.zero, emp) t
  | Vis : ('a, 'b) t * ('n, level option * normal) CubeOf.t -> ('a N.suc, ('b, 'n) ext) t
  | Invis : ('a, 'b) t * ('n, level option * normal) CubeOf.t -> ('a, ('b, 'n) ext) t
  | Split :
      ('a, 'b) t
      * ('n, 'f) count_faces
      * ('a, 'f, 'af) N.plus
      * ('n, level option * normal) CubeOf.t
      -> ('af, ('b, 'n) ext) t

let vis : type a b n. (a, b) t -> (n, level option * normal) CubeOf.t -> (a N.suc, (b, n) ext) t =
 fun ctx vars -> Vis (ctx, vars)

let invis : type a b n. (a, b) t -> (n, level option * normal) CubeOf.t -> (a, (b, n) ext) t =
 fun ctx vars -> Invis (ctx, vars)

let split :
    type a b n f af.
    (a, b) t ->
    (n, f) count_faces ->
    (a, f, af) N.plus ->
    (n, level option * normal) CubeOf.t ->
    (af, (b, n) ext) t =
 fun ctx af pf vars -> Split (ctx, af, pf, vars)

let rec length : type a b. (a, b) t -> int = function
  | Emp -> 0
  | Vis (ctx, _) -> length ctx + 1
  | Invis (ctx, _) -> length ctx + 1
  | Split (ctx, _, _, _) -> length ctx + 1

let empty : (N.zero, emp) t = Emp

(* When we look up a visible variable in a context, we find the level (if any), the value, and the corresponding possibly-invisible variable. *)
let rec lookup : type a b n. (a, b) t -> a Raw.index -> level option * normal * b index =
 fun ctx k ->
  match ctx with
  | Emp -> (
      match k with
      | _ -> .)
  | Vis (ctx, x) -> (
      match k with
      | Top, None ->
          (* If the raw index variable doesn't have a specified face, it means the top face. *)
          let n = CubeOf.dim x in
          let j, x = CubeOf.find_top x in
          (j, x, Top (id_sface n))
      | Top, Some (Any_sface fa) -> (
          match compare (cod_sface fa) (CubeOf.dim x) with
          | Eq ->
              let j, x = CubeOf.find x fa in
              (j, x, Top fa)
          | Neq -> fatal (Invalid_variable_face (CubeOf.dim x, fa)))
      | Pop k, fa ->
          let j, x, v = lookup ctx (k, fa) in
          (j, x, Pop v))
  | Invis (ctx, _) ->
      let j, x, v = lookup ctx k in
      (j, x, Pop v)
  | Split (ctx, af, pf, xs) -> lookup_face pf (sfaces af) ctx xs k

and lookup_face :
    type a f af b n.
    (a, f, af) N.plus ->
    (n sface_of, f) Bwv.t ->
    (a, b) t ->
    (n, level option * normal) CubeOf.t ->
    af Raw.index ->
    level option * normal * (b, n) ext index =
 fun pf sf ctx xs k ->
  match (pf, sf) with
  | Zero, Emp ->
      let i, x, v = lookup ctx k in
      (i, x, Pop v)
  | Suc pf, Snoc (sf, SFace_of fb) -> (
      match k with
      | Pop k, fa -> lookup_face pf sf ctx xs (k, fa)
      | Top, None ->
          let i, x = CubeOf.find xs fb in
          (i, x, Top fb)
      | Top, Some (Any_sface fa) -> fatal (Invalid_variable_face (D.zero, fa)))

(* We can also look up a possibly-invisible variable in a context, in which case the only things to return are the possible-level and value. *)
let rec lookup_invis : type a b. (a, b) t -> b index -> level option * normal =
 fun ctx k ->
  match ctx with
  | Emp -> (
      match k with
      | _ -> .)
  | Vis (ctx, x) -> (
      match k with
      | Top fa -> CubeOf.find x fa
      | Pop k -> lookup_invis ctx k)
  | Invis (ctx, x) -> (
      match k with
      | Top fa -> CubeOf.find x fa
      | Pop k -> lookup_invis ctx k)
  | Split (ctx, _, _, x) -> (
      match k with
      | Top fa -> CubeOf.find x fa
      | Pop k -> lookup_invis ctx k)

(* Look up a De Bruijn level in a context and find the corresponding possibly-invisible index, if one exists. *)
let rec find_level : type a b. (a, b) t -> level -> b index option =
 fun ctx i ->
  match ctx with
  | Emp -> None
  | Vis (ctx, vars) -> find_level_in_cube ctx vars i
  | Invis (ctx, vars) -> find_level_in_cube ctx vars i
  | Split (ctx, _, _, vars) -> find_level_in_cube ctx vars i

and find_level_in_cube :
    type a b n. (a, b) t -> (n, level option * normal) CubeOf.t -> level -> (b, n) ext index option
    =
 fun ctx vars i ->
  let open CubeOf.Monadic (Monad.State (struct
    type t = (b, n) ext index option
  end)) in
  match
    miterM
      { it = (fun fa [ (j, _) ] s -> if j = Some i then ((), Some (Top fa)) else ((), s)) }
      [ vars ] None
  with
  | (), Some v -> Some v
  | (), None -> Option.map (fun v -> Pop v) (find_level ctx i)

(* Every context has an underlying environment that substitutes each (level) variable for itself (index).  This environment ALWAYS HAS DIMENSION ZERO, and therefore in particular the variables don't need to come with any boundaries. *)
let rec env : type a b. (a, b) t -> (D.zero, b) env = function
  | Emp -> Emp D.zero
  | Vis (ctx, v) ->
      Ext (env ctx, CubeOf.mmap { map = (fun _ [ x ] -> CubeOf.singleton (snd x).tm) } [ v ])
  | Invis (ctx, v) ->
      Ext (env ctx, CubeOf.mmap { map = (fun _ [ x ] -> CubeOf.singleton (snd x).tm) } [ v ])
  | Split (ctx, _, _, v) ->
      Ext (env ctx, CubeOf.mmap { map = (fun _ [ x ] -> CubeOf.singleton (snd x).tm) } [ v ])

(* Evaluate a term in (the environment of) a context.  Thus, replace its De Bruijn indices with De Bruijn levels, and substitute the values of variables with definitions. *)
let eval : type a b. (a, b) t -> b term -> value = fun ctx tm -> Norm.eval (env ctx) tm

(* Extend a context by one new variable, without a value but with an assigned type. *)
let ext : type a b. (a, b) t -> value -> (a N.suc, (b, D.zero) ext) t =
 fun ctx ty ->
  let n = length ctx in
  Vis (ctx, CubeOf.singleton (Some (n, 0), { tm = var (n, 0) ty; ty }))

(* Extend a context by one new variable with an assigned value. *)
let ext_let : type a b. (a, b) t -> normal -> (a N.suc, (b, D.zero) ext) t =
 fun ctx v -> Vis (ctx, CubeOf.singleton (None, v))

(* Extend a context by a finite number of new variables, whose types and values are specified in a Bwv, and some last number of which are visible. *)
let rec exts :
    type a b1 b2 b ab2 c d db.
    (a, d) t ->
    (b1, b2, b) N.plus ->
    (a, b2, ab2) N.plus ->
    (d, b, db, D.zero) exts ->
    (level option * normal, b) Bwv.t ->
    (ab2, db) t =
 fun ctx bb ab db keys ->
  match (bb, ab, db, keys) with
  | Zero, Zero, Zero, Emp -> ctx
  | Suc bb, Suc ab, Suc db, Snoc (keys, key) ->
      let newctx = exts ctx bb ab db keys in
      Vis (newctx, CubeOf.singleton key)
  | Zero, Zero, Suc db, Snoc (keys, key) ->
      let newctx = exts ctx Zero ab db keys in
      Invis (newctx, CubeOf.singleton key)

(* Extend a context by a finite number of invisible variables. *)
let rec ext_invis :
    type a b1 b2 b ab2 c d db.
    (a, d) t -> (d, b, db, D.zero) exts -> (level option * normal, b) Bwv.t -> (a, db) t =
 fun ctx db keys ->
  match (db, keys) with
  | Zero, Emp -> ctx
  | Suc db, Snoc (keys, key) ->
      let newctx = ext_invis ctx db keys in
      Invis (newctx, CubeOf.singleton key)

(* Extend a context by a finite number of cubes of new visible variables at some dimension, with boundaries, whose types are specified by the evaluation of some telescope in some (possibly higher-dimensional) environment (and hence may depend on the earlier ones).  Also return the new variables in a Bwd of Cubes, and the new environment extended by the *top-dimensional variables only*. *)
let ext_tel :
    type a b c ac bc e ec n.
    (a, e) t ->
    (n, b) env ->
    (b, c, bc) Telescope.t ->
    (a, c, ac) N.plus ->
    (e, c, ec, n) exts ->
    (ac, ec) t * (n, bc) env * ((n, value) CubeOf.t, c) Bwv.t =
 fun ctx env tel ac ec ->
  let rec ext_tel :
      type a b c ac bc d dc e ec.
      (a, e) t ->
      (n, b) env ->
      (b, c, bc) Telescope.t ->
      (a, c, ac) N.plus ->
      (e, c, ec, n) exts ->
      (d, c, dc) N.plus ->
      ((n, value) CubeOf.t, d) Bwv.t ->
      (ac, ec) t * (n, bc) env * ((n, value) CubeOf.t, dc) Bwv.t =
   fun ctx env tel ac ec dc vars ->
    match (tel, ac, dc) with
    | Emp, Zero, Zero ->
        let Zero, Zero = (ac, ec) in
        (ctx, env, vars)
    | Ext (rty, rest), Suc _, Suc _ ->
        let newvars, newnfs =
          dom_vars (length ctx)
            (CubeOf.build (dim_env env)
               { build = (fun fa -> Norm.eval (Act (env, op_of_sface fa)) rty) }) in
        let newctx = Vis (ctx, newnfs) in
        ext_tel newctx
          (Ext (env, CubeOf.singleton newvars))
          rest (N.suc_plus'' ac) (exts_suc'' ec) (N.suc_plus'' dc)
          (Snoc (vars, newvars)) in
  ext_tel ctx env tel ac ec (N.zero_plus (N.plus_right ac)) Emp

(* Let-bind some of the variables in a context *)

let bind_some_cube :
    type n.
    (level -> normal option) ->
    (n, level option * normal) CubeOf.t ->
    (n, level option * normal) CubeOf.t =
 fun binder xs ->
  CubeOf.mmap
    {
      map =
        (fun _ [ (i, x) ] ->
          match i with
          | None -> (i, x)
          | Some i -> (
              match binder i with
              | None -> (Some i, x)
              | Some t -> (None, t)));
    }
    [ xs ]

let rec bind_some : type a e n. (level -> normal option) -> (a, e) t -> (a, e) t =
 fun binder ctx ->
  match ctx with
  | Emp -> Emp
  | Vis (ctx, xs) -> Vis (bind_some binder ctx, bind_some_cube binder xs)
  | Invis (ctx, xs) -> Invis (bind_some binder ctx, bind_some_cube binder xs)
  | Split (ctx, af, pf, xs) -> Split (bind_some binder ctx, af, pf, bind_some_cube binder xs)
