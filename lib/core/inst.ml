open Util
open Reporter
open Dim
open Syntax
open Value
open Bwd
open Hctx

(* Ensure that a value is a fully instantiated type, and extract its relevant pieces.  In most situations, the failure of this is a bug, but we allow the caller to specify it differently, since during typechecking it could be a user error. *)
type full_inst = Fullinst : uninst * (D.zero, 'k, 'k, normal) TubeOf.t -> full_inst

let full_inst ?severity (ty : value) (err : string) : full_inst =
  match ty with
  (* Since we expect fully instantiated types, in the uninstantiated case the dimension must be zero. *)
  | Uninst (ty, (lazy (Uninst (UU n, _)))) -> (
      match compare n D.zero with
      | Eq -> Fullinst (ty, TubeOf.empty D.zero)
      | Neq -> fatal ?severity (Type_not_fully_instantiated err))
  | Inst { tm = ty; dim = _; args; tys = _ } -> (
      match compare (TubeOf.uninst args) D.zero with
      | Eq ->
          let Eq = D.plus_uniq (TubeOf.plus args) (D.zero_plus (TubeOf.inst args)) in
          Fullinst (ty, args)
      | Neq -> fatal ?severity (Type_not_fully_instantiated err))
  | _ -> fatal ?severity (Type_not_fully_instantiated err)

(* Instantiate an arbitrary value, combining tubes. *)
let rec inst : type m n mn. value -> (m, n, mn, normal) TubeOf.t -> value =
 fun tm args2 ->
  let n = TubeOf.inst args2 in
  match D.compare_zero n with
  | Zero -> tm
  | Pos dim2 -> (
      match tm with
      | Inst { tm; dim = _; args = args1; tys = tys1 } -> (
          match compare (TubeOf.out args2) (TubeOf.uninst args1) with
          | Neq ->
              fatal
                (Dimension_mismatch
                   ( "instantiating a partially instantiated type",
                     TubeOf.out args2,
                     TubeOf.uninst args1 ))
          | Eq ->
              let (Plus nk) = D.plus (TubeOf.inst args1) in
              let args = TubeOf.plus_tube nk args1 args2 in
              let tys = TubeOf.middle (D.zero_plus (TubeOf.uninst args2)) (TubeOf.plus args2) tys1 in
              let tys = inst_args args2 tys in
              Inst { tm; dim = D.pos_plus dim2 nk; args; tys })
      | Uninst (tm, (lazy ty)) -> (
          (* In this case, the type must be a fully instantiated universe of the right dimension, and the remaining types come from its instantiation arguments. *)
          let (Fullinst (ty, tyargs)) = full_inst ty "inst" in
          match ty with
          | UU k -> (
              match (compare k (TubeOf.out args2), compare k (TubeOf.out tyargs)) with
              | Neq, _ ->
                  fatal
                    (Dimension_mismatch ("instantiating an uninstantiated type", k, TubeOf.out args2))
              | _, Neq ->
                  fatal
                    (Dimension_mismatch
                       ("instantiating an uninstantiated type", k, TubeOf.out tyargs))
              | Eq, Eq ->
                  let tys =
                    val_of_norm_tube
                      (TubeOf.middle (D.zero_plus (TubeOf.uninst args2)) (TubeOf.plus args2) tyargs)
                  in
                  let tys = inst_args args2 tys in
                  Inst { tm; dim = dim2; args = args2; tys })
          | _ -> fatal (Anomaly "can't instantiate non-type"))
      | Lam _ -> fatal (Anomaly "can't instantiate lambda-abstraction")
      | Struct _ -> fatal (Anomaly "can't instantiate struct")
      | Constr _ -> fatal (Anomaly "can't instantiate constructor"))

and inst_args :
    type m n mn.
    (m, n, mn, normal) TubeOf.t -> (D.zero, m, m, value) TubeOf.t -> (D.zero, m, m, value) TubeOf.t
    =
 fun args2 tys ->
  let n = TubeOf.inst args2 in
  TubeOf.mmap
    {
      map =
        (fun fe [ ty ] ->
          let j = dom_tface fe in
          let (Plus jn) = D.plus n in
          let args =
            TubeOf.build j jn
              {
                build =
                  (fun fa ->
                    let (PFace_of_plus (pq, fc, fd)) = pface_of_plus fa in
                    let fec = comp_sface (sface_of_tface fe) fc in
                    let fecd = sface_plus_pface fec (TubeOf.plus args2) pq fd in
                    TubeOf.find args2 fecd);
              } in
          inst ty args);
    }
    [ tys ]

(* Given a *type*, hence an element of a fully instantiated universe, extract the arguments of the instantiation of that universe.  These were stored in the extra arguments of Uninst and Inst. *)
type inst_tys = Inst_tys : (D.zero, 'n, 'n, value) TubeOf.t -> inst_tys

let inst_tys : value -> inst_tys = function
  | Uninst (_, (lazy (Uninst (UU z, _)))) -> (
      match compare z D.zero with
      | Eq -> Inst_tys (TubeOf.empty D.zero)
      | Neq -> fatal (Anomaly "higher universe must be instantiated to be a type"))
  | Uninst (_, (lazy (Inst { tm = UU _; dim = _; args = tys; tys = _ }))) -> (
      match compare (TubeOf.uninst tys) D.zero with
      | Eq ->
          let Eq = D.plus_uniq (D.zero_plus (TubeOf.inst tys)) (TubeOf.plus tys) in
          Inst_tys (val_of_norm_tube tys)
      | Neq -> fatal (Anomaly "universe must be fully instantiated to be a type"))
  | Inst { tm = _; dim = _; args = _; tys } -> Inst_tys tys
  | _ -> fatal (Anomaly "invalid type, has no instantiation arguments")

(* Given two families of values, the second intended to be the types of the other, annotate the former by instantiations of the latter to make them into normals. *)
and norm_of_vals : type k. (k, value) CubeOf.t -> (k, value) CubeOf.t -> (k, normal) CubeOf.t =
 fun tms tys ->
  (* Since we have to instantiate the types at the *normal* version of the terms, which is what we are computing, we also add the results to a hashtable as we create them so we can access them randomly later. *)
  let new_tm_tbl = Hashtbl.create 10 in
  let new_tms =
    CubeOf.mmap
      {
        map =
          (fun fab [ tm; ty ] ->
            let args =
              TubeOf.build D.zero
                (D.zero_plus (dom_sface fab))
                {
                  build =
                    (fun fc ->
                      Hashtbl.find new_tm_tbl (SFace_of (comp_sface fab (sface_of_tface fc))));
                } in
            let ty = inst ty args in
            let newtm = { tm; ty } in
            Hashtbl.add new_tm_tbl (SFace_of fab) newtm;
            newtm);
      }
      [ tms; tys ] in
  new_tms

(* Assemble an environment from a Bwv of values. *)
let rec env_of_bwv :
    type n a ea.
    n D.t -> ((n, normal) CubeOf.t, a) Bwv.t -> (emp, a, ea, D.zero) exts -> (n, ea) env =
 fun n xs ea ->
  match (xs, ea) with
  | Emp, Zero -> Emp n
  | Snoc (xs, x), Suc ea -> Ext (env_of_bwv n xs ea, CubeOf.singleton (val_of_norm_cube x))

(* Require that the supplied Bwd contains exactly b arguments, rearrange each mn-cube argument into an n-cube of m-cubes, and add all of them to the given environment. *)
let rec take_args :
    type m n mn a b ab.
    (m, a) env ->
    (m, n, mn) D.plus ->
    (mn, value) CubeOf.t Bwd.t ->
    (a, b, ab, n) exts ->
    (m, ab) env =
 fun env mn dargs plus ->
  let m = dim_env env in
  let n = D.plus_right mn in
  match (dargs, plus) with
  | Emp, Zero -> env
  | Snoc (args, arg), Suc plus ->
      let env = take_args env mn args plus in
      Ext
        ( env,
          CubeOf.build n
            {
              build =
                (fun fb ->
                  CubeOf.build m
                    {
                      build =
                        (fun fa ->
                          let (Plus jk) = D.plus (dom_sface fb) in
                          let fab = sface_plus_sface fa mn jk fb in
                          CubeOf.find arg fab);
                    });
            } )
  | _ -> fatal (Anomaly "wrong number of arguments in argument list")

(* A version of take_args that takes some number of actual arguments without insertions from a Bwd, adds a specified number of them to the environment, and returns the others in a Bwv of specified length.  *)
let rec take_canonical_args :
    type n a b ab c.
    (n, a) env ->
    (n, normal) CubeOf.t Bwd.t ->
    (a, b, ab, D.zero) exts ->
    c N.t ->
    (n, ab) env * ((n, normal) CubeOf.t, c) Bwv.t =
 fun env args ab c ->
  match c with
  | Nat (Suc c) -> (
      match args with
      | Snoc (args, arg) ->
          let env, rest = take_canonical_args env args ab (Nat c) in
          (env, Snoc (rest, arg))
      | Emp -> fatal (Anomaly "not enough arguments in canonical argument list"))
  | Nat Zero -> (
      match (args, ab) with
      | Snoc (args, arg), Suc ab ->
          let env, Emp = take_canonical_args env args ab (Nat Zero) in
          (Ext (env, CubeOf.singleton (val_of_norm_cube arg)), Emp)
      | Emp, Zero -> (env, Emp)
      | _ -> fatal (Anomaly "wrong number of arguments in canonical argument list"))

(* The universe of any dimension belongs to an instantiation of itself.  Note that the result is not itself a type (i.e. in the 0-dimensional universe) unless n=0. *)
let rec universe : type n. n D.t -> value =
 fun n ->
  match D.compare_zero n with
  | Zero ->
      (* Without lazy this would be an infinite loop *)
      Uninst (UU D.zero, lazy (universe D.zero))
  | Pos n' ->
      Uninst
        ( UU n,
          lazy
            (let args =
               TubeOf.build D.zero (D.zero_plus n)
                 {
                   build =
                     (fun fa ->
                       let m = dom_tface fa in
                       universe_nf m);
                 } in
             Inst { tm = UU n; dim = n'; args; tys = TubeOf.empty D.zero }) )

and universe_nf : type n. n D.t -> normal =
 fun n ->
  let uun = universe n in
  match uun with
  | Uninst (_, (lazy uunty)) -> { tm = uun; ty = uunty }
  | _ -> fatal (Anomaly "impossible result from universe")

(* Given a type belonging to the m+n dimensional universe instantiated at tyargs, compute the instantiation of the m-dimensional universe that its instantiation belongs to. *)
let rec tyof_inst :
    type m n mn. (D.zero, mn, mn, normal) TubeOf.t -> (m, n, mn, normal) TubeOf.t -> value =
 fun tyargs eargs ->
  let m = TubeOf.uninst eargs in
  let n = TubeOf.inst eargs in
  let mn = TubeOf.plus eargs in
  let margs =
    TubeOf.build D.zero (D.zero_plus m)
      {
        build =
          (fun fe ->
            let j = dom_tface fe in
            let (Plus jn) = D.plus (D.plus_right mn) in
            let jnargs =
              TubeOf.build j jn
                {
                  build =
                    (fun fa ->
                      let (PFace_of_plus (pq, fc, fd)) = pface_of_plus fa in
                      TubeOf.find eargs
                        (sface_plus_tface
                           (comp_sface (sface_of_tface fe) fc)
                           (D.plus_zero m) mn pq fd));
                } in
            (* We need to able to look things up in tyargs that are indexed by a composite of tfaces.  TODO: Actually define composites of tfaces, with each other and/or with sfaces on one side or the other, so that this works.  For the moment, we punt and use a hashtbl indexed by sfaces. *)
            let tyargtbl = Hashtbl.create 10 in
            TubeOf.miter
              { it = (fun fa [ ty ] -> Hashtbl.add tyargtbl (SFace_of (sface_of_tface fa)) ty) }
              [ tyargs ];
            let jntyargs =
              TubeOf.build D.zero
                (D.zero_plus (D.plus_out j jn))
                {
                  build =
                    (fun fa ->
                      let fb = sface_plus_sface (sface_of_tface fe) mn jn (id_sface n) in
                      Hashtbl.find tyargtbl (SFace_of (comp_sface fb (sface_of_tface fa))));
                } in
            let tm = inst (TubeOf.find tyargs (tface_plus fe mn mn jn)).tm jnargs in
            let ty = tyof_inst jntyargs jnargs in
            { tm; ty });
      } in
  inst (universe m) margs

(* To typecheck a lambda, do an eta-expanding equality check, check pi-types for equality, or read back a pi-type or a term at a pi-type, we must create one new variable for each argument in the boundary.  Sometimes we need these variables as values and other times as normals. *)
let dom_vars :
    type m. int -> (m, value) CubeOf.t -> (m, value) CubeOf.t * (m, level option * normal) CubeOf.t
    =
 fun i doms ->
  (* To make these variables into values, we need to annotate them with their types, which in general are instantiations of the domains at previous variables.  Thus, we assemble them in a hashtable as we create them for random access to the previous ones. *)
  let argtbl = Hashtbl.create 10 in
  let j = ref 0 in
  let [ args; nfs ] =
    CubeOf.pmap
      {
        map =
          (fun fa [ dom ] ->
            let ty =
              inst dom
                (TubeOf.build D.zero
                   (D.zero_plus (dom_sface fa))
                   {
                     build =
                       (fun fc ->
                         Hashtbl.find argtbl (SFace_of (comp_sface fa (sface_of_tface fc))));
                   }) in
            let level = (i, !j) in
            j := !j + 1;
            let v = { tm = var level ty; ty } in
            Hashtbl.add argtbl (SFace_of fa) v;
            [ v.tm; (Some level, v) ]);
      }
      [ doms ] (Cons (Cons Nil)) in
  (args, nfs)