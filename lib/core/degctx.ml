open Dim
open Reporter
open Syntax
open Value
open Readback

(* Degenerating contexts (for higher inductive and coinductive types).  The degeneracy of a context by a dimension k is a context in which all the n-cubes of variables have been replaced by (k+n)-cubes, whose types are degenerate versions of those in the original context.  Thus, while its raw length is the same, its checked length has k added to all the dimensions.  There is a canonical k-dimensional substitution (environment) from the degenerated context to the original one, which is "universal" in an appropriate sense (the degenerate context is a path-space object in the cubically enriched category of contexts).

   You might naively think that we could build the degenerated context by iterating through the original one and applying "act" to all the types.  However, the degenerated context has *more level variables* than the original context does, and we need to create these variables and ensure that they appear in all the later types as needed.  Thus, we really do have to do an eval-readback cycle. *)

module Ordered = struct
  open Ctx.Ordered

  let degenerate_binding :
      type k n kn a b.
      int ->
      k D.t ->
      (k, n, kn) D.plus ->
      (n, Binding.t) CubeOf.t ->
      (a, b) t ->
      (k, b) env ->
      (kn, Binding.t) CubeOf.t * (kn, kinetic value) CubeOf.t =
   fun i k kn xs ctx env ->
    let ctx = Ctx.of_ordered ctx in
    let readbacks =
      CubeOf.mmap
        {
          map =
            (fun _ [ x ] ->
              let nf = Binding.value x in
              match Binding.level x with
              | None -> (Some (readback_nf ctx nf), readback_val ctx nf.ty)
              | Some _ -> (None, readback_val ctx nf.ty));
        }
        [ xs ] in
    let j = ref 0 in
    let newxs =
      CubeOf.build (D.plus_out k kn)
        {
          build =
            (fun fab ->
              let (SFace_of_plus (_, fa, fb)) = sface_of_plus kn fab in
              match CubeOf.find readbacks fb with
              | None, ty ->
                  let level = (i, !j) in
                  j := !j + 1;
                  let ty = Norm.eval_term (Act (env, op_of_sface fa)) ty in
                  Binding.make (Some level) { tm = var level ty; ty }
              | Some tm, ty ->
                  (* Incrementing the level isn't really necessary since we aren't going to use it in this case, but we do it anyway for consistency. *)
                  j := !j + 1;
                  let tm = Norm.eval_term (Act (env, op_of_sface fa)) tm in
                  let ty = Norm.eval_term (Act (env, op_of_sface fa)) ty in
                  Binding.make None { tm; ty });
        } in
    let newvals = CubeOf.mmap { map = (fun _ [ v ] -> (Binding.value v).tm) } [ newxs ] in
    (newxs, newvals)

  type (_, _, _) degctx =
    | Degctx : ('k, 'b, 'kb) Plusmap.t * ('a, 'kb) t * ('k, 'b) env -> ('a, 'b, 'k) degctx

  let rec degenerate : type a b k. (a, b) t -> k D.t -> (a, b, k) degctx =
   fun ctx k ->
    match ctx with
    | Emp -> Degctx (Map_emp, Emp, Emp k)
    | Snoc (ctx, entry, ax) ->
        let (Degctx (kb, newctx, env)) = degenerate ctx k in
        let mn = Ctx.dim_entry entry in
        let (Plus k_mn) = D.plus mn in
        let newentry, newenv =
          match entry with
          | Vis { hasfields = Has_fields; _ } ->
              fatal (Anomaly "attempt to degenerate a context containing illusory variables")
          | Vis { dim; plusdim; vars; bindings; hasfields = No_fields; fields; fplus } ->
              let (Plus km) = D.plus dim in
              let plusdim = D.plus_assocl km plusdim k_mn in
              let bindings, newval = degenerate_binding (length newctx) k k_mn bindings ctx env in
              let hasfields = Ctx.No_fields in
              ( Ctx.Vis { dim = D.plus_out k km; plusdim; vars; bindings; hasfields; fields; fplus },
                Ext (env, k_mn, newval) )
          | Invis xs ->
              let newxs, newval = degenerate_binding (length newctx) k k_mn xs ctx env in
              (Invis newxs, Ext (env, k_mn, newval)) in
        Degctx (Map_snoc (kb, k_mn), Snoc (newctx, newentry, ax), newenv)
    | Lock ctx ->
        let (Degctx (kb, newctx, env)) = degenerate ctx k in
        Degctx (kb, Lock newctx, env)
end

type (_, _, _) degctx =
  | Degctx : ('k, 'b, 'kb) Plusmap.t * ('a, 'kb) Ctx.t * ('k, 'b) env -> ('a, 'b, 'k) degctx

let degctx : type a b k. (a, b) Ctx.t -> k D.t -> (a, b, k) degctx =
 fun (Permute (p, ctx)) k ->
  let (Degctx (kb, newctx, env)) = Ordered.degenerate ctx k in
  Degctx (kb, Permute (p, newctx), env)
