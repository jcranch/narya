open Util
open Tbwd
open Dim
open Reporter
open Syntax
open Term
open Value
open Readback

(* Flatten contexts, abstract over flattened contexts, and apply terms to the free variables in a context.  Used for lifting locally "admitted" axioms and  and local function definitions to the global situation. *)

module Ordered = struct
  open Ctx
  open Ctx.Ordered

  (* Apply a term to all the FREE variables in an entry, converted back to index variables. *)
  let app_frees : type f n a b. (a, b) t -> (b, kinetic) term -> (f, n) entry -> (b, kinetic) term =
   fun total tm e ->
    (* We use a state monad to do a left fold over the cube of variables. *)
    let module M = CubeOf.Monadic (Monad.State (struct
      type t = (b, kinetic) term
    end)) in
    match e with
    | Vis { bindings; _ } | Invis bindings ->
        snd
          (M.miterM
             {
               it =
                 (fun _ [ x ] tm ->
                   match Binding.level x with
                   | Some lvl -> (
                       match find_level total lvl with
                       | Some ix -> ((), App (tm, CubeOf.singleton (Term.Var ix)))
                       | None -> fatal (Anomaly "missing level in app_frees"))
                   | None -> ((), tm));
             }
             [ bindings ] tm)

  type wrapped = Wrap : ('a, 'b) t -> wrapped

  (* Flatten a context into one containing only zero-dimensional free variables.  Throws away all the let-bound variables and field-view variables.  Obviously this context can no longer be used for typechecking anything, since its raw length is undetermined.  Similarly, it can only be used for evaluating a term if that term was obtained by readback into itself.  But such readback can be done with values evaluated in the original context, since it contains the same level variables as that. *)
  let rec flatten : type a b. (a, b) t -> wrapped =
   fun ctx ->
    let module M = CubeOf.Monadic (Monad.State (struct
      type t = wrapped
    end)) in
    match ctx with
    | Emp -> Wrap Emp
    | Snoc (ctx, Invis bindings, _) ->
        snd
          (M.miterM
             {
               it =
                 (fun _ [ b ] (Wrap ctx) ->
                   match Binding.level b with
                   | Some _ -> ((), Wrap (Snoc (ctx, Invis (CubeOf.singleton b), Zero)))
                   | None -> ((), Wrap ctx));
             }
             [ bindings ] (flatten ctx))
    | Snoc (ctx, Vis { plusdim; vars; bindings; _ }, _) ->
        snd
          (M.miterM
             {
               it =
                 (fun fab [ b ] (Wrap ctx) ->
                   match Binding.level b with
                   | Some _ ->
                       let (SFace_of_plus (_, fa, fb)) = sface_of_plus plusdim fab in
                       let fa = string_of_sface fa in
                       ( (),
                         Wrap
                           (Snoc
                              ( ctx,
                                Vis
                                  {
                                    dim = D.zero;
                                    plusdim = D.zero_plus D.zero;
                                    vars =
                                      NICubeOf.singleton
                                        (* We replace the usual "." introducing the face with a "_", since we want to generate an actual valid local variable name.  Handling possible collisions is done elsewhere, in Parser.Names. *)
                                        (Option.map
                                           (fun s -> if fa = "" then s else s ^ "_" ^ fa)
                                           (NICubeOf.find vars fb));
                                    bindings = CubeOf.singleton b;
                                    hasfields = No_fields;
                                    fields = Emp;
                                    fplus = Zero;
                                  },
                                Suc Zero )) )
                   | None -> ((), Wrap ctx));
             }
             [ bindings ] (flatten ctx))
    | Lock ctx ->
        let (Wrap ctx) = flatten ctx in
        Wrap (Lock ctx)

  (* Compute the iterated 0-dimensional pi-type of some type over all the types of the non-let-bound variables in a FLATTENED context, along with the result of applying a given term (presumed to have that pi-type) to all of those variables. *)
  let rec pi_app_flattened :
      type a b c d.
      total:(c, d) t ->
      current:(a, b) t ->
      tm:(d, kinetic) term ->
      ty:(b, kinetic) term ->
      (emp, kinetic) term * (d, kinetic) term =
   fun ~total ~current ~tm ~ty ->
    match current with
    | Emp -> (ty, tm)
    | Snoc (ctx, e, _) -> (
        match D.compare (dim_entry e) D.zero with
        | Eq -> (
            let x, b =
              match e with
              | Vis { vars; bindings; _ } -> (NICubeOf.find_top vars, CubeOf.find_top bindings)
              | Invis bs -> (None, CubeOf.find_top bs) in
            match Binding.level b with
            | Some _ ->
                let dom = readback_val (Ctx.of_ordered ctx) (Binding.value b).ty in
                let ty = Term.Pi (x, CubeOf.singleton dom, CodCube.singleton ty) in
                let ty, tm = pi_app_flattened ~total ~current:ctx ~tm ~ty in
                (ty, app_frees total tm e)
            | None -> fatal (Anomaly "let-bound variable in flattened context"))
        | Neq -> fatal (Anomaly "unflattened context in pi_app_flattened"))
    (* TODO: This should really be some kind of modal pi-type.  Currently the lock means totally inaccessible, so this would be a sort of "type of constant functions". *)
    | Lock current -> pi_app_flattened ~total ~current ~tm ~ty
end

let pi_app (Ctx.Permute (_, ctx)) ~tm ~ty =
  let (Wrap flat_ctx) = Ordered.flatten ctx in
  Ordered.pi_app_flattened ~total:ctx ~current:flat_ctx ~tm
    ~ty:(readback_val (Ctx.of_ordered flat_ctx) ty)
