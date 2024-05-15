open Ctx

type (_, _, _) degenerate =
  | Degenerate : ('k, 'b, 'kb) Plusmap.t * ('a, 'kb) t * ('k, 'b) env -> ('a, 'b, 'k) degenerate

let degenerate : type a b k. readback -> (a, b) t -> k D.t -> (a, b, k) degenerate =
 fun readback (Permute (p, ctx)) k ->
  let (Degenerate (kb, newctx, env)) =
    Ordered.degenerate
      {
        nf = (fun ctx x -> readback.nf (Permute (N.id_perm (Ordered.raw_length ctx), ctx)) x);
        ty = (fun ctx x -> readback.ty (Permute (N.id_perm (Ordered.raw_length ctx), ctx)) x);
      }
      ctx k in
  Degenerate (kb, Permute (p, newctx), env)
