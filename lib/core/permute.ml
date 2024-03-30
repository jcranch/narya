open Util
open Tbwd
open Dim
open Syntax
open Value
open Act

(* Permute environments *)

(* Decompose the top of an env into either Emp or Ext, by pushing Acts and Shifts through. *)
type (_, _) env_decomp =
  | Emp : 'n D.t -> ('n, emp) env_decomp
  | Ext :
      ('n, 'b) env * ('k, ('n, kinetic value) CubeOf.t) CubeOf.t
      -> ('n, ('b, 'k) snoc) env_decomp

(* Select and remove an arbitrary entry from an environment. *)
type (_, _) selected = Selected : ('k, ('n, kinetic value) CubeOf.t) CubeOf.t -> ('k, 'n) selected

let rec env_top : type n a. (n, a) env -> (n, a) env_decomp = function
  | Emp n -> Emp n
  | Ext (env, xs) -> Ext (env, xs)
  | Act (env, (Op (fb, fa) as fba)) -> (
      match env_top env with
      | Emp _ -> Emp (dom_deg fa)
      | Ext (env, xs) ->
          (* Pushing an Act through means performing the action on the value and type of the top variable, and dropping the Act on the rest of the environment. *)
          Ext
            ( Act (env, fba),
              CubeOf.mmap
                { map = (fun _ [ ys ] -> act_value_cube (CubeOf.subcube fb ys) fa) }
                [ xs ] ))
  | Shift (env, m_n, nb) -> (
      match (env_top env, nb) with
      | Emp mn, Map_emp -> Emp (D.plus_left m_n mn)
      | Ext (env, xs), Map_snoc (nb, nk) ->
          (* Pushing a Shift through means re-associating a cube of cubes, and dropping the Shift on the rest of the environment. *)
          Ext
            ( Shift (env, m_n, nb),
              CubeOf.build
                (D.plus_out (D.plus_right m_n) nk)
                {
                  build =
                    (fun fbc ->
                      let (SFace_of_plus (_, fb, fc)) = sface_of_plus nk fbc in
                      CubeOf.build
                        (D.plus_left m_n (dim_env env))
                        {
                          build =
                            (fun fa ->
                              let (Plus p) = D.plus (dom_sface fb) in
                              CubeOf.find (CubeOf.find xs fc) (sface_plus_sface fa m_n p fb));
                        });
                } ))
  | Permute (p, env) -> env_top (permute_env p env)

(* Note that the return entry is n-dimensional, since all the operator actions have to be applied as we pull it out. *)
and select_env : type a b n k. (a, k, b) Tbwd.insert -> (n, b) env -> (n, a) env * (k, n) selected =
 fun ins env ->
  match ins with
  | Now ->
      let (Ext (env, top)) = env_top env in
      (env, Selected top)
  | Later ins ->
      let (Ext (env, top)) = env_top env in
      let env, sel = select_env ins env in
      (Ext (env, top), sel)

(* Permute an environment.  The delayed actions and shifts in the input environment are preserved in the leftmost part of the permutation that's the identity, but all the others are applied to the terms in the process of permuting. *)
and permute_env : type a b n. (a, b) Tbwd.permute -> (n, b) env -> (n, a) env =
 fun perm env ->
  match perm with
  | Id -> env
  | Insert (perm, ins) ->
      let env, Selected sel = select_env ins env in
      Ext (permute_env perm env, sel)
