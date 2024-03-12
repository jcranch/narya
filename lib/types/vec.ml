open Bwd
open Bwd.Infix
open Dim
open Util
open Core
open Syntax
open Term
open Nat
open Parser

let nil = Constr.intern "nil"
let cons = Constr.intern "cons"

let install () =
  Nat.install ();
  let vec = Scope.define [ "Vec" ] in
  Global.add vec
    (pi None (UU D.zero) (pi None (Const nn) (UU D.zero)))
    (Defined
       (Lam
          ( D.zero,
            `Cube (Some "A"),
            Canonical
              (Data
                 ( N.one,
                   Constr.Map.empty
                   |> Constr.Map.add nil
                        (Dataconstr { args = Emp; indices = Snoc (Emp, Constr (zero, D.zero, Emp)) })
                   |> Constr.Map.add cons
                        (Dataconstr
                           {
                             args =
                               Ext
                                 ( None,
                                   Const nn,
                                   Ext
                                     ( None,
                                       Var (Pop (Top (id_sface D.zero))),
                                       Ext
                                         ( None,
                                           App
                                             ( App
                                                 ( Const vec,
                                                   CubeOf.singleton
                                                     (Term.Var (Pop (Pop (Top (id_sface D.zero)))))
                                                 ),
                                               CubeOf.singleton
                                                 (Term.Var (Pop (Top (id_sface D.zero)))) ),
                                           Emp ) ) );
                             indices =
                               Snoc
                                 ( Emp,
                                   Constr
                                     ( suc,
                                       D.zero,
                                       Emp
                                       <: CubeOf.singleton
                                            (Term.Var (Pop (Pop (Top (id_sface D.zero))))) ) );
                           }) )) )))
