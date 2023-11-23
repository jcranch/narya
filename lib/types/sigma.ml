open Util
open Dim
open Core
open Parser
open Notation
open Compile
open Raw
open Term

let sigma = Constant.make ()
let fst = Field.intern "fst"
let snd = Field.intern "snd"
let pair = Constant.make ()

open Monad.Ops (Monad.Maybe)

(* TODO: printing these notations *)

let sigman = make ~origname:"sigma" ~tightness:10. ~left:Closed ~right:Open ~assoc:Right

let () =
  set_tree sigman
    (eop LParen
       (ident (op Colon (term RParen (ops [ (Ident "×", Done sigman); (Op "><", Done sigman) ])))));
  set_compiler sigman
    {
      compile =
        (fun ctx obs ->
          let x, obs = get_ident obs in
          let tm, obs = get_term obs in
          let ty, obs = get_term obs in
          let () = get_done obs in
          let tm = compile ctx tm in
          let ty = compile (Snoc (ctx, x)) ty in
          Synth (App (App (Const sigma, tm), Lam (`Normal, ty))));
    }

let prodn = make ~origname:"prod" ~tightness:10. ~left:Open ~right:Open ~assoc:Right

let () =
  set_tree prodn (eops [ (Ident "×", Done prodn); (Op "><", Done prodn) ]);
  set_compiler prodn
    {
      compile =
        (fun ctx obs ->
          let tm, obs = get_term obs in
          let ty, obs = get_term obs in
          let () = get_done obs in
          let tm = compile ctx tm in
          let ty = compile (Snoc (ctx, None)) ty in
          Synth (App (App (Const sigma, tm), Lam (`Normal, ty))));
    }

let comma = make ~origname:"comma" ~tightness:10. ~left:Open ~right:Open ~assoc:Right

let () =
  set_tree comma (eop (Op ",") (Done comma));
  set_compiler comma
    {
      compile =
        (fun ctx obs ->
          let x, obs = get_term obs in
          let y, obs = get_term obs in
          let () = get_done obs in
          let x = compile ctx x in
          let y = compile ctx y in
          Raw.Struct (Field.Map.of_list [ (fst, [ x ]); (snd, [ y ]) ]));
    }

let install_notations () =
  Builtins.builtins := !Builtins.builtins |> State.add sigman |> State.add prodn |> State.add comma

let install () =
  install_notations ();
  Scope.set "Σ" sigma;
  Scope.set "pair" pair;
  Hashtbl.add Global.types sigma
    (pi (UU D.zero) (pi (pi (Var (Top (id_sface D.zero))) (UU D.zero)) (UU D.zero)));
  Hashtbl.add Global.types pair
    (pi (UU D.zero)
       (pi
          (pi (Var (Top (id_sface D.zero))) (UU D.zero))
          (pi
             (Var (Pop (Top (id_sface D.zero))))
             (pi
                (app (Var (Pop (Top (id_sface D.zero)))) (Var (Top (id_sface D.zero))))
                (app
                   (app (Const sigma) (Var (Pop (Pop (Pop (Top (id_sface D.zero)))))))
                   (Var (Pop (Pop (Top (id_sface D.zero))))))))));
  Hashtbl.add Global.constants sigma
    (Record
       {
         eta = true;
         params = Suc (Suc Zero);
         dim = D.zero;
         fields =
           [
             (fst, Var (Pop (Pop (Top (id_sface D.zero)))));
             ( snd,
               app (Var (Pop (Top (id_sface D.zero)))) (Field (Var (Top (id_sface D.zero)), fst)) );
           ];
       });
  Hashtbl.add Global.constants pair
    (Defined
       (ref
          (Case.Lam
             ( D.zero,
               ref
                 (Case.Lam
                    ( D.zero,
                      ref
                        (Case.Lam
                           ( D.zero,
                             ref
                               (Case.Lam
                                  ( D.zero,
                                    ref
                                      (Case.Leaf
                                         (Struct
                                            (Field.Map.empty
                                            |> Field.Map.add fst (Var (Pop (Top (id_sface D.zero))))
                                            |> Field.Map.add snd (Var (Top (id_sface D.zero)))))) ))
                           )) )) ))))
