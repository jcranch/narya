(* This module should not be imported, but used qualified (including its constructor names for printable). *)

open Bwd
open Dim
open Util
open Reporter
open Format
open Syntax
open Value
open Term

(* Functions to dump a partial direct representation of various kinds of syntax, avoiding the machinery of readback, unparsing, etc. that's needed for ordinary pretty-printing.  Intended only for debugging. *)

type printable +=
  | Val : 's value -> printable
  | Uninst : uninst -> printable
  | Head : head -> printable
  | Binder : ('b, 's) binder -> printable
  | Term : ('b, 's) term -> printable
  | Env : ('n, 'b) Value.env -> printable

let dim : formatter -> 'a D.t -> unit =
 fun ppf d -> fprintf ppf "%d" (String.length (string_of_deg (id_deg d)))

let rec dvalue : type s. int -> formatter -> s value -> unit =
 fun depth ppf v ->
  match v with
  | Uninst (tm, ty) ->
      if depth > 0 then fprintf ppf "Uninst (%a, %a)" uninst tm (dvalue (depth - 1)) (Lazy.force ty)
      else fprintf ppf "Uninst (%a, ?)" uninst tm
  | Inst { tm; dim = d; args = _; tys = _ } ->
      fprintf ppf "Inst (%a, %a, ?, ?)" uninst tm dim (D.pos d)
  | Lam (_, _) -> fprintf ppf "Lam ?"
  | Struct (f, _) -> fprintf ppf "Struct (%a)" fields f
  | Constr (c, d, args) ->
      fprintf ppf "Constr (%s, %a, (%a))" (Constr.to_string c) dim d
        (pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf ", ") value)
        (List.map CubeOf.find_top args)

and value : type s. formatter -> s value -> unit = fun ppf v -> dvalue 2 ppf v

and fields : type s. formatter -> (Field.t, s lazy_eval * [ `Labeled | `Unlabeled ]) Abwd.t -> unit
    =
 fun ppf -> function
  | Emp -> fprintf ppf "Emp"
  | Snoc (flds, (f, (v, l))) ->
      let v = Norm.force_eval v in
      fprintf ppf "%a <: (%s, %a, %s)" fields flds (Field.to_string f) evaluation v
        (match l with
        | `Unlabeled -> "`Unlabeled"
        | `Labeled -> "`Labeled")

and evaluation : type s. formatter -> s evaluation -> unit =
 fun ppf v ->
  match v with
  | Unrealized -> fprintf ppf "Unrealized"
  | Realize v -> fprintf ppf "Realize (%a)" value v
  | Val v -> fprintf ppf "Val (%a)" value v
  | Canonical _ -> fprintf ppf "Canonical ?"

and uninst : formatter -> uninst -> unit =
 fun ppf u ->
  match u with
  | UU n -> fprintf ppf "UU %a" dim n
  | Pi (_, _, _) -> fprintf ppf "Pi ?"
  | Neu { head = h; args = a; alignment = al } ->
      fprintf ppf "Neu (%a, (%a), %a)" head h args a alignment al

and args : formatter -> app Bwd.t -> unit =
 fun ppf args ->
  fprintf ppf "Emp";
  Mbwd.miter (fun [ a ] -> fprintf ppf " <: %a" app a) [ args ]

and app : formatter -> app -> unit =
 fun ppf -> function
  | App (a, _) -> fprintf ppf "(%a,?)" arg a

and arg : type n. formatter -> n arg -> unit =
 fun ppf -> function
  | Arg xs -> value ppf (CubeOf.find_top xs).tm
  | Field fld -> fprintf ppf ".%s" (Field.to_string fld)

and alignment : formatter -> alignment -> unit =
 fun ppf al ->
  match al with
  | True -> fprintf ppf "True"
  | Chaotic v -> fprintf ppf "Chaotic (%a)" value v
  | Lawful _ -> fprintf ppf "Lawful ?"

and head : formatter -> head -> unit =
 fun ppf h ->
  match h with
  | Var { level; _ } -> fprintf ppf "Var (%d,%d)" (fst level) (snd level)
  | Const { name; ins } ->
      fprintf ppf "Const (%a, %s)" pp_printed (print (PConstant name))
        (string_of_deg (perm_of_ins ins))
  | Meta { meta; env = _; ins } ->
      fprintf ppf "Meta (%s, ?, %s)" (Meta.name meta) (string_of_deg (perm_of_ins ins))

and binder : type b s. formatter -> (b, s) binder -> unit =
 fun ppf (Bind { env = e; ins = _; body }) -> fprintf ppf "Binder (%a, ?, %a)" env e term body

and env : type b n. formatter -> (n, b) Value.env -> unit =
 fun ppf e ->
  match e with
  | Emp d -> fprintf ppf "Emp %a" dim d
  | Ext (e, _, _) -> fprintf ppf "%a <: ?" env e
  | LazyExt (e, _, _) -> fprintf ppf "%a <: ?" env e
  | Act (e, Op (f, d)) -> fprintf ppf "%a <* (%s,%s)" env e (string_of_sface f) (string_of_deg d)
  | Permute (_, e) -> fprintf ppf "(%a) permuted(?)" env e
  | Shift (e, mn, _) -> fprintf ppf "%a << %a" env e dim (D.plus_right mn)

and term : type b s. formatter -> (b, s) term -> unit =
 fun ppf tm ->
  match tm with
  | Var _ -> fprintf ppf "Var ?"
  | Const c -> fprintf ppf "Const %a" pp_printed (print (PConstant c))
  | Meta (v, _) -> fprintf ppf "Meta %a" pp_printed (print (PMeta v))
  | MetaEnv (v, _) -> fprintf ppf "MetaEnv (%a,?)" pp_printed (print (PMeta v))
  | Field (tm, fld) -> fprintf ppf "Field (%a, %s)" term tm (Field.to_string fld)
  | UU n -> fprintf ppf "UU %a" dim n
  | Inst (tm, _) -> fprintf ppf "Inst (%a, ?)" term tm
  | Pi (x, doms, _) ->
      fprintf ppf "Pi^(%a) (%s, ?, ?)" dim (CubeOf.dim doms) (Option.value x ~default:"_")
  | App (fn, arg) -> fprintf ppf "App (%a, (... %a))" term fn term (CubeOf.find_top arg)
  | Lam (_, body) -> fprintf ppf "Lam (?, %a)" term body
  | Constr (c, _, _) -> fprintf ppf "Constr (%s, ?, ?)" (Constr.to_string c)
  | Act (tm, s) -> fprintf ppf "Act (%a, %s)" term tm (string_of_deg s)
  | Let (_, _, _) -> fprintf ppf "Let (?,?,?)"
  | Struct (_, _, _) -> fprintf ppf "Struct (?,?,?)"
  | Match _ -> fprintf ppf "Match (?,?,?)"
  | Realize tm -> fprintf ppf "Realize (%a)" term tm
  | Canonical c -> fprintf ppf "Canonical (%a)" canonical c

and canonical : type b. formatter -> b canonical -> unit =
 fun ppf c ->
  match c with
  | Data { indices; constrs; discrete = _ } ->
      fprintf ppf "Data (%d, (%s), ?)" (Fwn.to_int indices)
        (String.concat "," (Bwd_extra.to_list_map (fun (c, _) -> Constr.to_string c) constrs))
  | Codata { eta; fields; _ } ->
      fprintf ppf "Codata (%s, (%s))"
        (match eta with
        | Eta -> "Eta"
        | Noeta -> "Noeta")
        (String.concat "," (List.map (fun (f, _) -> Field.to_string f) (Bwd.to_list fields)))
