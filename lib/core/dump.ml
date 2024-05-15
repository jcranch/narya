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
  | Head : 'h head -> printable
  | Binder : ('b, 's) binder -> printable
  | Term : ('b, 's) term -> printable
  | Env : ('n, 'b) env -> printable

let dim : formatter -> 'a D.t -> unit =
 fun ppf d -> fprintf ppf "%d" (String.length (string_of_deg (id_deg d)))

let rec value : type s. formatter -> s value -> unit =
 fun ppf v ->
  match v with
  | Lazy (lazy v) -> value ppf v
  | Uninst (tm, _) -> fprintf ppf "Uninst (%a, ?)" uninst tm
  | Inst { tm; dim = d; args = _; tys = _ } ->
      fprintf ppf "Inst (%a, %a, ?, ?)" uninst tm dim (D.pos d)
  | Lam (_, _) -> fprintf ppf "Lam ?"
  | Struct (_, f, _) -> fprintf ppf "Struct (%a)" fields f
  | Constr (_, _, _) -> fprintf ppf "Constr ?"

and fields : type s ambient. formatter -> (s, ambient) Value.structfield Bwd.t -> unit =
 fun ppf -> function
  | Emp -> fprintf ppf "Emp"
  | Snoc (flds, Higher_structfield { name; _ }) ->
      fprintf ppf "%a <: (%s, ...)" fields flds (Field.string_of_checked name)
  | Snoc (flds, Lower_structfield { name; labeled; _ }) ->
      fprintf ppf "%a <: (%s, ..., %s)" fields flds (Field.to_string name)
        (match labeled with
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
  | Field fld -> fprintf ppf ".%s" (Field.string_of_checked fld)

and alignment : type h. formatter -> h alignment -> unit =
 fun ppf al ->
  match al with
  | True -> fprintf ppf "True"
  | Chaotic v -> fprintf ppf "Chaotic (%a)" value v
  | Lawful _ -> fprintf ppf "Lawful ?"

and head : type h. formatter -> h head -> unit =
 fun ppf h ->
  match h with
  | Var { level; _ } -> fprintf ppf "Var (%d,%d)" (fst level) (snd level)
  | Const { name; ins } ->
      fprintf ppf "Const (%a, %s)" pp_printed (print (PConstant name))
        (string_of_deg (perm_of_ins ins))

and binder : type b s. formatter -> (b, s) binder -> unit =
 fun ppf (Bind { env = e; ins = _; body }) -> fprintf ppf "Binder (%a, ?, %a)" env e term body

and env : type b n. formatter -> (n, b) env -> unit =
 fun ppf e ->
  match e with
  | Emp d -> fprintf ppf "Emp %a" dim d
  | Ext (e, _) -> fprintf ppf "%a <: ?" env e
  | Act (e, Op (f, d)) -> fprintf ppf "%a <* (%s,%s)" env e (string_of_sface f) (string_of_deg d)
  | Permute (_, e) -> fprintf ppf "(%a) permuted(?)" env e
  | Shift (e, mn, _) -> fprintf ppf "%a << %a" env e dim (D.plus_right mn)

and term : type b s. formatter -> (b, s) term -> unit =
 fun ppf tm ->
  match tm with
  | Var _ -> fprintf ppf "Var ?"
  | Const c -> fprintf ppf "Const %a" pp_printed (print (PConstant c))
  | Field (tm, fld) -> fprintf ppf "Field (%a, %s)" term tm (Field.string_of_checked fld)
  | UU n -> fprintf ppf "UU %a" dim n
  | Inst (tm, _) -> fprintf ppf "Inst (%a, ?)" term tm
  | Pi (_, _, _) -> fprintf ppf "Pi (?, ?, ?)"
  | App (fn, _) -> fprintf ppf "App (%a, ?)" term fn
  | Lam (_, body) -> fprintf ppf "Lam (?, %a)" term body
  | Constr (c, _, _) -> fprintf ppf "Constr (%s, ?, ?)" (Constr.to_string c)
  | Act (tm, s) -> fprintf ppf "Act (%a, %s)" term tm (string_of_deg s)
  | Let (_, _, _) -> fprintf ppf "Let (?,?,?)"
  | Struct (_, _, _, _) -> fprintf ppf "Struct (?,?,?,?)"
  | Match (_, _, _) -> fprintf ppf "Match (?,?,?)"
  | Realize tm -> fprintf ppf "Realize %a" term tm
  | Canonical _ -> fprintf ppf "Canonical ?"
