open Core
open Reporter
open Uuseg_string
open Printconfig

type t =
  | Field of string * string list (* Starting with . *)
  | Constr of string (* Ending with . *)
  | LParen (* ( *)
  | RParen (* ) *)
  | LBracket (* [ *)
  | RBracket (* ] *)
  | LBrace (* { *)
  | RBrace (* } *)
  | Arrow (* Both -> and → *)
  | Mapsto (* Both |-> and ↦ *)
  | DblMapsto (* Both |=> and ⤇ *)
  | Colon (* : *)
  | Coloneq (* Both := and ≔ *)
  | DblColoneq (* Both ::= and ⩴ *)
  | Pluseq (* Both += and ⩲ *)
  | Dot (* . *)
  | Ellipsis (* ... and … *)
  | String of string (* Double-quoted *)
  | Underscore (* _ *)
  | Internal of string (* Starting or ending with _ *)
  | Axiom
  | Def
  | Echo
  | Match
  | Sig
  | Data
  | Codata
  | Notation
  | Let
  | In
  | Op of string (* Sequence of common ASCII symbols, other than : := ::= += -> |-> |=> etc. *)
  (* Alphanumeric/unicode other than common ASCII symbols and above single-token characters, with dots and underscores occuring only internally, and each dot-separated piece being nonempty.  Those not containing any dots could be local variable names (with one dot, they could be a face of a cube variable), and those consisting entirely of digits could be numerals.  We can't separate these out at lexing time into those that are parts of mixfix notations and those that are potential identifiers, since the mixfix notations in scope change as we go through a file. *)
  | Ident of string list
  | Superscript of string
  | Bof
  | Eof

let compare : t -> t -> int = compare

(* Whether a string is valid as a dot-separated piece of an identifier name, or equivalently as a local variable name.  We don't test for absence of the delimited symbols, since they will automatically be lexed separately; this is for testing validity after the lexer has split things into potential tokens.  We also don't test for absence of dots, since identifiers will be split on dots automatically. *)
let ok_ident s =
  let len = String.length s in
  len > 0 && s.[0] <> '_' && s.[len - 1] <> '_'

(* All (or at least most) of the Unicode superscript characters, their code points, and the corresponding ASCII characters.  This is digits, lowercase Roman letters, parentheses, and plus, minus, and equals signs. *)
let supers =
  [|
    ("⁰", 0x2070, '0');
    ("¹", 0xb9, '1');
    ("²", 0xb2, '2');
    ("³", 0xb3, '3');
    ("⁴", 0x2074, '4');
    ("⁵", 0x2075, '5');
    ("⁶", 0x2076, '6');
    ("⁷", 0x2077, '7');
    ("⁸", 0x2078, '8');
    ("⁹", 0x2079, '9');
    ("ᵃ", 0x1d43, 'a');
    ("ᵇ", 0x1d47, 'b');
    ("ᶜ", 0x1d9c, 'c');
    ("ᵈ", 0x1d48, 'd');
    ("ᵉ", 0x1d49, 'e');
    ("ᶠ", 0x1da0, 'f');
    ("ᵍ", 0x1d4d, 'g');
    ("ʰ", 0x2b0, 'h');
    ("ⁱ", 0x2071, 'i');
    ("ʲ", 0x2b2, 'j');
    ("ᵏ", 0x1d4f, 'k');
    ("ˡ", 0x2e1, 'l');
    ("ᵐ", 0x1d50, 'm');
    ("ⁿ", 0x207f, 'n');
    ("ᵒ", 0x1d52, 'o');
    ("ᵖ", 0x1d56, 'p');
    ("𐞥", 0x107a5, 'q');
    ("ʳ", 0x2b3, 'r');
    ("ˢ", 0x2e2, 's');
    ("ᵗ", 0x1d57, 't');
    ("ᵘ", 0x1d58, 'u');
    ("ᵛ", 0x1d5b, 'v');
    ("ʷ", 0x2b7, 'w');
    ("ˣ", 0x2e3, 'x');
    ("ʸ", 0x2b8, 'y');
    ("ᶻ", 0x1dbb, 'z');
    ("⁽", 0x207d, '(');
    ("⁾", 0x207e, ')');
    ("⁺", 0x207a, '+');
    ("⁻", 0x207b, '-');
    ("⁼", 0x207c, '=');
  |]

let super_strings = Array.map (fun (x, _, _) -> x) supers
let super_uchars = Array.map (fun (_, c, _) -> Uchar.of_int c) supers
let unsupers = Array.map (fun (_, _, s) -> s) supers

(* Convert a string of ASCII characters to a corresponding Unicode superscript. *)
let to_super (s : string) : string =
  let b = Buffer.create 10 in
  String.iter
    (fun c ->
      match Array.find_index (fun x -> x = c) unsupers with
      | Some i -> Buffer.add_string b super_strings.(i)
      | None -> fatal (Anomaly "character has no superscript version"))
    s;
  Buffer.contents b

(* Convert a Unicode superscript to a corresponding string of ASCII characters. *)
let of_super (s : string) : string =
  let b = Buffer.create 10 in
  let rec of_super n =
    if n >= String.length s then Buffer.contents b
    else
      let dec = String.get_utf_8_uchar s n in
      if Uchar.utf_decode_is_valid dec then (
        let len = Uchar.utf_decode_length dec in
        let c = Uchar.utf_decode_uchar dec in
        (match Array.find_index (fun x -> x = c) super_uchars with
        | Some i -> Buffer.add_char b unsupers.(i)
        | None -> fatal (Anomaly "character is not a superscript"));
        of_super (n + len))
      else fatal Encoding_error in
  of_super 0

let to_string = function
  | Field (f, strs) -> String.concat "." ("" :: f :: strs)
  | Constr s -> s ^ "."
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Arrow -> alt_char "→" "->"
  | Mapsto -> alt_char "↦" "|->"
  | DblMapsto -> alt_char "⤇" "|=>"
  | Colon -> ":"
  | Coloneq -> alt_char "≔" ":="
  | DblColoneq -> alt_char "⩴" "::="
  | Pluseq -> alt_char "⩲" "+="
  | Dot -> "."
  | Ellipsis -> alt_char "…" "..."
  | String s -> "\"" ^ s ^ "\""
  | Underscore -> "_"
  | Internal s -> s
  | Axiom -> "axiom"
  | Def -> "def"
  | Echo -> "echo"
  | Match -> "match"
  | Sig -> "sig"
  | Data -> "data"
  | Codata -> "codata"
  | Notation -> "notation"
  | Let -> "let"
  | In -> "in"
  | Op s -> s
  | Ident s -> String.concat "." s
  | Superscript s -> to_super s
  | Bof -> "BOF"
  | Eof -> "EOF"

(* Given a token, create a constant pretty-printer that prints that token. *)
let pp tok ppf () = pp_utf_8 ppf (to_string tok)
