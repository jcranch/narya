open Util
open Bwd
open Core.Reporter
open Fmlib_parse
open Notation
module TokMap = Map.Make (Token)

(* Sometimes we want to parse only a single term, other times we want to parse and execute a sequence of commands.  Since these two processes return different results, they have to be based on different instances of Token_parser.Make.  But they share all the code of the combinators for parsing terms, so we make those instances of a functor as well. *)

(* Parsing a term outputs a parse tree (which is then postprocessed in a context of local variables). *)
module ParseTree = struct
  type t = observation
end

(* We misuse Fmlib's "semantic" errors for a special class of errors that are really syntactic, but which we don't detect until after the relevant tokens have already been "successfully" parsed, and for which we want to report more structured error information than just an "expected" string. *)
module SemanticError = struct
  type t =
    (* These strings are the names of notations.  Arguably we should display their *namespaced* names, which would mean calling out to Yuujinchou.  It would also mean some special-casing, because applications are implemented specially in the parser and not as an actual Notation. *)
    | No_relative_precedence of Asai.Range.t * string * string
    | Invalid_degeneracy of Position.range * string
end

(* The functor that defines all the term-parsing combinators. *)
module Combinators (Final : Fmlib_std.Interfaces.ANY) = struct
  module Basic = Token_parser.Make (Unit) (Lexer.Token_whitespace) (Final) (SemanticError)
  open Basic

  (* We aren't using Fmlib's error reporting, so there's no point in supplying it nonempty "expect" strings. *)
  let step f = step "" f
  let followed_by f = followed_by f ""

  (* Similarly, we want locations reported using Asai ranges rather than Fmlib ones. *)
  let located c =
    let* rng, x = located c in
    return (Range.convert rng, x)

  let locate (loc : Asai.Range.t) (value : 'a) : 'a Asai.Range.located = { value; loc = Some loc }

  let rec tree :
      type tight strict.
      (tight, strict) tree ->
      observation Bwd.t ->
      (Token.t * Whitespace.t list) Bwd.t ->
      (observation Bwd.t
      * (Token.t * Whitespace.t list) Bwd.t
      * (tight, strict) notation_in_interval)
      t =
   fun t obs ws ->
    match t with
    | Inner ({ term = Some e; _ } as br) -> (
        inner_nonterm br obs ws
        </>
        (* This is an *interior* term, so it has no tightness restrictions on what notations can occur inside, and is ended by the specified ending tokens. *)
        let* subterm = lclosed Interval.entire e in
        match subterm.get Interval.entire with
        | Ok tm -> tree_op e (Snoc (obs, Term tm)) ws
        | Error n -> fatal (Anomaly (Printf.sprintf "Interior term failed on notation %s" n)))
    | Inner ({ term = None; _ } as br) -> inner_nonterm br obs ws
    | Done_open (lt, n) -> return (obs, ws, Open_in_interval (lt, n))
    | Done_closed n -> return (obs, ws, Closed_in_interval n)
    | Lazy (lazy t) -> tree t obs ws

  (* Parse an inner branch of a tree except for the possibility of a term. *)
  and inner_nonterm :
      type tight strict.
      (tight, strict) branch ->
      observation Bwd.t ->
      (Token.t * Whitespace.t list) Bwd.t ->
      (observation Bwd.t
      * (Token.t * Whitespace.t list) Bwd.t
      * (tight, strict) notation_in_interval)
      t =
   fun { ops; field; term = _ } obs ws ->
    let* loc, (br, x, w) =
      located
        (step (fun state _ (tok, w) ->
             match TokMap.find_opt tok ops with
             | Some br -> Some ((br, None, [ (tok, w) ]), state)
             | None -> (
                 (* Field names have already been validated by the lexer. *)
                 match (field, tok) with
                 | Some br, Field (x, p) -> Some ((br, Some (Field (x, p, w)), []), state)
                 | _ -> None))) in
    let x =
      match x with
      | None -> []
      | Some x -> [ Term (locate loc x) ] in
    tree br (Bwd.append obs x) (Bwd.append ws w)

  and tree_op :
      type tight strict.
      (tight, strict) tree TokMap.t ->
      observation Bwd.t ->
      (Token.t * Whitespace.t list) Bwd.t ->
      (observation Bwd.t
      * (Token.t * Whitespace.t list) Bwd.t
      * (tight, strict) notation_in_interval)
      t =
   fun ops obs ws ->
    let* optree, w =
      step (fun state _ (tok, w) ->
          match TokMap.find_opt tok ops with
          | Some br -> Some ((br, (tok, w)), state)
          | None -> None) in
    tree optree obs (Snoc (ws, w))

  and entry :
      type tight strict.
      (tight, strict) tree TokMap.t ->
      (observation Bwd.t
      * (Token.t * Whitespace.t list) Bwd.t
      * (tight, strict) notation_in_interval)
      t =
   fun e -> tree_op e Emp Emp

  (* "lclosed" is passed an upper tightness interval and an additional set of ending ops (stored as a map, since that's how they occur naturally, but here we ignore the values and look only at the keys).  It parses an arbitrary left-closed tree (pre-merged).  The interior terms are calls to "lclosed" with the next ops passed as the ending ones. *)
  and lclosed :
      type lt ls rt rs.
      (lt, ls) Interval.tt -> (rt, rs) tree TokMap.t -> (lt, ls) right_wrapped_parse t =
   fun tight stop ->
    let* res =
      (let* inner_loc, (inner, ws, notn) = located (entry (State.Current.left_closeds ())) in
       let ws = Bwd.to_list ws in
       match notn with
       | Open_in_interval (lt, _) -> No.plusomega_nlt lt (* This case is impossible *)
       | Closed_in_interval notn -> (
           match right notn with
           | Closed ->
               (* If the parse ended right-closed, we slurp up any superscripts. *)
               with_supers { get = (fun _ -> Ok (locate inner_loc (outfix ~notn ~ws ~inner))) }
           | Open _ ->
               (* If the parse ended right-open, we call "lclosed" again, with the right-side upper tightness interval of the just-parsed notation, to pick up the open argument.  Since the tightness here is that of the notation n, not the "tight" from the surrounding one that called lclosed, if while parsing a right-open argument of some operator X we see a left-closed, right-open notation Z of *lower* tightness than X, we allow it, and it does not end if we encounter the start of a left-open notation Y of tightness in between X and Z, only if we see something of lower tightness than Z, or a stop-token from an *enclosing* notation (otherwise we wouldn't be able to delimit right-open operators by parentheses). *)
               let* last_arg = lclosed (interval_right notn) stop in
               return
                 {
                   get =
                     (* Both the notation and anything in its right-open argument must be allowed in a right-tightness interval. *)
                     (fun ivl ->
                       match Interval.contains ivl (tightness notn) with
                       | None -> Error (name notn)
                       | Some right_ok -> (
                           match last_arg.get ivl with
                           | Ok last ->
                               Ok
                                 {
                                   value = prefix ~notn ~ws ~inner ~last ~right_ok;
                                   loc = Range.merge_opt (Some inner_loc) last.loc;
                                 }
                           | Error e -> Error e));
                 }))
      (* Otherwise, we parse a single ident or constructor. *)
      </> let* loc, (tm, w) =
            located
              (step (fun state _ (tok, w) ->
                   match tok with
                   | Ident x -> Some ((`Ident x, w), state)
                   (* Constructor names have already been validated by the lexer. *)
                   | Constr x -> Some ((`Constr x, w), state)
                   | Underscore -> Some ((`Placeholder, w), state)
                   | _ -> None)) in
          with_supers
            {
              get =
                (fun _ ->
                  Ok
                    (locate loc
                       (match tm with
                       | `Ident x -> Ident (x, w)
                       | `Constr x -> Constr (x, w)
                       | `Placeholder -> Placeholder w)));
            } in
    (* Then "lclosed" ends by calling "lopen" with its interval and ending ops, and also its own result (with extra argument added if necessary).  Note that we don't incorporate d.tightness here; it is only used to find the delimiter of the right-hand argument if the notation we parsed was right-open.  In particular, therefore, a right-closed notation can be followed by anything, even a left-open notation that binds tighter than it does; the only restriction is if we're inside the right-hand argument of some containing right-open notation, so we inherit a "tight" from there.  *)
    lopen tight stop res

  (* Parse a possibly-empty sequence of nonempty superscripts. *)
  and supers : (Asai.Range.t * string * Whitespace.t list) list t =
    zero_or_more
      (let* loc, res =
         located
           (step (fun state rng (tok, ws) ->
                match tok with
                | Superscript s -> Some (Ok (s, ws), state)
                | Invalid_superscript s ->
                    Some (Error (SemanticError.Invalid_degeneracy (rng, s)), state)
                | _ -> None)) in
       match res with
       | Ok (s, ws) -> return (loc, s, ws)
       | Error e -> fail e)

  (* Given a parsed term and a possibly-empty list of superscripts, tack them all onto the term sequentially. *)
  and superify :
      type lt ls.
      (lt, ls) right_wrapped_parse ->
      (Asai.Range.t * string * Whitespace.t list) list ->
      (lt, ls) right_wrapped_parse =
   fun arg sups ->
    match sups with
    | [] -> arg
    | (loc, s, ws) :: sups ->
        superify
          {
            get =
              (fun _ ->
                match arg.get Interval.empty with
                | Ok x -> Ok { value = Superscript (Some x, s, ws); loc = Some loc }
                | Error e -> Error e);
          }
          sups

  and with_supers : type lt ls. (lt, ls) right_wrapped_parse -> (lt, ls) right_wrapped_parse t =
   fun arg ->
    let* sups = supers in
    return (superify arg sups)

  (* "lopen" is passed an upper tightness interval and a set of ending ops, plus a parsed result for the left open argument and the tightness of the outermost notation in that argument if it is right-open. *)
  and lopen :
      type lt ls rt rs.
      (lt, ls) Interval.tt ->
      (rt, rs) tree TokMap.t ->
      (lt, ls) right_wrapped_parse ->
      (lt, ls) right_wrapped_parse t =
   fun tight stop first_arg ->
    match Interval.contains tight No.plus_omega with
    (* If the left tightness interval is the empty one (+ω,+ω], we aren't allowed to go on at all.  Otherwise, we need to get a witness of nonemptiness of that interval, for the case when we end up with an application. *)
    | None -> succeed first_arg
    | Some nontrivial ->
        (* Now we start by looking ahead one token.  If we see one of the specified ending ops, or the initial op of a left-open tree with looser tightness than the lower endpoint of the current interval (with strictness determined by the tree in question), we return the result argument without parsing any more.  Note that the order matters, in case the next token could have more than one role.  Ending ops are tested first, which means that if a certain operator could end an "inner term" in an outer containing notation, it always does, even if it could also be interpreted as some infix notation inside that inner term.  If a certain token could be the initial op of more than one left-open, we stop here if *any* of those is looser; we don't backtrack and try other possibilities.  So the rule is that if multiple notations start with the same token, the looser one is used preferentially in cases when it matters.  (In cases where it doesn't matter, i.e. they would both be allowed at the same grouping relative to other notations, we can proceed to parse a merged tree containing both of them and decide later which one it is.)  *)
        followed_by
          (step (fun state _ (tok, _) ->
               if TokMap.mem tok stop then Some (first_arg, state)
               else
                 let open Monad.Ops (Monad.Maybe) in
                 let* (Interval ivl) = State.Current.left_opens tok in
                 let t = tight.endpoint in
                 let* _ = Interval.contains ivl t in
                 return (first_arg, state)))
        (* Otherwise, we parse either an arbitrary left-closed tree (applying the given result to it as a function) or an arbitrary left-open tree with tightness in the given interval (passing the given result as the starting open argument).  Interior terms are treated as in "lclosed".  *)
        </> (let* res =
               (let* inner_loc, (inner, ws, notn) = located (entry (State.Current.tighters tight)) in
                let ws = Bwd.to_list ws in
                match notn with
                | Open_in_interval (left_ok, notn) -> (
                    match (first_arg.get (interval_left notn), right notn) with
                    | Error e, _ -> fail (No_relative_precedence (inner_loc, e, name notn))
                    | Ok first, Closed ->
                        with_supers
                          {
                            get =
                              (fun _ ->
                                Ok
                                  {
                                    value = postfix ~notn ~ws ~first ~inner ~left_ok;
                                    loc = Range.merge_opt first.loc (Some inner_loc);
                                  });
                          }
                    | Ok first, Open _ ->
                        let* last_arg = lclosed (interval_right notn) stop in
                        return
                          {
                            get =
                              (fun ivl ->
                                match
                                  (last_arg.get ivl, Interval.contains ivl (tightness notn))
                                with
                                | Ok last, Some right_ok ->
                                    Ok
                                      {
                                        value =
                                          infix ~notn ~ws ~first ~inner ~last ~left_ok ~right_ok;
                                        loc = Range.merge_opt3 first.loc (Some inner_loc) last.loc;
                                      }
                                | Error e, _ -> Error e
                                | _, None -> Error (name notn));
                          })
                | Closed_in_interval notn -> (
                    match (first_arg.get Interval.plus_omega_only, right notn) with
                    | Error e, _ -> fail (No_relative_precedence (inner_loc, e, "application"))
                    | Ok fn, Closed ->
                        let* sups = supers in
                        return
                          {
                            get =
                              (fun ivl ->
                                match Interval.contains ivl No.plus_omega with
                                | None -> Error "application 1"
                                | Some right_ok -> (
                                    let arg =
                                      {
                                        get =
                                          (fun _ -> Ok (locate inner_loc (outfix ~notn ~ws ~inner)));
                                      } in
                                    let arg = superify arg sups in
                                    match arg.get ivl with
                                    | Ok arg ->
                                        let value =
                                          App { fn; arg; left_ok = nontrivial; right_ok } in
                                        let loc = Range.merge_opt fn.loc (Some inner_loc) in
                                        Ok { value; loc }
                                    | Error e -> Error e));
                          }
                    | Ok fn, Open _ ->
                        let* last_arg = lclosed (interval_right notn) stop in
                        return
                          {
                            get =
                              (fun ivl ->
                                match
                                  ( last_arg.get ivl,
                                    Interval.contains ivl (tightness notn),
                                    Interval.contains ivl No.plus_omega )
                                with
                                | Ok last, Some right_ok, Some right_app ->
                                    let arg_loc = Range.merge_opt (Some inner_loc) last.loc in
                                    Ok
                                      {
                                        value =
                                          App
                                            {
                                              fn;
                                              arg =
                                                {
                                                  value = prefix ~notn ~ws ~inner ~last ~right_ok;
                                                  loc = arg_loc;
                                                };
                                              left_ok = nontrivial;
                                              right_ok = right_app;
                                            };
                                        loc = Range.merge_opt fn.loc arg_loc;
                                      }
                                | Error e, _, _ -> Error e
                                | _, None, _ -> Error (name notn)
                                | _, _, None -> Error "application 2");
                          }))
               (* If this fails, we can parse a single variable name, constr, or field projection and apply the first term to it.  Constructors are allowed here because they might have no arguments. *)
               </> let* arg_loc, (arg, w) =
                     located
                       (step (fun state _ (tok, w) ->
                            match tok with
                            | Ident x -> Some ((`Ident x, w), state)
                            (* Constructor and field names have already been validated by the lexer. *)
                            | Constr x -> Some ((`Constr x, w), state)
                            | Underscore -> Some ((`Placeholder, w), state)
                            | Field (x, p) -> Some ((`Field (x, p), w), state)
                            | _ -> None)) in
                   let* sups = supers in
                   match first_arg.get Interval.plus_omega_only with
                   | Error e -> fail (No_relative_precedence (arg_loc, e, "application"))
                   | Ok fn ->
                       return
                         {
                           get =
                             (fun ivl ->
                               match Interval.contains ivl No.plus_omega with
                               | Some right_ok -> (
                                   let arg =
                                     superify
                                       {
                                         get =
                                           (fun _ ->
                                             Ok
                                               (locate arg_loc
                                                  (match arg with
                                                  | `Ident x -> Ident (x, w)
                                                  | `Constr x -> Constr (x, w)
                                                  | `Placeholder -> Placeholder w
                                                  | `Field (x, p) -> Field (x, p, w))));
                                       }
                                       sups in
                                   match arg.get ivl with
                                   | Ok arg ->
                                       Ok
                                         {
                                           value = App { fn; arg; left_ok = nontrivial; right_ok };
                                           loc = Range.merge_opt fn.loc (Some arg_loc);
                                         }
                                   | Error e -> Error e)
                               | None -> Error "application 3");
                         } in
             (* Same comment here about carrying over "tight" as in lclosed. *)
             lopen tight stop res)
        (* If that also fails, another possibility is that we're at the end of the term with no more operators to parse, so we can just return the supplied "first argument". *)
        </> succeed first_arg

  (* The master term-parsing combinator parses an lclosed of arbitrary tightness, with specified ending tokens.  If the ending tokens are empty, it must extend until the next token that can't be part of a term (like a command name or EOF).  It does NOT parse the initial Bof token, since it can also appear as part of a command. *)
  let term toks =
    let tokmap =
      List.fold_left
        (fun map tok ->
          TokMap.add tok (Lazy (lazy (fatal (Anomaly "dummy notation tree accessed")))) map)
        TokMap.empty toks in
    let* tm = lclosed Interval.entire tokmap in
    match tm.get Interval.entire with
    | Ok tm -> return (Term tm)
    | Error e -> fatal (Anomaly ("Outer term failed: " ^ e))

  module Lex_and_parse =
    Parse_with_lexer.Make_utf8 (Unit) (Lexer.Token_whitespace) (Final) (SemanticError)
      (Lexer.Parser)
      (Basic.Parser)

  let ensure_success p =
    let open Lex_and_parse in
    (* Fmlib_parse has its own built-in error reporting with locations.  However, we instead use Asai's error reporting, so that we have a common "look" for parse errors and typechecking errors. *)
    if has_failed_syntax p then
      (* It should be possible to report more detailed error information from the parser than just the location.  Fmlib supplies "failed_expectations", but I haven't been able to figure out how to make that useful with this parsing algorithm. *)
      fatal ~loc:(Range.convert (range p)) Parse_error
    else if has_failed_semantic p then
      match failed_semantic p with
      | No_relative_precedence (loc, n1, n2) -> fatal ~loc (No_relative_precedence (n1, n2))
      | Invalid_degeneracy (rng, str) -> fatal ~loc:(Range.convert rng) (Invalid_degeneracy str)
    else if has_succeeded p then p
    else if needs_more p then fatal (Anomaly "parser needs more")
    else fatal (Anomaly "what")

  (* Strip off the initial Bof token attached to initial comments and whitespace, and return that whitespace. *)
  let bof = step (fun state _ (tok, ws) -> if tok = Bof then Some (ws, state) else None)

  (* TODO: Save the whitespace! *)
  let term_only () =
    let* _ = bof in
    term []
end

module Parse_term = struct
  module C = Combinators (ParseTree)

  let parse (source : Asai.Range.source) : C.Lex_and_parse.t =
    let (env : Range.Data.t), run =
      match source with
      | `String src ->
          ( { source = `String src; length = Int64.of_int (String.length src.content) },
            fun p -> C.Lex_and_parse.run_on_string src.content p )
      | `File name ->
          let ic = In_channel.open_text name in
          ( { source = `File name; length = In_channel.length ic },
            fun p -> C.Lex_and_parse.run_on_channel ic p ) in
    Range.run ~env @@ fun () ->
    let p = C.Lex_and_parse.make Lexer.Parser.start (C.Basic.make () (C.term_only ())) in
    let p = run p in
    C.ensure_success p

  let final p = C.Lex_and_parse.final p
  let has_consumed_end p = C.Lex_and_parse.has_consumed_end p
end

module Parse_command = struct
  module C = Combinators (Command)
  open C.Basic

  let token x = step "" (fun state _ (tok, w) -> if tok = x then Some (w, state) else None)

  let ident =
    step "" (fun state _ (tok, w) ->
        match tok with
        | Ident name -> Some ((name, w), state)
        | _ -> None)

  let variable =
    step "" (fun state _ (tok, w) ->
        match tok with
        | Ident [ x ] -> Some ((Some x, w), state)
        | Ident xs -> fatal (Invalid_variable xs)
        | Underscore -> Some ((None, w), state)
        | _ -> None)

  let parameter =
    let* wslparen = token LParen in
    let* name, names = one_or_more variable in
    let names = name :: names in
    let* wscolon = token Colon in
    let* ty = C.term [ RParen ] in
    let* wsrparen = token RParen in
    return ({ wslparen; names; wscolon; ty; wsrparen } : Parameter.t)

  let axiom =
    let* wsaxiom = token Axiom in
    let* name, wsname = ident in
    let* parameters = zero_or_more parameter in
    let* wscolon = token Colon in
    let* ty = C.term [] in
    return (Command.Axiom { wsaxiom; name; wsname; parameters; wscolon; ty })

  let def tok =
    let* wsdef = token tok in
    let* name, wsname = ident in
    let* parameters = zero_or_more parameter in
    let* wscolon, ty, wscoloneq, tm =
      (let* wscolon = token Colon in
       let* ty = C.term [ Coloneq ] in
       let* wscoloneq = token Coloneq in
       let* tm = C.term [] in
       return (wscolon, Some ty, wscoloneq, tm))
      </>
      let* wscoloneq = token Coloneq in
      let* tm = C.term [] in
      return ([], None, wscoloneq, tm) in
    return ({ wsdef; name; wsname; parameters; wscolon; ty; wscoloneq; tm } : Command.def)

  let def_and =
    let* first = def Def in
    let* rest = zero_or_more (def And) in
    return (Command.Def (first :: rest))

  let echo =
    let* wsecho = token Echo in
    let* tm = C.term [] in
    return (Command.Echo { wsecho; tm })

  let tightness_and_name :
      (No.wrapped option * Whitespace.t list * Scope.Trie.path * Whitespace.t list) t =
    let* tloc, tight_or_name = located ident in
    (let* name, wsname = ident in
     let tight, wstight = tight_or_name in
     let tight = String.concat "." tight in
     match No.of_rat (Q.of_string tight) with
     | Some tight -> return (Some tight, wstight, name, wsname)
     | None | (exception Invalid_argument _) ->
         fatal ~loc:(Range.convert tloc) (Invalid_tightness tight))
    </>
    let name, wsname = tight_or_name in
    return (None, [], name, wsname)

  let pattern_token =
    step "" (fun state _ (tok, ws) ->
        match tok with
        | String str -> (
            match Lexer.single str with
            | Some tok -> Some (`Op (tok, `Nobreak, ws), state)
            | None -> fatal (Invalid_notation_symbol str))
        | _ -> None)

  let pattern_var =
    let* x, ws = ident in
    match x with
    | [ x ] -> return (`Var (x, `Break, ws))
    | _ -> fatal (Invalid_variable x)

  let pattern_ellipsis =
    let* ws = token Ellipsis in
    return (`Ellipsis ws)

  let fixity_of_pattern pat tight =
    match (pat, Bwd.of_list pat, tight) with
    | `Op _ :: _, Snoc (_, `Op _), None -> (Fixity Outfix, pat, [])
    | `Op _ :: _, Snoc (_, `Var _), Some (No.Wrap tight) -> (Fixity (Prefix tight), pat, [])
    | `Op _ :: _, Snoc (bwd_pat, `Ellipsis ws), Some (No.Wrap tight) ->
        (Fixity (Prefixr tight), Bwd.to_list bwd_pat, ws)
    | `Var _ :: _, Snoc (_, `Op _), Some (No.Wrap tight) -> (Fixity (Postfix tight), pat, [])
    | `Ellipsis ws :: pat, Snoc (_, `Op _), Some (No.Wrap tight) ->
        (Fixity (Postfixl tight), pat, ws)
    | `Var _ :: _, Snoc (_, `Var _), Some (No.Wrap tight) -> (Fixity (Infix tight), pat, [])
    | `Ellipsis ws :: pat, Snoc (_, `Var _), Some (No.Wrap tight) -> (Fixity (Infixl tight), pat, ws)
    | `Var _ :: _, Snoc (bwd_pat, `Ellipsis ws), Some (No.Wrap tight) ->
        (Fixity (Infixr tight), Bwd.to_list bwd_pat, ws)
    | [ `Ellipsis _ ], Snoc (Emp, `Ellipsis _), _ -> fatal Zero_notation_symbols
    | `Ellipsis _ :: _, Snoc (_, `Ellipsis _), _ -> fatal Ambidextrous_notation
    | _ -> fatal Fixity_mismatch

  let notation_head =
    step "" (fun state _ (tok, ws) ->
        match tok with
        | Ident name -> Some ((`Constant name, ws), state)
        | Constr c -> Some ((`Constr c, ws), state)
        | _ -> None)

  let notation_var =
    let* x, ws = ident in
    match x with
    | [ x ] -> return (x, ws)
    | _ -> fatal (Invalid_variable x)

  let notation : Command.t C.Basic.t =
    let* wsnotation = token Notation in
    let* tight, wstight, name, wsname = tightness_and_name in
    let* wscolon = token Colon in
    let* pat, pattern = one_or_more (pattern_token </> pattern_var </> pattern_ellipsis) in
    let pattern = pat :: pattern in
    let Fixity fixity, pattern, wsellipsis = fixity_of_pattern pattern tight in
    let* wscoloneq = token Coloneq in
    let* head, wshead = notation_head in
    let* args = zero_or_more notation_var in
    return
      (Command.Notation
         {
           fixity;
           wsnotation;
           wstight;
           wsellipsis;
           name;
           wsname;
           wscolon;
           pattern;
           wscoloneq;
           head;
           wshead;
           args;
         })

  let bof =
    let* ws = C.bof in
    return (Command.Bof ws)

  let eof =
    let* () = expect_end () in
    return Command.Eof

  let command : unit -> Command.t C.Basic.t =
   fun () -> bof </> axiom </> def_and </> echo </> notation </> eof

  let command_or_echo : unit -> Command.t C.Basic.t =
   fun () ->
    command ()
    </> let* tm = C.term [] in
        return (Command.Echo { wsecho = []; tm })

  type open_source = Range.Data.t * [ `String of int * string | `File of In_channel.t ]

  let start_parse ?(or_echo = false) source : C.Lex_and_parse.t * open_source =
    let (env : Range.Data.t), run =
      match source with
      | `String src ->
          ( { source = `String src; length = Int64.of_int (String.length src.content) },
            fun p ->
              let n, p = C.Lex_and_parse.run_on_string_at 0 src.content p in
              (`String (n, src.content), p) )
      | `File name ->
          let ic = In_channel.open_text name in
          ( { source = `File name; length = In_channel.length ic },
            fun p -> (`File ic, C.Lex_and_parse.run_on_channel ic p) ) in
    Range.run ~env @@ fun () ->
    let p =
      C.Lex_and_parse.make Lexer.Parser.start
        (C.Basic.make_partial () (if or_echo then command_or_echo () else command ())) in
    let out, p = run p in
    (C.ensure_success p, (env, out))

  let restart_parse ?(or_echo = false) (p : C.Lex_and_parse.t) ((env, source) : open_source) :
      C.Lex_and_parse.t * open_source =
    let run =
      match source with
      | `String (n, content) ->
          fun p ->
            let n, p = C.Lex_and_parse.run_on_string_at n content p in
            (`String (n, content), p)
      | `File ic -> fun p -> (`File ic, C.Lex_and_parse.run_on_channel ic p) in
    Range.run ~env @@ fun () ->
    let p =
      C.Lex_and_parse.make_next p
        (C.Basic.make_partial () (if or_echo then command_or_echo () else command ())) in
    let out, p = run p in
    (C.ensure_success p, (env, out))

  let final p = C.Lex_and_parse.final p
  let has_consumed_end p = C.Lex_and_parse.has_consumed_end p
end

let parse_single_command (content : string) : Whitespace.t list * Command.t option =
  let src : Asai.Range.source = `String { content; title = Some "interactive input" } in
  let p, src = Parse_command.start_parse ~or_echo:true src in
  match Parse_command.final p with
  | Bof ws ->
      let p, src = Parse_command.restart_parse ~or_echo:true p src in
      let cmd = Parse_command.final p in
      if cmd <> Eof then
        let p, _ = Parse_command.restart_parse ~or_echo:true p src in
        let eof = Parse_command.final p in
        if eof = Eof then (ws, Some cmd) else Core.Reporter.fatal Too_many_commands
      else (ws, None)
  | _ -> Core.Reporter.fatal (Anomaly "interactive parse doesn't start with Bof")
