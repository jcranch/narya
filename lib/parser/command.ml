open Bwd
open Dim
open Util
open List_extra
open Core
open Notation
open Postprocess
open Unparse
open Format
open Uuseg_string
open Print
open Reporter
open User
open Modifier
module Trie = Yuujinchou.Trie

type def = {
  wsdef : Whitespace.t list;
  name : Trie.path;
  wsname : Whitespace.t list;
  parameters : Parameter.t list;
  wscolon : Whitespace.t list;
  ty : observation option;
  wscoloneq : Whitespace.t list;
  tm : observation;
}

module Command = struct
  type t =
    | Axiom of {
        wsaxiom : Whitespace.t list;
        name : Trie.path;
        wsname : Whitespace.t list;
        parameters : Parameter.t list;
        wscolon : Whitespace.t list;
        ty : observation;
      }
    | Def of def list
    | Echo of { wsecho : Whitespace.t list; tm : observation }
    | Notation : {
        fixity : ('left, 'tight, 'right) fixity;
        wsnotation : Whitespace.t list;
        wstight : Whitespace.t list; (* Empty for outfix *)
        wsellipsis : Whitespace.t list; (* Empty for non-associative *)
        name : Trie.path;
        wsname : Whitespace.t list;
        wscolon : Whitespace.t list;
        pattern : User.pattern;
        wscoloneq : Whitespace.t list;
        head : [ `Constr of string | `Constant of Trie.path ];
        wshead : Whitespace.t list;
        args : (string * Whitespace.t list) list;
      }
        -> t
    | Import of {
        wsimport : Whitespace.t list;
        export : bool;
        origin : [ `File of string | `Path of Trie.path ];
        wsorigin : Whitespace.t list;
        op : (Whitespace.t list * modifier) option;
      }
    | Solve of {
        wssolve : Whitespace.t list;
        number : int;
        wsnumber : Whitespace.t list;
        wscoloneq : Whitespace.t list;
        tm : observation;
      }
    | Show of {
        wsshow : Whitespace.t list;
        what : [ `Hole of Whitespace.t list * int | `Holes ];
        wswhat : Whitespace.t list;
      }
    | Quit of Whitespace.t list
    | Bof of Whitespace.t list
    | Eof
end

include Command

module Parse = struct
  open Parse
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
    return ({ wsdef; name; wsname; parameters; wscolon; ty; wscoloneq; tm } : def)

  let def_and =
    let* first = def Def in
    let* rest = zero_or_more (def And) in
    return (Command.Def (first :: rest))

  let echo =
    let* wsecho = token Echo in
    let* tm = C.term [] in
    return (Command.Echo { wsecho; tm })

  let tightness_and_name : (No.wrapped option * Whitespace.t list * Trie.path * Whitespace.t list) t
      =
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
            (* Currently we hard code a `Nobreak after each symbol in a notation. *)
            | Some tok -> Some (`Op (tok, `Nobreak, ws), state)
            | None -> fatal (Invalid_notation_symbol str))
        | _ -> None)

  let pattern_var =
    let* x, ws = ident in
    match x with
    (* Currently we hard code a `Break after each variable in a notation. *)
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

  let notation =
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

  let path =
    ident
    </> let* wsdot = token Dot in
        return ([], wsdot)

  let rec modifier () =
    let* m =
      step "" (fun state _ (tok, ws) ->
          match tok with
          | Ident [ "all" ] -> Some (`All ws, state)
          | Ident [ "id" ] -> Some (`Id ws, state)
          | Ident [ "only" ] -> Some (`Only ws, state)
          | In -> Some (`In ws, state)
          | Ident [ "none" ] -> Some (`None ws, state)
          | Ident [ "except" ] -> Some (`Except ws, state)
          | Ident [ "renaming" ] -> Some (`Renaming ws, state)
          | Ident [ "seq" ] -> Some (`Seq ws, state)
          | Ident [ "union" ] -> Some (`Union ws, state)
          | _ -> None) in
    match m with
    | `All wsall -> return (All { wsall })
    | `Id wsid -> return (Id { wsid })
    | `Only wsonly ->
        let* path, wspath = path in
        return (Only { wsonly; path; wspath })
    | `In wsin ->
        let* path, wspath = path in
        let* op = modifier () in
        return (In { wsin; path; wspath; op })
    | `None wsnone -> return (MNone { wsnone })
    | `Except wsexcept ->
        let* path, wspath = path in
        return (Except { wsexcept; path; wspath })
    | `Renaming wsrenaming ->
        let* source, wssource = path in
        let* target, wstarget = path in
        return (Renaming { wsrenaming; source; wssource; target; wstarget })
    | `Seq wsseq ->
        let* wslparen = token LParen in
        let* ops =
          zero_or_more_fold_left Emp
            (fun x y -> return (Snoc (x, y)))
            (backtrack
               (let* op = modifier () in
                let* wssemi = token (Op ",") in
                return (op, wssemi))
               "") in
        let* lastop = optional (modifier ()) in
        let ops =
          Bwd.fold_right
            (fun x y -> x :: y)
            ops
            (Option.fold ~none:[] ~some:(fun x -> [ (x, []) ]) lastop) in
        let* wsrparen = token RParen in
        return (Seq { wsseq; wslparen; ops; wsrparen })
    | `Union wsunion ->
        let* wslparen = token LParen in
        let* ops =
          zero_or_more_fold_left Emp
            (fun x y -> return (Snoc (x, y)))
            (backtrack
               (let* op = modifier () in
                let* wssemi = token (Op ",") in
                return (op, wssemi))
               "") in
        let* lastop = optional (modifier ()) in
        let ops =
          Bwd.fold_right
            (fun x y -> x :: y)
            ops
            (Option.fold ~none:[] ~some:(fun x -> [ (x, []) ]) lastop) in
        let* wsrparen = token RParen in
        return (Union { wsunion; wslparen; ops; wsrparen })

  let import =
    let* wsimport, export =
      (let* wsimport = token Import in
       return (wsimport, false))
      </> let* wsimport = token Export in
          return (wsimport, true) in
    let* origin, wsorigin =
      step "" (fun state _ (tok, ws) ->
          match tok with
          | String file -> Some ((`File file, ws), state)
          | Ident x -> Some ((`Path x, ws), state)
          | Dot -> Some ((`Path [], ws), state)
          | _ -> None) in
    let* op =
      optional
        (backtrack
           (let* wsbar = token (Op "|") in
            let* m = modifier () in
            return (wsbar, m))
           "") in
    return (Import { wsimport; export; origin; wsorigin; op })

  let integer =
    step "" (fun state _ (tok, ws) ->
        match tok with
        | Ident [ num ] -> Some ((int_of_string num, ws), state)
        | _ -> None)

  let solve =
    let* wssolve = token Solve in
    let* number, wsnumber = integer in
    let* wscoloneq = token Coloneq in
    let* tm = C.term [] in
    return (Solve { wssolve; number; wsnumber; wscoloneq; tm })

  let show =
    let* wsshow = token Show in
    let* what =
      step "" (fun state _ (tok, ws) ->
          match tok with
          | Ident [ "hole" ] -> Some (`Hole ws, state)
          | Ident [ "holes" ] -> Some (`Holes ws, state)
          | _ -> None) in
    let* what, wswhat =
      match what with
      | `Hole ws ->
          let* number, wsnumber = integer in
          return (`Hole (ws, number), wsnumber)
      | `Holes ws -> return (`Holes, ws) in
    return (Show { wsshow; what; wswhat })

  let quit =
    let* wsquit = token Quit in
    return (Command.Quit wsquit)

  let bof =
    let* ws = C.bof in
    return (Command.Bof ws)

  let eof =
    let* () = expect_end () in
    return Command.Eof

  let command () =
    bof </> axiom </> def_and </> echo </> notation </> import </> solve </> show </> quit </> eof

  let command_or_echo () =
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
      | `File name -> (
          try
            let ic = In_channel.open_text name in
            ( { source = `File name; length = In_channel.length ic },
              fun p -> (`File ic, C.Lex_and_parse.run_on_channel ic p) )
          with Sys_error _ -> fatal (No_such_file name)) in
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

let parse_single (content : string) : Whitespace.t list * Command.t option =
  let src : Asai.Range.source = `String { content; title = Some "interactive input" } in
  let p, src = Parse.start_parse ~or_echo:true src in
  match Parse.final p with
  | Bof ws ->
      let p, src = Parse.restart_parse ~or_echo:true p src in
      let cmd = Parse.final p in
      if cmd <> Eof then
        let p, _ = Parse.restart_parse ~or_echo:true p src in
        let eof = Parse.final p in
        if eof = Eof then (ws, Some cmd) else Core.Reporter.fatal Too_many_commands
      else (ws, None)
  | _ -> Core.Reporter.fatal (Anomaly "interactive parse doesn't start with Bof")

(* Put a given starting namespace into the scope, and also extract the notations from it.  TODO: It would be nice to make this be just Scope.run, but that creates a dependency cycle with Postprocess and Builtins. *)
let run_with_scope ~init_visible f =
  Scope.run ~init_visible @@ fun () ->
  Situation.run_on
    (Seq.fold_left
       (fun state (_, (data, _)) ->
         match data with
         | `Notation (user, _) -> fst (Situation.add_user user state)
         | _ -> state)
       !Builtins.builtins
       (Trie.to_seq (Trie.find_subtree [ "notations" ] init_visible)))
  @@ f

let show_hole ppf err = function
  | Eternity.Find_number (m, Wrap (Metadef { data = Undef_meta { vars; _ }; termctx; ty }), _) ->
      pp_open_vbox ppf 0;
      fprintf ppf "hole %s:" (Meta.name m);
      pp_print_newline ppf ();
      pp_print_newline ppf ();
      pp_printed ppf (Reporter.print (Termctx.PHole (vars, termctx, ty)));
      pp_close_box ppf ();
      pp_print_newline ppf ();
      pp_print_newline ppf ()
  | _ -> fatal err

let to_string : Command.t -> string = function
  | Axiom _ -> "axiom"
  | Def _ -> "def"
  | Echo _ -> "echo"
  | Notation _ -> "notation"
  | Import _ -> "import"
  | Solve _ -> "solve"
  | Show _ -> "show"
  | Quit _ -> "quit"
  | Bof _ -> "bof"
  | Eof -> "eof"

(* Whether a command requires an interactive mode (i.e. not interactive mode and not ProofGeneral interaction). *)
let needs_interactive : Command.t -> bool = function
  | Solve _ | Show _ -> true
  | _ -> false

(* Most execution of commands we can do here, but there are a couple things where we need to call out to the executable: noting when an effectual action like 'echo' is taken (for recording warnings in compiled files), and loading another file.  So this function takes a couple of callbacks as arguments. *)
let execute :
    action_taken:(unit -> unit) ->
    get_file:(string -> (Scope.P.data, Scope.P.tag) Trie.t) ->
    Command.t ->
    unit =
 fun ~action_taken ~get_file cmd ->
  if needs_interactive cmd && not (Core.Command.Mode.read ()).interactive then
    fatal (Forbidden_interactive_command (to_string cmd));
  match cmd with
  | Axiom { name; parameters; ty = Term ty; _ } ->
      Scope.check_constant_name name;
      let const = Scope.define (Compunit.Current.read ()) name in
      Reporter.try_with ~fatal:(fun d ->
          Scope.modify_visible (Yuujinchou.Language.except name);
          Scope.modify_export (Yuujinchou.Language.except name);
          Reporter.fatal_diagnostic d)
      @@ fun () ->
      let (Processed_tel (params, ctx)) = process_tel Emp parameters in
      Core.Command.execute (Axiom (const, params, process ctx ty))
  | Def defs ->
      let [ names; cdefs ] =
        Mlist.pmap
          (fun [ d ] ->
            Scope.check_constant_name d.name;
            let c = Scope.define (Compunit.Current.read ()) d.name in
            [ d.name; (c, d) ])
          [ defs ] (Cons (Cons Nil)) in
      Reporter.try_with ~fatal:(fun d ->
          List.iter
            (fun c ->
              Scope.modify_visible (Yuujinchou.Language.except c);
              Scope.modify_export (Yuujinchou.Language.except c))
            names;
          Reporter.fatal_diagnostic d)
      @@ fun () ->
      let defs =
        List.map
          (function
            | const, { parameters; ty; tm = Term tm; _ } -> (
                let (Processed_tel (params, ctx)) = process_tel Emp parameters in
                match ty with
                | Some (Term ty) ->
                    ( const,
                      Core.Command.Def_check { params; ty = process ctx ty; tm = process ctx tm } )
                | None -> (
                    match process ctx tm with
                    | { value = Synth tm; loc } ->
                        (const, Def_synth { params; tm = { value = tm; loc } })
                    | _ -> fatal (Nonsynthesizing "body of def without specified type"))))
          cdefs in
      Core.Command.execute (Def defs)
  | Echo { tm = Term tm; _ } -> (
      let rtm = process Emp tm in
      action_taken ();
      match rtm.value with
      | Synth stm ->
          Readback.Display.run ~env:true @@ fun () ->
          let ctm, ety = Check.synth (Kinetic `Nolet) Ctx.empty { value = stm; loc = rtm.loc } in
          let etm = Norm.eval_term (Emp D.zero) ctm in
          let btm = Readback.readback_at Ctx.empty etm ety in
          let bty = Readback.readback_at Ctx.empty ety (Syntax.universe D.zero) in
          let utm = unparse Names.empty btm Interval.entire Interval.entire in
          let uty = unparse Names.empty bty Interval.entire Interval.entire in
          let ppf = Format.std_formatter in
          pp_open_vbox ppf 2;
          pp_term `None ppf (Term utm);
          pp_print_cut ppf ();
          pp_tok ppf Colon;
          pp_print_string ppf " ";
          pp_term `None ppf (Term uty);
          pp_close_box ppf ();
          pp_print_newline ppf ();
          pp_print_newline ppf ()
      | _ -> fatal (Nonsynthesizing "argument of echo"))
  | Notation { fixity; name; pattern; head; args; _ } ->
      let notation_name = "notations" :: name in
      (* A notation name can't be redefining anything. *)
      if Option.is_some (Scope.lookup notation_name) then
        fatal ~severity:Asai.Diagnostic.Error
          (Name_already_defined (String.concat "." notation_name));
      let key =
        match head with
        | `Constr c -> `Constr (Constr.intern c, List.length args)
        | `Constant c -> (
            match Scope.lookup c with
            | Some c -> `Constant c
            | None -> fatal (Invalid_notation_head (String.concat "." c))) in
      let unbound, _ =
        List.fold_left
          (fun (args, seen) item ->
            match item with
            | `Var (x, _, _) -> (
                if List.mem x seen then fatal (Duplicate_notation_variable x);
                let found, rest = List.partition (fun (y, _) -> x = y) args in
                match found with
                | [ _ ] -> (rest, x :: seen)
                | [] -> fatal (Unused_notation_variable x)
                | _ -> fatal (Notation_variable_used_twice x))
            | `Op _ | `Ellipsis _ -> (args, seen))
          (args, []) pattern in
      if not (List.is_empty unbound) then
        fatal (Unbound_variable_in_notation (List.map fst unbound));
      let user =
        User { name = String.concat "." name; fixity; pattern; key; val_vars = List.map fst args }
      in
      let notn, shadow = Situation.Current.add_user user in
      Scope.include_singleton (notation_name, (`Notation (user, notn), ()));
      (if shadow then
         let keyname =
           match notn.key with
           | `Constr (c, _) -> Constr.to_string c ^ "."
           | `Constant c -> String.concat "." (Scope.name_of c) in
         emit (Head_already_has_notation keyname));
      emit (Notation_defined (String.concat "." name))
  | Import { export; origin; op; _ } ->
      let trie =
        match origin with
        | `File file ->
            if FilePath.check_extension file "ny" then emit (Library_has_extension file);
            let file = FilePath.add_extension file "ny" in
            get_file file
        | `Path path -> Trie.find_subtree path (Scope.get_visible ()) in
      let trie =
        match op with
        | Some (_, op) -> Scope.M.modify (process_modifier op) trie
        | None -> trie in
      if export then Scope.include_subtree ([], trie) else Scope.import_subtree ([], trie);
      Seq.iter
        (fun (_, (data, _)) ->
          match data with
          | `Notation (user, _) ->
              let _ = Situation.Current.add_user user in
              ()
          | _ -> ())
        (Trie.to_seq (Trie.find_subtree [ "notations" ] trie))
  | Solve { number; tm = Term tm; _ } -> (
      let (Find_number
            (m, Wrap (Metadef { data; termctx; ty }), ({ global; scope; discrete } : Eternity.data)))
          =
        Eternity.find_number number in
      match data with
      | Undef_meta { vars; status } ->
          run_with_scope ~init_visible:scope @@ fun () ->
          Core.Command.execute
            (Solve (global, status, termctx, process vars tm, ty, Eternity.solve m, discrete))
      | Def_meta _ ->
          (* Yes, this is an anomaly and not a user error, because find_number should only be looking at the unsolved holes. *)
          fatal (Anomaly "hole already defined"))
  | Show { what; _ } -> (
      match what with
      | `Hole (_, number) ->
          show_hole Format.std_formatter (No_such_hole number) (Eternity.find_number number)
      | `Holes -> (
          match Eternity.all_holes () with
          | [] ->
              fprintf Format.std_formatter "no open holes";
              pp_print_newline Format.std_formatter ();
              pp_print_newline Format.std_formatter ()
          | holes ->
              List.iter
                (show_hole Format.std_formatter (Anomaly "defined hole in undefined list"))
                holes))
  | Quit _ -> fatal (Quit None)
  | Bof _ -> ()
  | Eof -> fatal (Anomaly "EOF cannot be executed")

let tightness_of_fixity : type left tight right. (left, tight, right) fixity -> string option =
  function
  | Infix tight
  | Infixl tight
  | Infixr tight
  | Prefix tight
  | Prefixr tight
  | Postfix tight
  | Postfixl tight -> Some (No.to_string tight)
  | Outfix -> None

let pp_parameter : formatter -> Parameter.t -> unit =
 fun ppf { wslparen; names; wscolon; ty; wsrparen } ->
  pp_tok ppf LParen;
  pp_ws `None ppf wslparen;
  List.iter
    (fun (name, wsname) ->
      pp_var ppf name;
      pp_ws `Break ppf wsname)
    names;
  pp_tok ppf Colon;
  pp_ws `Nobreak ppf wscolon;
  pp_term `None ppf ty;
  pp_tok ppf RParen;
  pp_ws `Break ppf wsrparen

let rec pp_defs : formatter -> Token.t -> Whitespace.t list -> def list -> Whitespace.t list =
 fun ppf tok ws defs ->
  match defs with
  | [] -> ws
  | { wsdef; name; wsname; parameters; wscolon; ty; wscoloneq; tm = Term tm } :: defs ->
      pp_ws `None ppf ws;
      pp_open_hvbox ppf 2;
      pp_tok ppf tok;
      pp_ws `Nobreak ppf wsdef;
      pp_utf_8 ppf (String.concat "." name);
      pp_ws `Break ppf wsname;
      List.iter (pp_parameter ppf) parameters;
      (match ty with
      | Some ty ->
          pp_tok ppf Colon;
          pp_ws `Nobreak ppf wscolon;
          pp_term `Break ppf ty
      | None -> ());
      pp_tok ppf Coloneq;
      pp_ws `Nobreak ppf wscoloneq;
      let tm, rest = split_ending_whitespace tm in
      pp_term `None ppf (Term tm);
      pp_close_box ppf ();
      pp_defs ppf And rest defs

let pp_command : formatter -> t -> Whitespace.t list =
 fun ppf cmd ->
  match cmd with
  | Axiom { wsaxiom; name; wsname; parameters; wscolon; ty = Term ty } ->
      pp_open_hvbox ppf 2;
      pp_tok ppf Axiom;
      pp_ws `Nobreak ppf wsaxiom;
      pp_utf_8 ppf (String.concat "." name);
      pp_ws `Break ppf wsname;
      List.iter (pp_parameter ppf) parameters;
      pp_tok ppf Colon;
      pp_ws `Nobreak ppf wscolon;
      let ty, rest = split_ending_whitespace ty in
      pp_term `None ppf (Term ty);
      pp_close_box ppf ();
      rest
  | Def defs -> pp_defs ppf Def [] defs
  | Echo { wsecho; tm = Term tm } ->
      pp_open_hvbox ppf 2;
      pp_tok ppf Echo;
      pp_ws `Nobreak ppf wsecho;
      let tm, rest = split_ending_whitespace tm in
      pp_term `None ppf (Term tm);
      pp_close_box ppf ();
      rest
  | Notation
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
      } ->
      pp_open_hvbox ppf 2;
      pp_tok ppf Notation;
      pp_ws `Nobreak ppf wsnotation;
      (match tightness_of_fixity fixity with
      | Some str -> pp_print_string ppf str
      | None -> ());
      pp_ws `Nobreak ppf wstight;
      pp_utf_8 ppf (String.concat "." name);
      pp_ws `Break ppf wsname;
      pp_tok ppf Colon;
      pp_ws `Nobreak ppf wscolon;
      (match fixity with
      | Infixl _ | Postfixl _ ->
          pp_tok ppf Ellipsis;
          pp_ws `Break ppf wsellipsis
      | _ -> ());
      User.pp_pattern ppf pattern;
      (match fixity with
      | Infixr _ | Prefixr _ ->
          pp_tok ppf Ellipsis;
          pp_ws `Break ppf wsellipsis
      | _ -> ());
      pp_tok ppf Coloneq;
      pp_ws `Nobreak ppf wscoloneq;
      (match head with
      | `Constr c -> pp_constr ppf c
      | `Constant c -> pp_utf_8 ppf (String.concat "." c));
      let rest =
        match split_last args with
        | None ->
            let wshead, rest = Whitespace.split wshead in
            pp_ws `None ppf wshead;
            rest
        | Some (args, (last, wslast)) ->
            List.iter
              (fun (arg, wsarg) ->
                pp_utf_8 ppf arg;
                pp_ws `Break ppf wsarg)
              args;
            pp_utf_8 ppf last;
            let wslast, rest = Whitespace.split wslast in
            pp_ws `None ppf wslast;
            rest in
      pp_close_box ppf ();
      rest
  | Import { wsimport; export; origin; wsorigin; op } -> (
      pp_open_hvbox ppf 2;
      pp_tok ppf (if export then Export else Import);
      pp_ws `Nobreak ppf wsimport;
      (match origin with
      | `File file ->
          pp_print_string ppf "\"";
          pp_print_string ppf file;
          pp_print_string ppf "\""
      | `Path [] -> pp_tok ppf Dot
      | `Path path -> pp_utf_8 ppf (String.concat "." path));
      match op with
      | None ->
          let ws, rest = Whitespace.split wsorigin in
          pp_ws `None ppf ws;
          pp_close_box ppf ();
          rest
      | Some (wsbar, op) ->
          pp_ws `Break ppf wsorigin;
          pp_tok ppf (Op "|");
          pp_ws `Nobreak ppf wsbar;
          let ws, rest = Whitespace.split (pp_modifier ppf op) in
          pp_ws `None ppf ws;
          pp_close_box ppf ();
          rest)
  | Solve { wssolve; number; wsnumber; wscoloneq; tm = Term tm } ->
      pp_open_hvbox ppf 2;
      pp_tok ppf Solve;
      pp_ws `Nobreak ppf wssolve;
      pp_print_int ppf number;
      pp_ws `Break ppf wsnumber;
      pp_tok ppf Coloneq;
      pp_ws `Nobreak ppf wscoloneq;
      let tm, rest = split_ending_whitespace tm in
      pp_term `None ppf (Term tm);
      pp_close_box ppf ();
      rest
  | Show { wsshow; what; wswhat } ->
      pp_open_hvbox ppf 2;
      pp_tok ppf Show;
      pp_ws `Nobreak ppf wsshow;
      (match what with
      | `Hole (ws, number) ->
          pp_print_string ppf "hole";
          pp_ws `Nobreak ppf ws;
          pp_print_int ppf number
      | `Holes -> pp_print_string ppf "holes");
      let ws, rest = Whitespace.split wswhat in
      pp_ws `None ppf ws;
      rest
  | Quit ws -> ws
  | Bof ws -> ws
  | Eof -> []
