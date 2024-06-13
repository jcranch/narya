open Dim
open Asai.Diagnostic
open Format
open Uuseg_string

(* In order to display terms and suchlike in Asai messages, we utilize a double indirection.  Firstly, displaying a term requires "unparsing" it to a parse tree and then printing the parse tree, but parse trees and unparsing aren't defined until the Parser library, which is loaded after Core.  (Displaying a value additionally requires reading it back into a term, which is defined later in Core.)  For this reason, we introduce a wrapper type "printable" that can contain a term, value, etc.  Since terms and values haven't been defined yet in this file, we make "printable" an extensible variant, so they can be added later (in the module Printable) after they are defined.  (They also have to be bundled with their context.) *)

type printable = ..

type printable +=
  | PUnit : printable
  | PString : string -> printable
  | PConstant of Constant.t
  | PMeta : ('b, 's) Meta.t -> printable
  | PField : Field.t -> printable
  | PConstr : Constr.t -> printable

(* The function that actually does the work of printing a "printable" will be defined in Parser.Unparse.  But we need to be able to "call" that function in this file to define "default_text" that converts structured messages to text.  Thus, in this file we define a mutable global variable to contain that function, starting with a dummy function, and call its value to print "printable"s; then in Parser.Unparse we will set the value of that variable after defining the function it should contain. *)

(* Secondly, in Asai messages are emitted by performing an effect or raising an exception that carries with it the data of a function of type "formatter -> unit", which is then called by the handler Reporter.run to format the message text as part of a larger display formatting.  This causes problems if we define our printing functions naively, since it means that any effects performed by the formatting function (such as looking up names in a Yuujinchou Scope) will take place in the context of the handler, not that where the message was invoked, and hence in the wrong scope.  To deal with this, we introduce another wrapper "printed" that stores an existential type, a value of that type, and a function that can format a value of that type. *)

type printed = Printed : (formatter -> 'a -> unit) * 'a -> printed

(* Obviously the only thing that can be done with a value of type "printed" is to apply the function to the argument and a formatter.  But the virtue of wrapping it up this way is that the argument is evaluated when the "printed" value is computed, not when the function is called, and hence takes place in the correct scope.  The function to be defined in Parser.Unparse will thus have type "printable -> printed", and here is where we store it. *)

let printer : (printable -> printed) ref =
  ref (fun _ -> raise (Failure "print not set (hint: Parser.Unparse must be loaded)"))

(* Finally, here are some convenience functions. *)

let pp_printed ppf (Printed (pp, x)) = pp ppf x
let print pr = !printer pr

module Code = struct
  type t =
    | Parse_error : t
    | Encoding_error : t
    | Parsing_ambiguity : string -> t
    | No_relative_precedence : string * string -> t
    | Invalid_variable : string list -> t
    | Invalid_numeral : string -> t
    | Invalid_constr : string -> t
    | Invalid_field : string -> t
    | Invalid_degeneracy : string -> t
    | Not_enough_lambdas : int -> t
    | Not_enough_arguments_to_function : t
    | Not_enough_arguments_to_instantiation : t
    | Type_not_fully_instantiated : string -> t
    | Instantiating_zero_dimensional_type : printable -> t
    | Unequal_synthesized_type : printable * printable -> t
    | Checking_tuple_at_degenerated_record : printable -> t
    | Missing_field_in_tuple : Field.t -> t
    | Missing_method_in_comatch : Field.t -> t
    | Extra_field_in_tuple : Field.t option -> t
    | Extra_method_in_comatch : Field.t -> t
    | Invalid_field_in_tuple : t
    | Duplicate_field_in_tuple : Field.t -> t
    | Duplicate_method_in_codata : Field.t -> t
    | Duplicate_field_in_record : Field.t -> t
    | Invalid_method_in_comatch : t
    | Duplicate_method_in_comatch : Field.t -> t
    | Missing_constructor_in_match : Constr.t -> t
    | Unnamed_variable_in_match : t
    | Checking_lambda_at_nonfunction : printable -> t
    | Checking_tuple_at_nonrecord : printable -> t
    | Comatching_at_noncodata : printable -> t
    | Comatching_at_degenerated_codata : printable -> t
    | No_such_constructor :
        [ `Data of printable | `Nondata of printable | `Other of printable ] * Constr.t
        -> t
    | Wrong_number_of_arguments_to_constructor : Constr.t * int -> t
    | No_such_field :
        [ `Record of printable | `Nonrecord of printable | `Other | `Degenerated_record ]
        * Field.or_index
        -> t
    | Missing_instantiation_constructor :
        Constr.t * [ `Constr of Constr.t | `Nonconstr of printable ]
        -> t
    | Unequal_indices : printable * printable -> t
    | Unbound_variable : string -> t
    | Undefined_constant : printable -> t
    | Undefined_metavariable : printable -> t
    | Nonsynthesizing : string -> t
    | Low_dimensional_argument_of_degeneracy : (string * 'a D.t) -> t
    | Missing_argument_of_degeneracy : string -> t
    | Applying_nonfunction_nontype : printable * printable -> t
    | Unimplemented : string -> t
    | Matching_datatype_has_degeneracy : printable -> t
    | Wrong_number_of_arguments_to_pattern : Constr.t * int -> t
    | Wrong_number_of_arguments_to_motive : int -> t
    | No_such_constructor_in_match : printable * Constr.t -> t
    | Duplicate_constructor_in_match : Constr.t -> t
    | Duplicate_constructor_in_data : Constr.t -> t
    | Matching_on_nondatatype : printable -> t
    | Matching_wont_refine : string * printable -> t
    | Dimension_mismatch : string * 'a D.t * 'b D.t -> t
    | Invalid_variable_face : 'a D.t * ('n, 'm) sface -> t
    | Unsupported_numeral : Q.t -> t
    | Anomaly : string -> t
    | No_such_level : printable -> t
    | Constant_already_defined : string -> t
    | Invalid_constant_name : string -> t
    | Too_many_commands : t
    | Invalid_tightness : string -> t
    | Fixity_mismatch : t
    | Zero_notation_symbols : t
    | Missing_notation_symbol : t
    | Ambidextrous_notation : t
    | Invalid_notation_symbol : string -> t
    | Invalid_notation_head : string -> t
    | Duplicate_notation_variable : string -> t
    | Unused_notation_variable : string -> t
    | Notation_variable_used_twice : string -> t
    | Unbound_variable_in_notation : string list -> t
    | Head_already_has_notation : string -> t
    | Constant_assumed : printable * int -> t
    | Constant_defined : (printable * bool) list * int -> t
    | Notation_defined : string -> t
    | Show : string * printable -> t
    | Comment_end_in_string : t
    | Error_printing_error : t -> t
    | Checking_canonical_at_nonuniverse : string * printable -> t
    | Bare_case_tree_construct : string -> t
    | Wrong_boundary_of_record : int -> t
    | Invalid_constructor_type : Constr.t -> t
    | Missing_constructor_type : Constr.t -> t
    | Locked_variable : t
    | Locked_axiom : printable -> t
    | Hole_generated : ('b, 's) Meta.t * printable -> t
    | Open_holes : t
    | Quit : t
    | Synthesizing_recursion : printable -> t
    | Invalid_synthesized_type : string * printable -> t
    | Unrecognized_attribute : t
    | Invalid_degeneracy_action : string * 'nk D.t * 'n D.t -> t
    | Wrong_number_of_patterns : t
    | Inconsistent_patterns : t
    | Overlapping_patterns : t
    | No_remaining_patterns : t
    | Invalid_refutation : t
    | Duplicate_pattern_variable : string -> t

  (** The default severity of messages with a particular message code. *)
  let default_severity : t -> Asai.Diagnostic.severity = function
    | Parse_error -> Error
    | Encoding_error -> Error
    | Parsing_ambiguity _ -> Error
    | No_relative_precedence _ -> Error
    | Invalid_variable _ -> Error
    | Invalid_numeral _ -> Error
    | Invalid_constr _ -> Error
    | Invalid_field _ -> Error
    | Invalid_degeneracy _ -> Error
    | Not_enough_lambdas _ -> Error
    | Type_not_fully_instantiated _ -> Error
    | Unequal_synthesized_type _ -> Error
    | Checking_tuple_at_degenerated_record _ -> Error
    | Missing_field_in_tuple _ -> Error
    | Missing_method_in_comatch _ -> Error
    | Extra_field_in_tuple _ -> Error
    | Extra_method_in_comatch _ -> Error
    | Invalid_field_in_tuple -> Error
    | Duplicate_field_in_tuple _ -> Error
    | Duplicate_method_in_codata _ -> Error
    | Duplicate_field_in_record _ -> Error
    | Invalid_method_in_comatch -> Error
    | Duplicate_method_in_comatch _ -> Error
    | Missing_constructor_in_match _ -> Error
    | Unnamed_variable_in_match -> Error
    | Checking_lambda_at_nonfunction _ -> Error
    | Checking_tuple_at_nonrecord _ -> Error
    | Comatching_at_noncodata _ -> Error
    | Comatching_at_degenerated_codata _ -> Error
    | No_such_constructor _ -> Error
    | Missing_instantiation_constructor _ -> Error
    | Unequal_indices _ -> Error
    | Unbound_variable _ -> Error
    | Undefined_constant _ -> Bug
    | Undefined_metavariable _ -> Bug
    | No_such_field _ -> Error
    | Nonsynthesizing _ -> Error
    | Low_dimensional_argument_of_degeneracy _ -> Error
    | Missing_argument_of_degeneracy _ -> Error
    | Not_enough_arguments_to_function -> Error
    | Instantiating_zero_dimensional_type _ -> Error
    | Invalid_variable_face _ -> Error
    | Not_enough_arguments_to_instantiation -> Error
    | Applying_nonfunction_nontype _ -> Error
    | Wrong_number_of_arguments_to_constructor _ -> Error
    | Unimplemented _ -> Error
    | Matching_datatype_has_degeneracy _ -> Error
    | Wrong_number_of_arguments_to_pattern _ -> Error
    | Wrong_number_of_arguments_to_motive _ -> Error
    | No_such_constructor_in_match _ -> Error
    | Duplicate_constructor_in_match _ -> Error
    | Duplicate_constructor_in_data _ -> Error
    | Matching_on_nondatatype _ -> Error
    | Matching_wont_refine _ -> Hint
    | Dimension_mismatch _ -> Bug (* Sometimes Error? *)
    | Unsupported_numeral _ -> Error
    | Anomaly _ -> Bug
    | No_such_level _ -> Bug
    | Constant_already_defined _ -> Warning
    | Invalid_constant_name _ -> Error
    | Too_many_commands -> Error
    | Invalid_tightness _ -> Error
    | Fixity_mismatch -> Error
    | Zero_notation_symbols -> Error
    | Missing_notation_symbol -> Error
    | Ambidextrous_notation -> Error
    | Invalid_notation_symbol _ -> Error
    | Invalid_notation_head _ -> Error
    | Duplicate_notation_variable _ -> Error
    | Unused_notation_variable _ -> Error
    | Notation_variable_used_twice _ -> Error
    | Unbound_variable_in_notation _ -> Error
    | Head_already_has_notation _ -> Warning
    | Constant_assumed _ -> Info
    | Constant_defined _ -> Info
    | Notation_defined _ -> Info
    | Show _ -> Info
    | Comment_end_in_string -> Warning
    | Error_printing_error _ -> Bug
    | Checking_canonical_at_nonuniverse _ -> Error
    | Bare_case_tree_construct _ -> Hint
    | Wrong_boundary_of_record _ -> Error
    | Invalid_constructor_type _ -> Error
    | Missing_constructor_type _ -> Error
    | Locked_variable -> Error
    | Locked_axiom _ -> Error
    | Hole_generated _ -> Info
    | Open_holes -> Error
    | Quit -> Info
    | Synthesizing_recursion _ -> Error
    | Invalid_synthesized_type _ -> Error
    | Unrecognized_attribute -> Error
    | Invalid_degeneracy_action _ -> Bug
    | Wrong_number_of_patterns -> Error
    | Inconsistent_patterns -> Error
    | Overlapping_patterns -> Error
    | No_remaining_patterns -> Bug
    | Invalid_refutation -> Error
    | Duplicate_pattern_variable _ -> Error

  (** A short, concise, ideally Google-able string representation for each message code. *)
  let short_code : t -> string = function
    (* Usually bugs *)
    | Anomaly _ -> "E0000"
    | No_such_level _ -> "E0001"
    | Error_printing_error _ -> "E0002"
    | Invalid_degeneracy_action _ -> "E0003"
    (* Unimplemented future features *)
    | Unimplemented _ -> "E0100"
    | Unsupported_numeral _ -> "E0101"
    (* Parse errors *)
    | Parse_error -> "E0200"
    | Parsing_ambiguity _ -> "E0201"
    | Invalid_variable _ -> "E0202"
    | Invalid_field _ -> "E0203"
    | Invalid_constr _ -> "E0204"
    | Invalid_numeral _ -> "E0205"
    | Invalid_degeneracy _ -> "E0206"
    | No_relative_precedence _ -> "E0207"
    | Unrecognized_attribute -> "E0208"
    | Comment_end_in_string -> "E0250"
    | Encoding_error -> "E0299"
    (* Scope errors *)
    | Unbound_variable _ -> "E0300"
    | Undefined_constant _ -> "E0301"
    | Undefined_metavariable _ -> "E0302"
    | Locked_variable -> "E0310"
    | Locked_axiom _ -> "E0311"
    (* Bidirectional typechecking and case trees *)
    | Nonsynthesizing _ -> "E0400"
    | Unequal_synthesized_type _ -> "E0401"
    | Synthesizing_recursion _ -> "E0402"
    | Bare_case_tree_construct _ -> "H0403"
    | Invalid_synthesized_type _ -> "E0404"
    (* Dimensions *)
    | Dimension_mismatch _ -> "E0500"
    | Not_enough_lambdas _ -> "E0501"
    | Not_enough_arguments_to_function -> "E0502"
    | Not_enough_arguments_to_instantiation -> "E0503"
    | Type_not_fully_instantiated _ -> "E0504"
    | Instantiating_zero_dimensional_type _ -> "E0505"
    | Invalid_variable_face _ -> "E0506"
    (* Degeneracies *)
    | Missing_argument_of_degeneracy _ -> "E0600"
    | Low_dimensional_argument_of_degeneracy _ -> "E0601"
    (* Function-types *)
    | Checking_lambda_at_nonfunction _ -> "E0700"
    | Applying_nonfunction_nontype _ -> "E0701"
    (* Record fields *)
    | No_such_field _ -> "E0800"
    (* Tuples *)
    | Checking_tuple_at_nonrecord _ -> "E0900"
    | Checking_tuple_at_degenerated_record _ -> "E0901"
    | Missing_field_in_tuple _ -> "E0902"
    | Extra_field_in_tuple _ -> "E0903"
    | Duplicate_field_in_tuple _ -> "E0904"
    | Invalid_field_in_tuple -> "E0905"
    (* Datatype constructors *)
    | No_such_constructor _ -> "E1000"
    | Wrong_number_of_arguments_to_constructor _ -> "E1001"
    | Missing_instantiation_constructor _ -> "E1002"
    | Unequal_indices _ -> "E1003"
    (* Matches *)
    (* - Match variable *)
    | Unnamed_variable_in_match -> "E1100"
    | Matching_wont_refine _ -> "E1101"
    (* - Match type *)
    | Matching_on_nondatatype _ -> "E1200"
    | Matching_datatype_has_degeneracy _ -> "E1201"
    (* - Match branches *)
    | Missing_constructor_in_match _ -> "E1300"
    | No_such_constructor_in_match _ -> "E1301"
    | Duplicate_constructor_in_match _ -> "E1302"
    | Wrong_number_of_arguments_to_pattern _ -> "E1303"
    | Duplicate_pattern_variable _ -> "E1304"
    | Wrong_number_of_patterns -> "E1305"
    | Inconsistent_patterns -> "E1306"
    | Overlapping_patterns -> "E1307"
    | No_remaining_patterns -> "E1308"
    | Invalid_refutation -> "E1309"
    (* - Match motive *)
    | Wrong_number_of_arguments_to_motive _ -> "E1400"
    (* Comatches *)
    | Comatching_at_noncodata _ -> "E1400"
    | Comatching_at_degenerated_codata _ -> "E1401"
    | Missing_method_in_comatch _ -> "E1402"
    | Extra_method_in_comatch _ -> "E1403"
    | Duplicate_method_in_comatch _ -> "E1404"
    | Invalid_method_in_comatch -> "E1405"
    (* Canonical types *)
    | Checking_canonical_at_nonuniverse _ -> "E1500"
    | Duplicate_field_in_record _ -> "E1501"
    | Duplicate_method_in_codata _ -> "E1502"
    | Duplicate_constructor_in_data _ -> "E1503"
    | Wrong_boundary_of_record _ -> "E1504"
    | Invalid_constructor_type _ -> "E1505"
    | Missing_constructor_type _ -> "E1506"
    (* Commands *)
    | Too_many_commands -> "E2000"
    (* def *)
    | Constant_already_defined _ -> "E2100"
    | Invalid_constant_name _ -> "E2101"
    (* notation *)
    | Invalid_tightness _ -> "E2200"
    | Invalid_notation_symbol _ -> "E2201"
    | Zero_notation_symbols -> "E2202"
    | Missing_notation_symbol -> "E2203"
    | Ambidextrous_notation -> "E2204"
    | Fixity_mismatch -> "E2205"
    | Duplicate_notation_variable _ -> "E2206"
    | Invalid_notation_head _ -> "E2207"
    | Unused_notation_variable _ -> "E2208"
    | Notation_variable_used_twice _ -> "E2209"
    | Unbound_variable_in_notation _ -> "E2210"
    | Head_already_has_notation _ -> "E2211"
    (* Interactive proof *)
    | Open_holes -> "E3000"
    (* Command success *)
    | Constant_defined _ -> "I0000"
    | Constant_assumed _ -> "I0001"
    | Notation_defined _ -> "I0002"
    (* Events during command execution *)
    | Hole_generated _ -> "I0100"
    (* Control of execution *)
    | Quit -> "I0200"
    (* Debugging *)
    | Show _ -> "I9999"

  let rec default_text : t -> text = function
    | Parse_error -> text "parse error"
    | Encoding_error -> text "UTF-8 encoding error"
    | Parsing_ambiguity str -> textf "potential parsing ambiguity: %s" str
    | Invalid_variable str -> textf "invalid local variable name: %s" (String.concat "." str)
    | Invalid_field str -> textf "invalid field name: %s" str
    | Invalid_constr str -> textf "invalid constructor name: %s" str
    | Invalid_numeral str -> textf "invalid numeral: %s" str
    | Invalid_degeneracy str ->
        if str = "" then text "missing degeneracy" else textf "invalid degeneracy: %s" str
    | Invalid_variable_face (k, fa) ->
        let str = string_of_dim k in
        textf "invalid face: variable of dimension %s has no face '%s'"
          (if str = "" then "0" else str)
          (string_of_sface fa)
    | No_relative_precedence (n1, n2) ->
        textf
          "notations \"%s\" and \"%s\" have no relative precedence or associativity; they can only be combined with parentheses"
          n1 n2
    | Not_enough_lambdas n ->
        textf "not enough non-cube variables for higher-dimensional abstraction: need %d more" n
    | Not_enough_arguments_to_function ->
        text "not enough arguments for a higher-dimensional function application"
    | Not_enough_arguments_to_instantiation ->
        text "not enough arguments to instantiate a higher-dimensional type"
    | Type_not_fully_instantiated str -> textf "type not fully instantiated in %s" str
    | Instantiating_zero_dimensional_type ty ->
        textf "@[<hv 0>can't apply/instantiate a zero-dimensional type@;<1 2>%a@]" pp_printed
          (print ty)
    | Unequal_synthesized_type (sty, cty) ->
        textf "@[<hv 0>term synthesized type@;<1 2>%a@ but is being checked against type@;<1 2>%a@]"
          pp_printed (print sty) pp_printed (print cty)
    | Checking_tuple_at_degenerated_record r ->
        textf "can't check a tuple against a record %a with a nonidentity degeneracy applied"
          pp_printed (print r)
    | Comatching_at_degenerated_codata r ->
        textf "can't comatch against a codatatype %a with a nonidentity degeneracy applied"
          pp_printed (print r)
    | Missing_field_in_tuple f -> textf "record field '%s' missing in tuple" (Field.to_string f)
    | Missing_method_in_comatch f ->
        textf "codata method '%s' missing in comatch" (Field.to_string f)
    | Extra_field_in_tuple f -> (
        match f with
        | Some f -> textf "field '%s' in tuple doesn't occur in record type" (Field.to_string f)
        | None -> text "too many un-labeled fields in tuple")
    | Extra_method_in_comatch f ->
        textf "method '%s' in comatch doesn't occur in codata type" (Field.to_string f)
    | Invalid_field_in_tuple -> text "invalid field in tuple"
    | Invalid_method_in_comatch -> text "invalid method in comatch"
    | Duplicate_field_in_tuple f ->
        textf "record field '%s' appears more than once in tuple" (Field.to_string f)
    | Duplicate_method_in_comatch f ->
        textf "method '%s' appears more than once in comatch" (Field.to_string f)
    | Missing_constructor_in_match c ->
        textf "missing match clause for constructor %s" (Constr.to_string c)
    | Unnamed_variable_in_match -> text "unnamed match variable"
    | Checking_lambda_at_nonfunction ty ->
        textf "@[<hv 0>checking abstraction against non-function type@;<1 2>%a@]" pp_printed
          (print ty)
    | Checking_tuple_at_nonrecord ty ->
        textf "@[<hv 0>checking tuple against non-record type@;<1 2>%a@]" pp_printed (print ty)
    | Comatching_at_noncodata ty ->
        textf "@[<hv 0>checking comatch against non-codata type@;<1 2>%a@]" pp_printed (print ty)
    | No_such_constructor (d, c) -> (
        match d with
        | `Data d ->
            textf "datatype %a has no constructor named %s" pp_printed (print d)
              (Constr.to_string c)
        | `Nondata d ->
            textf "non-datatype %a has no constructor named %s" pp_printed (print d)
              (Constr.to_string c)
        | `Other ty ->
            textf "@[<hv 0>non-datatype@;<1 2>%a@ has no constructor named %s@]" pp_printed
              (print ty) (Constr.to_string c))
    | Wrong_number_of_arguments_to_constructor (c, n) ->
        if n > 0 then textf "too many arguments to constructor %s (%d extra)" (Constr.to_string c) n
        else
          textf "not enough arguments to constructor %s (need %d more)" (Constr.to_string c) (abs n)
    | No_such_field (d, f) -> (
        match d with
        | `Record d ->
            textf "record type %a has no field named %s" pp_printed (print d)
              (Field.to_string_ori f)
        | `Nonrecord d ->
            textf "non-record type %a has no field named %s" pp_printed (print d)
              (Field.to_string_ori f)
        | `Other -> textf "term has no field named %s" (Field.to_string_ori f)
        | `Degenerated_record ->
            textf
              "record type with a nonidentity degeneracy applied is no longer a record, hence has no field named %s"
              (Field.to_string_ori f))
    | Missing_instantiation_constructor (exp, got) ->
        let pp_got =
          match got with
          | `Nonconstr tm -> print tm
          | `Constr c -> Printed (pp_print_string, Constr.to_string c) in
        fun ppf ->
          fprintf ppf
            "@[<hv 0>instantiation arguments of datatype must be matching constructors:@ expected@;<1 2>%s@ but got@;<1 2>"
            (Constr.to_string exp);
          pp_printed ppf pp_got;
          pp_close_box ppf ()
    | Unequal_indices (t1, t2) ->
        textf
          "@[<hv 0>index@;<1 2>%a@ of constructor application doesn't match the corresponding index@;<1 2>%a@ of datatype instance@]"
          pp_printed (print t1) pp_printed (print t2)
    | Unbound_variable c -> textf "unbound variable: %s" c
    | Undefined_constant c -> textf "undefined constant: %a" pp_printed (print c)
    | Undefined_metavariable v -> textf "undefined metavariable: %a" pp_printed (print v)
    | Nonsynthesizing pos -> textf "non-synthesizing term in synthesizing position (%s)" pos
    | Low_dimensional_argument_of_degeneracy (deg, dim) ->
        textf "argument of degeneracy '%s' must have dimension at least %s" deg (string_of_dim dim)
    | Missing_argument_of_degeneracy deg -> textf "missing argument for degeneracy %s" deg
    | Applying_nonfunction_nontype (tm, ty) ->
        textf
          "@[<hv 0>attempt to apply/instantiate@;<1 2>%a@ of type@;<1 2>%a@ which is not a function-type or universe@]"
          pp_printed (print tm) pp_printed (print ty)
    | Unimplemented str -> textf "%s not yet implemented" str
    | Matching_datatype_has_degeneracy ty ->
        textf "@[<hv 0>can't match on element of datatype@;<1 2>%a@ that has a degeneracy applied@]"
          pp_printed (print ty)
    | Wrong_number_of_arguments_to_pattern (c, n) ->
        if n > 0 then
          textf "too many arguments to constructor %s in match pattern (%d extra)"
            (Constr.to_string c) n
        else
          textf "not enough arguments to constructor %s in match pattern (need %d more)"
            (Constr.to_string c) (abs n)
    | Wrong_number_of_arguments_to_motive n ->
        textf "wrong number of arguments for match motive: should be %d" n
    | No_such_constructor_in_match (d, c) ->
        textf "datatype %a being matched against has no constructor %s" pp_printed (print d)
          (Constr.to_string c)
    | Duplicate_constructor_in_match c ->
        textf "constructor %s appears twice in match" (Constr.to_string c)
    | Matching_on_nondatatype ty ->
        textf "@[<hv 0>can't match on variable belonging to non-datatype@;<1 2>%a@]" pp_printed
          (print ty)
    | Matching_wont_refine (msg, d) ->
        textf "match will not refine the goal or context (%s): %a" msg pp_printed (print d)
    | Dimension_mismatch (op, a, b) ->
        let sa, sb = (string_of_dim a, string_of_dim b) in
        textf "dimension mismatch in %s (%s ≠ %s)" op
          (if sa = "" then "0" else sa)
          (if sb = "" then "0" else sb)
    | Unsupported_numeral n -> textf "unsupported numeral: %a" Q.pp_print n
    | Anomaly str -> textf "anomaly: %s" str
    | No_such_level i -> textf "@[<hov 2>no level variable@ %a@ in context@]" pp_printed (print i)
    | Constant_already_defined name -> textf "redefining constant: %a" pp_utf_8 name
    | Invalid_constant_name name -> textf "invalid constant name: %a" pp_utf_8 name
    | Too_many_commands -> text "too many commands: enter one at a time"
    | Fixity_mismatch ->
        text
          "notation command doesn't match pattern (tightness must be omitted only for outfix notations)"
    | Zero_notation_symbols -> text "notation has no symbols"
    | Missing_notation_symbol -> text "missing notation symbol between variables"
    | Ambidextrous_notation -> text "notation can't be both right and left associative"
    | Invalid_tightness str -> textf "invalid tightness: %s" str
    | Invalid_notation_symbol str -> textf "invalid notation symbol: %s" str
    | Invalid_notation_head str -> textf "invalid notation head: %s" str
    | Duplicate_notation_variable x -> textf "duplicate notation variable: '%s'" x
    | Unused_notation_variable x -> textf "unused notation variable: '%s'" x
    | Notation_variable_used_twice x -> textf "notation variable '%s' used twice" x
    | Unbound_variable_in_notation xs ->
        textf "unbound variable(s) in notation definition: %s" (String.concat ", " xs)
    | Head_already_has_notation name ->
        textf "replacing printing notation for %s (previous notation will still be parseable)" name
    | Constant_assumed (name, h) ->
        if h > 1 then textf "Axiom %a assumed, containing %d holes" pp_printed (print name) h
        else if h = 1 then textf "Axiom %a assumed, containing 1 hole" pp_printed (print name)
        else textf "Axiom %a assumed" pp_printed (print name)
    | Constant_defined (names, h) -> (
        match names with
        | [] -> textf "Anomaly: no constant defined"
        | [ (name, discrete) ] ->
            let discrete = if discrete then " (discrete)" else "" in
            if h > 1 then
              textf "Constant %a defined%s, containing %d holes" pp_printed (print name) discrete h
            else if h = 1 then
              textf "Constant %a defined%s, containing 1 hole" pp_printed (print name) discrete
            else textf "Constant %a defined%s" pp_printed (print name) discrete
        | _ ->
            (if h > 1 then textf "@[<v 2>Constants defined mutually, containing %d holes:@,%a@]" h
             else if h = 1 then textf "@[<v 2>Constants defined mutually, containing 1 hole:@,%a@]"
             else textf "@[<v 2>Constants defined mutually:@,%a@]")
              (fun ppf names ->
                pp_print_list
                  (fun ppf (name, discrete) ->
                    pp_printed ppf (print name);
                    if discrete then pp_print_string ppf " (discrete)")
                  ppf names)
              names)
    | Notation_defined name -> textf "Notation %s defined" name
    | Show (str, x) -> textf "%s: %a" str pp_printed (print x)
    | Comment_end_in_string ->
        text "comment-end sequence `} in quoted string: cannot be commented out"
    | Error_printing_error e ->
        textf "error while printing message:@ %a" (fun ppf () -> default_text e ppf) ()
    | Checking_canonical_at_nonuniverse (tm, ty) ->
        textf "checking %s at non-universe %a" tm pp_printed (print ty)
    | Bare_case_tree_construct str ->
        textf "%s encountered outside case tree, wrapping in implicit let-binding" str
    | Duplicate_method_in_codata fld ->
        textf "duplicate method in codatatype: %s" (Field.to_string fld)
    | Duplicate_field_in_record fld ->
        textf "duplicate field in record type: %s" (Field.to_string fld)
    | Duplicate_constructor_in_data c ->
        textf "duplicate constructor in datatype: %s" (Constr.to_string c)
    | Wrong_boundary_of_record n ->
        if n > 0 then
          textf "too many variables in boundary of higher-dimensional record (%d extra)" n
        else
          textf "not enough variables in boundary of higher-dimensional record (need %d more)"
            (abs n)
    | Invalid_constructor_type c ->
        textf "invalid type for constructor %s: must be current datatype instance"
          (Constr.to_string c)
    | Missing_constructor_type c ->
        textf "missing type for constructor %s of indexed datatype" (Constr.to_string c)
    | Locked_variable -> text "variable locked behind external degeneracy"
    | Locked_axiom a -> textf "axiom %a locked behind external degeneracy" pp_printed (print a)
    | Hole_generated (n, ty) ->
        textf "@[<v 0>hole %s generated:@,@,%a@]" (Meta.name n) pp_printed (print ty)
    | Open_holes -> text "There are open holes"
    | Quit -> text "Goodbye!"
    | Synthesizing_recursion c ->
        textf "for '%a' to be recursive, it must have a declared type" pp_printed (print c)
    | Invalid_synthesized_type (str, ty) ->
        textf "type %a synthesized by %s is invalid for entire term" pp_printed (print ty) str
    | Unrecognized_attribute -> textf "unrecognized attribute"
    | Invalid_degeneracy_action (str, nk, n) ->
        textf "invalid degeneracy action on %s: dimension '%s' doesn't factor through codomain '%s'"
          str (string_of_dim nk) (string_of_dim n)
    | Wrong_number_of_patterns -> text "wrong number of patterns for match"
    | Inconsistent_patterns -> text "inconsistent patterns in match"
    | Overlapping_patterns -> text "overlapping patterns in match"
    | No_remaining_patterns -> text "no remaining patterns while parsing match"
    | Invalid_refutation -> text "invalid refutation: no discriminee has an empty type"
    | Duplicate_pattern_variable x ->
        textf "variable name '%s' used more than once in match patterns" x
end

include Asai.StructuredReporter.Make (Code)
open Code

let struct_at_degenerated_type eta name =
  match eta with
  | `Eta -> Checking_tuple_at_degenerated_record name
  | `Noeta -> Comatching_at_degenerated_codata name

let missing_field_in_struct eta fld =
  match eta with
  | `Eta -> Missing_field_in_tuple fld
  | `Noeta -> Missing_method_in_comatch fld

let struct_at_nonrecord eta p =
  match eta with
  | `Eta -> Checking_tuple_at_nonrecord p
  | `Noeta -> Comatching_at_noncodata p

let duplicate_field eta fld =
  match eta with
  | `Eta -> Duplicate_field_in_tuple fld
  | `Noeta -> Duplicate_method_in_comatch fld

let ( <|> ) : type a b. a option -> Code.t -> a =
 fun x e ->
  match x with
  | Some x -> x
  | None -> fatal e
