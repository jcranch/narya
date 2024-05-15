open Dim

(* ******************** Groups of terms ******************** *)

(* At both the checked and the value level we have actually two different types to define: ordinary terms and case trees.  However, there is some overlap in the types of constructors and operations that these support: they can both contain lambda-abstractions and structs.  Thus, to avoid duplication of code, we actually define both together as one GADT type family, indexed by a two-element type to distinguish them.  We name the two groups after the two kinds of energy:

   - Ordinary terms are "kinetic", because ordinary computation applies directly to them.
   - Case trees are "potential", because they don't compute until enough arguments are applied to reach a leaf of the case tree.  That leaf can be either a kinetic term or information about a canonical type (which is not a computation, just a specification of behavior).
*)

type kinetic = Dummy_kinetic
type potential = Dummy_potential
type _ energy = Kinetic : kinetic energy | Potential : potential energy

(* Structs can have or lack eta-conversion, but the only kinetic ones are the ones with eta (records). *)
type yes_eta = Dummy_yes_eta
type no_eta = Dummy_no_eta
type (_, _) eta = Eta : ('s, yes_eta) eta | Noeta : (potential, no_eta) eta

(* Only structs without eta (codatatypes) can have higher fields. *)
type (_, _) higher = Higher : ('unused, no_eta) higher | Lower : (D.zero, yes_eta) higher

