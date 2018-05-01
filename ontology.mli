(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010                                     *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  This library is free software; you can redistribute it *)
(*  and/or modify it under the terms of the GNU Lesser     *)
(*  General Public License as published by the Free        *)
(*  Software Foundation; either version 2.1 of the         *)
(*  License, or (at your option) any later version.        *)
(*                                                         *)
(*  This library is distributed in the hope that it will   *)
(*  be useful, but WITHOUT ANY WARRANTY; without even the  *)
(*  implied warranty of MERCHANTABILITY or FITNESS FOR A   *)
(*  PARTICULAR PURPOSE. See the GNU Lesser General Public  *)
(*  License for more details.                              *)
(*                                                         *)
(*  You should have received a copy of the GNU Lesser      *)
(*  General Public License along with this library; if     *)
(*  not write to the Free Software Foundation, Inc., 51    *)
(*  Franklin Street, Fifth Floor, Boston, MA  02110-1301   *)
(*  USA                                                    *)
(***********************************************************)

open Types
open Owl2

module ClassMap : Map.S with type key = Class.t
module ClassSet : Set.S with type elt = Class.t

type t
val create : unit -> t

(**======================= consing =========================**)

val cons_IRI : t -> IRI.elt -> IRI.t
val cons_NodeID : t -> NodeID.elt -> NodeID.t
val cons_ObjectProperty : t -> ObjectProperty.elt -> ObjectProperty.t
val cons_Literal : t -> Literal.elt -> Literal.t
val cons_ObjectPropertyExpression : t -> ObjectPropertyExpression.elt -> ObjectPropertyExpression.t
val cons_DataPropertyExpression : t -> DataPropertyExpression.elt -> DataPropertyExpression.t
val cons_DataRange : t -> DataRange.elt -> DataRange.t
val cons_ClassExpression : t -> ClassExpression.elt -> ClassExpression.t
val cons_ClassExpressionAxiom : t -> ClassExpressionAxiom.elt -> ClassExpressionAxiom.t
val cons_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.elt -> ObjectPropertyAxiom.t
val cons_DataPropertyAxiom : t -> DataPropertyAxiom.elt -> DataPropertyAxiom.t
val cons_DatatypeDefinition : t -> DatatypeDefinition.elt -> DatatypeDefinition.t
val cons_Key : t -> Key.elt -> Key.t
val cons_Assertion : t -> Assertion.elt -> Assertion.t
val cons_AnnotationSubject : t -> AnnotationSubject.elt -> AnnotationSubject.t
val cons_AnnotationValue : t -> AnnotationValue.elt -> AnnotationValue.t
val cons_Annotation : t -> Annotation.elt -> Annotation.t
val cons_AnnotationAxiom : t -> AnnotationAxiom.elt -> AnnotationAxiom.t

(**====================== iterators ========================**)

val iter_record_ObjectProperty : (ObjectProperty.t -> int -> unit) -> t -> unit
val iter_record_Class : (Class.t -> int -> unit) -> t -> unit
val iter_record_Individual : (Individual.t -> int -> unit) -> t -> unit

val iter_record_ComplexObjectPropertyExpression : (ObjectPropertyExpression.t -> Polarity.Counter.t -> unit) -> t -> unit
val iter_record_ComplexClassExpression : (ClassExpression.t -> Polarity.Counter.t -> unit) -> t -> unit

val iter_record_ObjectPropertyAxiom : (ObjectPropertyAxiom.t -> unit) -> t -> unit
val iter_record_ClassExpressionAxiom : (ClassExpressionAxiom.t -> unit) -> t -> unit
val iter_record_Assertion : (Assertion.t -> unit) -> t -> unit

(**============== statistical information ================**)

val has_positive_Nothing : t -> bool
val has_positive_ComplementOf : t -> bool
val has_negative_Thing : t -> bool

val total_ObjectPropertyIRI : t -> int
val total_ClassIRI : t -> int
val total_IndividualIRI : t -> int

val count_TopObjectProperty : t -> Polarity.Counter.t
val count_BottomObjectProperty : t -> Polarity.Counter.t
val count_Thing : t -> Polarity.Counter.t
val count_Nothing : t -> Polarity.Counter.t

val count_InverseObjectProperty : t -> Polarity.Counter.t
val total_InverseObjectProperty : t -> int

val count_ObjectIntersectionOf : t -> Polarity.Counter.t
val count_ObjectUnionOf : t -> Polarity.Counter.t
val count_ObjectComplementOf : t -> Polarity.Counter.t
val count_ObjectOneOf : t -> Polarity.Counter.t
val count_ObjectSomeValuesFrom : t -> Polarity.Counter.t
val count_ObjectAllValuesFrom : t -> Polarity.Counter.t
val count_ObjectHasValue : t -> Polarity.Counter.t
val count_ObjectHasSelf : t -> Polarity.Counter.t
val count_ObjectMinCardinality : t -> Polarity.Counter.t
val count_ObjectMaxCardinality : t -> Polarity.Counter.t
val count_ObjectExactCardinality : t -> Polarity.Counter.t
val count_DataSomeValuesFrom : t -> Polarity.Counter.t
val count_DataAllValuesFrom : t -> Polarity.Counter.t
val count_DataHasValue : t -> Polarity.Counter.t
val count_DataMinCardinality : t -> Polarity.Counter.t
val count_DataMaxCardinality : t -> Polarity.Counter.t
val count_DataExactCardinality : t -> Polarity.Counter.t

val total_ObjectIntersectionOf : t -> int
val total_ObjectUnionOf : t -> int
val total_ObjectComplementOf : t -> int
val total_ObjectOneOf : t -> int
val total_ObjectSomeValuesFrom : t -> int
val total_ObjectAllValuesFrom : t -> int
val total_ObjectHasValue : t -> int
val total_ObjectHasSelf : t -> int
val total_ObjectMinCardinality : t -> int
val total_ObjectMaxCardinality : t -> int
val total_ObjectExactCardinality : t -> int
val total_DataSomeValuesFrom : t -> int
val total_DataAllValuesFrom : t -> int
val total_DataHasValue : t -> int
val total_DataMinCardinality : t -> int
val total_DataMaxCardinality : t -> int
val total_DataExactCardinality : t -> int
                  
val total_SubPropertyOf : t -> int                              
val total_EquivalentProperties : t -> int
val total_InverseProperties : t -> int
val total_FunctionalProperty : t -> int
val total_TransitiveProperty : t -> int
val total_RoleComposition : t -> int
val total_SubClassOf : t -> int
val total_EquivalentClasses : t -> int
val total_ClassAssertion : t -> int
val total_PropertyAssertion : t -> int

(**================= insertion of axioms ====================**)

val add_ObjectPropertyAxiom : t -> ObjectPropertyAxiom.t -> unit
val add_ClassExpressionAxiom : t -> ClassExpressionAxiom.t -> unit
val add_Assertion : t -> Assertion.t -> unit

(**=============== printing of statistics ===================**)
val print_statistics : t -> out_channel -> unit

val get_original_concept_names : t -> StringSet.t
val get_concept_names : t -> StringSet.t

val get_role_names : t -> StringSet.t

type occurrence_hash = (string, StringSet.t) Hashtbl.t

(* Computes the mapping "definitorial depth -> concept names", i.e. the concepted names at a given  *)
(* definitorial depth, for a given ontology and occurrence hash, i.e. a hash containing for every   *)
(* definition (equivalent A C) occurring in the ontology the mapping                                *)
(* "A -> [concept names occurring in C]"                                                            *)
val compute_definitorial_depth_hash : t -> occurrence_hash -> (int, StringSet.t) Hashtbl.t

val create_occurrence_hash : unit -> occurrence_hash

val insert_in_occurrence_hash : occurrence_hash -> string -> string list -> unit

val get_domain_concept_names : t -> StringSet.t
val get_range_concept_names : t -> StringSet.t


val pre_concepts : (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val pre_sigma_concepts : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t

val pre_sigma_concepts_with_domain_range : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t

val pre_domain : t -> (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val pre_sigma_domain : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t

val pre_domain_concept_names : t -> (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val pre_sigma_domain_concept_names : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t

val pre_range : t -> (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val pre_sigma_range : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t

val pre_range_concept_names : t -> (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val pre_sigma_range_concept_names : t -> (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t


val post_concepts : (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val post_proper_concepts : (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val post_proper_concepts_with_domain_bindings : (string, StringSet.t) Hashtbl.t -> string -> StringSet.t

val post_sigma_concepts : (string, StringSet.t) Hashtbl.t -> Sigma.t -> string -> StringSet.t


val top_class : unit -> Class.t
val top_class_expression : t -> ClassExpression.t

val concept_name_class : t -> string -> Class.t
val concept_name_class_expression : t -> string -> ClassExpression.t

val role_name_object_property : t -> string -> ObjectProperty.t
val role_name_object_property_expression : t -> string -> ObjectPropertyExpression.t

val exists_rC_class_expression : t -> ObjectPropertyExpression.t -> ClassExpression.t -> ClassExpression.t

val conjunction_class_expression : t -> ClassExpression.t list -> ClassExpression.t

(* Constructs an axiom of the form e1 = e2 *)
val definition_axiom : t -> ClassExpression.t -> ClassExpression.t -> ClassExpressionAxiom.t

val subsumption_axiom : t -> ClassExpression.t -> ClassExpression.t -> ClassExpressionAxiom.t

val domain_class_expression : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t
val range_class_expression : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t

val domain_subsumption_axiom : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t -> ClassExpressionAxiom.Constructor.t
val range_subsumption_axiom : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t -> ClassExpressionAxiom.Constructor.t

val construct_ObjectIntersectionOf : ClassExpression.t * ClassExpression.t -> ClassExpression.Constructor.t

(* val concept_to_OWL : t -> concept -> ClassExpression.t *)

val add_domain_range_concept_definitions : StringSet.t -> t -> unit

val is_domain_range_axiom : t -> ClassExpressionAxiom.t -> bool

val concept_ELHr_to_OWL : t -> concept -> ClassExpression.t
val concept_ELran_to_OWL : t -> concept -> ClassExpression.t
val axioms_ELHr_to_Ontology : axiom list -> t

(* kate: replace-tabs on; indent-width 2; *)
