(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010-2013                                *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
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

open CommonTypes
open Consed.T
open Types

(**================= Common Interfaces ==================**)

module CommonExtended : sig
	module type S = sig
		include OrderedHashedType
		module Set : Set.S with type elt = t
		module Map : Map.S with type key = t
		module HSet : Hashsetlp.S with type key = t and type elt = t
		module HMap : Hashtbllp.S with type key = t
	end
end

module CommonConsed : sig
	module type S = sig
		type elt
		include OrderedHashedType with type t = elt consed
		module Cons : Consed.S with type key = elt
		val cons : Cons.t -> elt -> t
		module Set : Cset.S with type elt = t
		module Map : Cmap.S with type key = t and module Set = Set
		module HSet : Chashsetlp.S with type key = t
		module HMap : Chashtbllp.S with type key = t
	end
end

(**======================== IRIs ========================**)

module IRI : sig
	module Constructor : sig
		type t =
			| IRI of string
	end
	include CommonConsed.S with type elt = Constructor.t
	val str_of : t -> string
end

(**====================== NodeIDs =======================**)

module NodeID : sig
	module Constructor : sig
		type t =
			| NodeID of string
	end
	include CommonConsed.S with type elt = Constructor.t
	val str_of : t -> string
end

(**====================== Datatypes =====================**)

module Datatype : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| Rdfs_Literal
			| Owl_real
			| Owl_rational
			| Xsd_decimal
			| Xsd_integer
			| Xsd_nonNegativeInteger
			| Xsd_nonPositiveInteger
			| Xsd_positiveInteger
			| Xsd_negativeInteger
			| Xsd_long
			| Xsd_int
			| Xsd_short
			| Xsd_byte
			| Xsd_unsignedLong
			| Xsd_unsignedInt
			| Xsd_unsignedShort
			| Xsd_unsignedByte
			| Xsd_double
			| Xsd_float
			| Xsd_string
			| Xsd_normalizedString
			| Xsd_token
			| Xsd_language
			| Xsd_Name
			| Xsd_NCName
			| Xsd_NMTOKEN
			| Xsd_boolean
			| Xsd_hexBinary
			| Xsd_base64Binary
			| Xsd_anyURI
			| Xsd_dateTime
			| Xsd_dateTimeStamp
			| Rdf_XMLLiteral
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**================== Constraining Facets ===============**)

module ConstrainingFacet : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| Xsd_minInclusive
			| Xsd_maxInclusive
			| Xsd_minExclusive
			| Xsd_maxExclusive
			| Xsd_length
			| Xsd_minLength
			| Xsd_maxLength
			| Xsd_pattern
			| Rdf_langRange
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**================= Object Properties ==================**)

module ObjectProperty : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| TopObjectProperty
			| BottomObjectProperty
	end
	include CommonConsed.S with type elt = Constructor.t
	val str_of : t -> string
end

(**=================== Data Properties ==================**)

module DataProperty : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| TopDataProperty
			| BottomDataProperty
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**================ Annotation Properties ===============**)

module AnnotationProperty : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| Rdfs_label
			| Rdfs_comment
			| Rdfs_seeAlso
			| Rdfs_isDefinedBy
			| Owl_deprecated
			| Owl_versionInfo
			| Owl_priorVersion
			| Owl_backwardCompatibleWith
			| Owl_incompatibleWith
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**====================== Classes =======================**)

module Class : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| Thing
			| Nothing
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**==================== Individuals =====================**)

module Individual : sig
	module Constructor : sig
		type t =
			| NamedIndividual of IRI.t
			| AnonymousIndividual of NodeID.t
	end
	include CommonExtended.S with type t = Constructor.t
	val str_of : t -> string
end

(**======================= Literals =====================**)

module Literal : sig
	module Constructor : sig
		type t =
			| TypedLiteral of string * Datatype.t
			| StringLiteralNoLanguage of string
			| StringLiteralWithLanguage of string * string
	end
	include CommonConsed.S with type elt = Constructor.t
end

(**============ Object Property Expressions =============**)

module ObjectPropertyExpression : sig
	module Constructor : sig
		type t =
			| ObjectProperty of ObjectProperty.t
			| InverseObjectProperty of ObjectProperty.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**============= Data Property Expressions ==============**)

module DataPropertyExpression : sig
	module Constructor : sig
		type t =
			| DataProperty of DataProperty.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**==================== Data Ranges =====================**)

module DataRange : sig
	module Constructor : sig
		type t =
			| Datatype of Datatype.t
			| DataIntersectionOf of t consed list
			| DataUnionOf of t consed list
			| DataComplementOf of t consed
			| DataOneOf of Literal.t list
			| DatatypeRestriction of Datatype.t * ((ConstrainingFacet.t * Literal.t) list)
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**==================== Class Expressions ==================**)

module ClassExpression : sig
	module Constructor : sig
		type t =
			| Class of Class.t
			| ObjectIntersectionOf of t consed * t consed
			(*|      | ObjectIntersectionOfb of t consed * t consed*)
			| ObjectUnionOf of t consed * t consed
			(*|      | ObjectUnionOfb of t consed * t consed*)
			| ObjectComplementOf of t consed
			| ObjectOneOf of Individual.t list
			| ObjectSomeValuesFrom of ObjectPropertyExpression.t * (t consed)
			| ObjectAllValuesFrom of ObjectPropertyExpression.t * (t consed)
			| ObjectHasValue of ObjectPropertyExpression.t * Individual.t
			| ObjectHasSelf of ObjectPropertyExpression.t
			| ObjectMinCardinality of int * ObjectPropertyExpression.t * (t consed option)
			| ObjectMaxCardinality of int * ObjectPropertyExpression.t * (t consed option)
			| ObjectExactCardinality of int * ObjectPropertyExpression.t * (t consed option)
			| DataSomeValuesFrom of DataPropertyExpression.t list * DataRange.t
			| DataAllValuesFrom of DataPropertyExpression.t list * DataRange.t
			| DataHasValue of DataPropertyExpression.t * Literal.t
			| DataMinCardinality of int * DataPropertyExpression.t * (DataRange.t option)
			| DataMaxCardinality of int * DataPropertyExpression.t * (DataRange.t option)
			| DataExactCardinality of int * DataPropertyExpression.t * (DataRange.t option)
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================= Class ExpressionAxioms ================**)

module ClassExpressionAxiom : sig
	module Constructor : sig
		type t =
			| SubClassOf of ClassExpression.t * ClassExpression.t
			| EquivalentClasses of ClassExpression.t list
			| DisjointClasses of ClassExpression.t list
			| DisjointUnion of Class.t * ClassExpression.t list
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================= Object Property Axioms ================**)

module ObjectPropertyAxiom : sig
	module Constructor : sig
		type t =
			| SubObjectPropertyOf of ObjectPropertyExpression.t list * ObjectPropertyExpression.t
			| EquivalentObjectProperties of ObjectPropertyExpression.t list
			| DisjointObjectProperties of ObjectPropertyExpression.t list
			| InverseObjectProperties of ObjectPropertyExpression.t * ObjectPropertyExpression.t
			| ObjectPropertyDomain of ObjectPropertyExpression.t * ClassExpression.t
			| ObjectPropertyRange of ObjectPropertyExpression.t * ClassExpression.t
			| FunctionalObjectProperty of ObjectPropertyExpression.t
			| InverseFunctionalObjectProperty of ObjectPropertyExpression.t
			| ReflexiveObjectProperty of ObjectPropertyExpression.t
			| IrreflexiveObjectProperty of ObjectPropertyExpression.t
			| SymmetricObjectProperty of ObjectPropertyExpression.t
			| AsymmetricObjectProperty of ObjectPropertyExpression.t
			| TransitiveObjectProperty of ObjectPropertyExpression.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================= Data Property Axioms ================**)

module DataPropertyAxiom : sig
	module Constructor : sig
		type t =
			| SubDataPropertyOf of DataPropertyExpression.t * DataPropertyExpression.t
			| EquivalentDataProperties of DataPropertyExpression.t list
			| DisjointDataProperties of DataPropertyExpression.t list
			| DataPropertyDomain of DataPropertyExpression.t * ClassExpression.t
			| DataPropertyRange of DataPropertyExpression.t * DataRange.t
			| FunctionalDataProperty of DataPropertyExpression.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================= Datatype Definitions =================**)

module DatatypeDefinition : sig
	module Constructor : sig
		type t =
			| DatatypeDefinition of Datatype.t * DataRange.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**========================= Keys =========================**)

module Key : sig
	module Constructor : sig
		type t =
			| HasKey of ClassExpression.t * ObjectPropertyExpression.t list * DataPropertyExpression.t list
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**====================== Assertions ======================**)

module Assertion : sig
	module Constructor : sig
		type t =
			| SameIndividual of Individual.t list
			| DifferentIndividuals of Individual.t list
			| ClassAssertion of ClassExpression.t * Individual.t
			| ObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
			| NegativeObjectPropertyAssertion of ObjectPropertyExpression.t * Individual.t * Individual.t
			| DataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
			| NegativeDataPropertyAssertion of DataPropertyExpression.t * Individual.t * Literal.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================= Annotation Subjects ==================**)

module AnnotationSubject : sig
	module Constructor : sig
		type t =
			| IRI of IRI.t
			| AnonymousIndividual of NodeID.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================== Annotation Values ===================**)

module AnnotationValue : sig
	module Constructor : sig
		type t =
			| AnonymousIndividual of NodeID.t
			| IRI of IRI.t
			| Literal of Literal.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**===================== Annotations ======================**)

module Annotation : sig
	module Constructor : sig
		type t =
			| Annotation of t consed list * AnnotationProperty.t * AnnotationValue.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(**================== Annotation Axioms ===================**)

module AnnotationAxiom : sig
	module Constructor : sig
		type t =
			| AnnotationAssertion of Annotation.t list * AnnotationProperty.t * AnnotationSubject.t * AnnotationValue.t
			| SubAnnotationPropertyOf of Annotation.t list * AnnotationProperty.t * AnnotationProperty.t
			| AnnotationPropertyDomain of Annotation.t list * AnnotationProperty.t * IRI.t
			| AnnotationPropertyRange of Annotation.t list * AnnotationProperty.t * IRI.t
	end
	include (CommonConsed.S with type elt = Constructor.t)
end

(** **)

val concept_names_in_class : Class.Constructor.t -> string
val concept_names_in_class_expression : ClassExpression.t -> string list

(* CAUTION: the class expression has to be a concept name *)
val concept_name_in_concept_name_class_expression : ClassExpression.t -> string

val is_concept_name_class : Class.t -> bool
val is_concept_name_class_option : Class.t -> string option
val is_concept_name_class_expression : ClassExpression.t -> bool
val is_concept_name_class_expression_option : ClassExpression.t -> string option

(* Returns true iff 'exp' is a concept name or Top *)
val is_atomic : ClassExpression.t -> bool

(* Returns 'r' iff 'exp' is of the form some r. Top *)
val is_domain_exists_expression : ClassExpression.t -> string option

(* Returns 'r' iff 'exp' is of the form some r. A *)
val is_domain_exists_concept_name_expression : ClassExpression.t -> (string * string) option

(* Returns 'r' iff 'exp' is of the form ran r *)
val is_range_expression : ClassExpression.t -> string option

(* Returns 'r' iff 'exp' is of the form B_1 \sqcap \dotsc \sqcap B_n *)
val is_conjunction_of_concept_names_expression : ClassExpression.t -> (string list) option

(* kate: replace-tabs on; indent-width 2; *)
