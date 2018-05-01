(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
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

open Owl2
val load_Ontology_from_channel : Ontology.t -> in_channel -> unit
val fprint_IRI :
  Format.formatter -> IRI.t -> unit
val fprint_Datatype :
  Format.formatter -> Datatype.t -> unit
val fprint_ConstrainingFacet :
  Format.formatter ->
  ConstrainingFacet.t -> unit
val fprint_ObjectProperty :
  Format.formatter ->
  ObjectProperty.t -> unit
val fprint_DataProperty :
  Format.formatter -> DataProperty.t -> unit
val fprint_AnnotationProperty :
  Format.formatter ->
  AnnotationProperty.t -> unit
val fprint_Class :
  Format.formatter -> Class.t -> unit
val fprint_Individual :
  Format.formatter -> Individual.t -> unit
val fprint_Literal :
  Format.formatter -> Literal.t -> unit
val fprint_ObjectPropertyExpression :
  Format.formatter ->
  ObjectPropertyExpression.t -> unit
val fprint_subObjectPropertyExpression :
  Format.formatter ->
  ObjectPropertyExpression.t list ->
  unit
val fprint_DataPropertyExpression :
  Format.formatter ->
  DataPropertyExpression.t -> unit
val fprint_DataRange :
  Format.formatter -> DataRange.t -> unit
val fprint_ClassExpression :
  Format.formatter ->
  ClassExpression.t -> unit
val fprint_ClassExpressionAxiom :
  Format.formatter ->
  ClassExpressionAxiom.t -> unit
val fprint_ObjectPropertyAxiom :
  Format.formatter ->
  ObjectPropertyAxiom.t -> unit
val fprint_DataPropertyAxiom :
  Format.formatter ->
  DataPropertyAxiom.t -> unit
val fprint_Assertion :
  Format.formatter -> Assertion.t -> unit
val fprint_ontology : Format.formatter -> Ontology.t -> unit
val print_ontology_ch : Ontology.t -> out_channel -> unit
val save_ontology : Ontology.t -> string -> unit
val str_of_IRI : IRI.t -> string
val str_of_Datatype :
  Datatype.t -> string
val str_of_ConstrainingFacet :
  ConstrainingFacet.t -> string
val str_of_ObjectProperty :
  ObjectProperty.t -> string
val str_of_DataProperty :
  DataProperty.t -> string
val str_of_Class : Class.t -> string
val str_of_Individual :
  Individual.t -> string
val str_of_Literal :
  Literal.t -> string
val str_of_ObjectPropertyExpression :
  ObjectPropertyExpression.t -> string
val str_of_DataPropertyExpression :
  DataPropertyExpression.t -> string
val str_of_DataRange :
  DataRange.t -> string
val str_of_ClassExpression :
  ClassExpression.t -> string
val str_of_ClassExpressionAxiom :
  ClassExpressionAxiom.t -> string
val str_of_ObjectPropertyAxiom :
  ObjectPropertyAxiom.t -> string
val str_of_DataPropertyAxiom :
  DataPropertyAxiom.t -> string
val str_of_Assertion :
  Assertion.t -> string
