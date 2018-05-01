(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010                                     *)
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

open Owl2
open Types

type concept_record = private {
    mutable c_impl : ClassExpression.Set.t;    
    c_conj : ClassExpression.t ClassExpression.HMap.t;
    mutable c_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
    mutable c_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ObjectProperty.Map.t;
  }
type role_record = private {    
    r_succ :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.HMap.t;
    r_succi :
    (ClassExpression.Set.t * ClassExpression.Set.t)
    ClassExpression.HMap.t;
    mutable r_sibl : ObjectProperty.Set.t;
    mutable r_isibl : ObjectProperty.Set.t;
    mutable r_sibli : ObjectProperty.Set.t;
    mutable r_isibli : ObjectProperty.Set.t;
  }
type t = private {
    hcr : concept_record ClassExpression.HMap.t;
    hrr : role_record ObjectProperty.HMap.t;
  }
(*|val empty_concept_record : concept_record*)
(*|val empty_role_record : role_record      *)
val find_concept_record : t -> ClassExpression.t -> concept_record
val find_role_record : t -> ObjectProperty.t -> role_record
val init : Ontology.t -> t
val print_statistics : t -> unit

(* type concept_name_type = PseudoPrimitive | Conjunctive | Exists *)

(* For A = C or A <= C occurring in the given terminology, 'C' is returned *)
(* FIXME: the occurrence hash can be removed  *)
val get_concept_rhs_in_terminology : t -> Ontology.t -> Ontology.occurrence_hash -> string -> ClassExpression.t

(* For A = some r B in a terminology, returns (r, B) *)
val get_rhs_of_definition_in_terminology : t -> Ontology.t -> Ontology.occurrence_hash -> string -> string * string

(* For A = some r B, or A => some r B, or A => some r top in a normalised terminology, returns Some r *)
val get_role_successor_in_terminology : t -> Ontology.t -> Ontology.occurrence_hash -> string -> string option

val is_primitive : t -> StringSet.t -> string -> bool

val is_pseudo_primitive : t -> Ontology.occurrence_hash -> string -> bool

val is_conjunctive : t -> Ontology.t -> Ontology.occurrence_hash -> string -> bool

(* For A <=> And(B_1, ..., B_n) or A => And(B_1, ..., B_n), this function returns Some [B_1, ..., B_n]. *)
(* For A <=> exists r.B, or A => exists r.B, or A => exists r.top, this function returns None. *)
val normalised_get_conjunctive_rhs : t -> Ontology.t -> Ontology.occurrence_hash -> StringSet.t -> string -> string list option

(* Returns true iff the RHS of the given concept name 'A' is of the form (some r B) in the terminology *)
val is_exists_definition : t -> Ontology.t -> Ontology.occurrence_hash -> string -> bool


(* kate: replace-tabs on; indent-width 2; *)
