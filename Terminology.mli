(***********************************************************)
(*  Copyright (C) 2014                                     *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
(*  TU Dresden                                             *)
(*                                                         *)
(*  This program is free software; you can redistribute    *)
(*  it and/or modify it under the terms of the GNU         *)
(*  General Public License as published by the Free        *)
(*  Software Foundation; either version 3 of the License,  *)
(*  or (at your option) any later version.                 *)
(*                                                         *)
(*  This program is distributed in the hope that it will   *)
(*  be useful, but WITHOUT ANY WARRANTY; without even      *)
(*  the implied warranty of MERCHANTABILITY or FITNESS     *)
(*  FOR A PARTICULAR PURPOSE.  See the GNU General Public  *)
(*  License for more details.                              *)
(*                                                         *)
(*  You should have received a copy of the GNU General     *)
(*  Public License along with this program; if not, see    *)
(*  <http://www.gnu.org/licenses/>.                        *)
(***********************************************************)

open Owl2
open Types

type t

val create : axiom list -> StringSet.t -> t

val contains_range_axioms : t -> bool

val get_role_difference_information : t -> RoleDifference.t

val get_direct_edges : t -> string -> StringSet.t
val get_associated_conjunctions : t -> string -> StringSet.t
val get_associated_closing_edges : t -> string -> StringPairSet.t

val is_primitive : t -> string -> bool

val get_concept_rhs : t -> string -> concept

val non_conjunctive : t -> string -> StringSet.t

val entails : t -> string -> string -> bool
val entailed_concept_names : t -> string -> StringSet.t
val entailed_by_concept_names : t -> string -> StringSet.t
val entailed_by_sigma_concept_names : t -> Sigma.t -> string -> StringSet.t

val for_all_entailed_by_sigma_concept_names : (string -> bool) -> t -> Sigma.t -> string -> bool
val for_all_entailed_by_sigma_domain_as_concept_names : (string -> bool) -> t -> Sigma.t -> string -> bool

val find_entailed_sigma_concept_names : (string -> string option) -> t -> Sigma.t -> string -> string option

val find_entailed_by_sigma_concept_names : (string -> string option) -> t -> Sigma.t -> string -> string option
val find_entailed_by_sigma_domain_as_concept_names : (string -> string option) -> t -> Sigma.t -> string -> string option
val find_entailed_by_sigma_range_as_concept_names : (string -> string option) -> t -> Sigma.t -> string -> string option

val entailed_by_domain_concept_names : t -> string -> StringSet.t
val entailed_by_range_concept_names : t -> string -> StringSet.t
val is_entailed_by_domain : t -> string -> string -> bool
val is_entailed_by_range : t -> string -> string -> bool

val iter_entailed_by_range : (string -> unit) -> t -> string -> unit

val is_defined_as_existential : t -> string -> bool
val get_definition_of_existential : t -> string -> string * string

val is_defined_as_conjunction : t -> string -> bool
val get_definition_of_conjunction : t -> string -> StringSet.t

val entailed_by_sigma_domain_as_concept_names : t -> Sigma.t -> string -> StringSet.t
val entailed_by_sigma_range_as_concept_names : t -> Sigma.t -> string -> StringSet.t

val handle_normalised_right_hand_side_expression : concept -> (unit -> 'a)
                                                           -> (string list -> 'a)
                                                           -> (string -> 'a)
                                                           -> (string -> string -> 'a)
                                                           -> 'a

val get_domain_restrictions : t -> string -> concept option
val get_range_restrictions : t -> string -> concept option

val get_signature : t -> StringSet.t * StringSet.t

(* kate: replace-tabs on; indent-width 2; *)