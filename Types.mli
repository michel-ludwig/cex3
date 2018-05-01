(***********************************************************)
(*  Copyright (C) 2010 - 2014                              *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
(*  University of Liverpool, and                           *)
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

open TypeSet

type concept = Name of string | Top | And of concept list | Exists of string * concept
                                                          | ExistsRoleConjunction of string list * concept
                                                          | ExistsUniversalRole of concept
                                                          | Domain of string
                                                          | Range of string

type axiom = ConceptInclusion of concept * concept | ConceptEquality of concept * concept | RoleInclusion of string * string

val is_atomic : concept -> bool
val is_concept_name : concept -> bool

val is_EL_ran_concept : concept -> bool
val contains_universal_role : concept -> bool
val contains_role_conjunction : concept -> bool

val concept_to_string_with_mapping : concept -> (string, string) Hashtbl.t -> string
val pretty_concept_to_string_with_mapping : concept -> (string, string) Hashtbl.t -> string
val print_concept_list_with_mapping : concept list -> (string, string) Hashtbl.t -> unit
val print_concept_with_mapping : concept -> (string, string) Hashtbl.t -> unit 
val pretty_print_concept_with_mapping : concept -> (string, string) Hashtbl.t -> unit 

val axiom_to_string_with_mapping : axiom -> (string, string) Hashtbl.t -> string
val pretty_axiom_to_string_with_mapping : axiom -> (string, string) Hashtbl.t -> string
val print_axiom_with_mapping : axiom -> (string, string) Hashtbl.t -> unit
val output_axiom_with_mapping : out_channel -> axiom -> (string, string) Hashtbl.t -> unit
val pretty_output_axiom_with_mapping : out_channel -> axiom -> (string, string) Hashtbl.t -> unit

val concept_to_string : concept -> string
val print_concept_list : concept list -> unit
val print_concept : concept -> unit 

val axiom_to_string : axiom -> string
val print_axiom : axiom -> unit
val output_axiom : out_channel -> axiom -> unit

module StringSet : TypeSet with type element = string
(* module RoleSet : TypeSet with type element = role *)
module IntSet : TypeSet with type element = int
module StringPairSet : TypeSet with type element = string * string
module StringSetSet : TypeSet with type element = StringSet.t
(* module ConceptSet : TypeSet with type element = concept *)
(* module AxiomSet : TypeSet with type element = axiom *)

val signature_of_concept : concept -> StringSet.t * StringSet.t
val signature_of_axiom : axiom -> StringSet.t * StringSet.t
val signature_of_axiom_list : axiom list -> StringSet.t * StringSet.t

val roles_occurring_in_axiom_list : axiom list -> StringSet.t

(* kate: replace-tabs on; indent-width 2; *)