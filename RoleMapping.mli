(***********************************************************)
(*  Copyright (C) 2010                                     *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
(*  University of Liverpool                                *)
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

val map_role_to_domain_binding : string -> string
val map_role_to_range_binding : string -> string

val map_domain_binding_to_role : string -> string
val map_range_binding_to_role : string -> string

(* Returns true iff only the given concept name does not represent the range or domain of a role *)
val is_proper_concept_name : string -> bool

val is_proper_concept_name_or_role_domain_binding : string -> bool

val is_role_domain_binding : string -> bool

val is_role_range_binding : string -> bool

(* kate: replace-tabs on; indent-width 2; *)