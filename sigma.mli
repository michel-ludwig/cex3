(***********************************************************)
(*  Copyright (C) 2010                                     *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
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

open Types

type t

val create_from_sets : StringSet.t * StringSet.t -> t

val create : string list * string list -> t

val get_concept_names : t -> StringSet.t

val get_role_names : t -> StringSet.t

val get_domain_concept_names : t -> StringSet.t

val get_range_concept_names : t -> StringSet.t

val is_role_name_contained : t -> string -> bool
val is_concept_name_contained : t -> string -> bool

(* kate: replace-tabs on; indent-width 2; *)