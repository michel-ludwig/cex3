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

open Utilities

let map_role_to_domain_binding roleName =
  ":DOM_" ^ roleName

let map_role_to_range_binding roleName =
  ":RAN_" ^ roleName

let is_proper_concept_name cname =
  ((String.length cname) != 0 && not (begins_with cname ":DOM_")
                              && not (begins_with cname ":RAN_"))

let is_role_domain_binding cname =
  ((String.length cname) != 0 && (begins_with cname ":DOM_"))

let is_role_range_binding cname =
  ((String.length cname) != 0 && (begins_with cname ":RAN_"))

let is_proper_concept_name_or_role_domain_binding cname =
  ((String.length cname) != 0 && not (begins_with cname ":RAN_"))

let map_domain_binding_to_role binding =
  String.sub binding 5 (String.length binding - 5)

let map_range_binding_to_role binding = 
  String.sub binding 5 (String.length binding - 5)

(* kate: replace-tabs on; indent-width 2; *) 