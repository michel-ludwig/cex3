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

type t = { concept_names : StringSet.t;
           role_names : StringSet.t;
           domain_concept_names : StringSet.t;
           range_concept_names : StringSet.t;
         }

let create_from_sets (conceptNameSet, roleNameSet) =
  let domainConceptNameSet = StringSet.fold (fun newSet rname -> StringSet.add newSet (RoleMapping.map_role_to_domain_binding rname)) StringSet.empty roleNameSet in
  let rangeConceptNameSet = StringSet.fold (fun newSet rname -> StringSet.add newSet (RoleMapping.map_role_to_range_binding rname)) StringSet.empty roleNameSet in
  { concept_names = conceptNameSet;
    role_names = roleNameSet;
    domain_concept_names = domainConceptNameSet;
    range_concept_names = rangeConceptNameSet;
  }

let create (conceptNames, roleNames) =
  create_from_sets (StringSet.from_list conceptNames, StringSet.from_list roleNames)

let get_concept_names t =
  t.concept_names

let get_role_names t =
  t.role_names

let get_domain_concept_names t =
  t.domain_concept_names

let get_range_concept_names t =
  t.range_concept_names

let is_role_name_contained t rname =
  StringSet.mem t.role_names rname

let is_concept_name_contained t cname =
  StringSet.mem t.concept_names cname