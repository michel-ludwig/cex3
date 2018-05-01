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

let remove_from_list f l c =
  let rec remove_from_list_ l res =
    match l with [] -> res
            |  h::tail -> if f c h then
                            remove_from_list_ tail res
                          else
                            remove_from_list_ tail (h::res)
  in
    remove_from_list_ l []

let simplifyListOfConcepts pre_concepts_hash l =
  let not_subsumes_concept_names cname c =
    match c with
      Name cname2 -> not (StringSet.mem (Ontology.pre_concepts pre_concepts_hash cname2) cname)
    | _ -> true in

  let rec simplifyListOfConcepts_ l res =
    match l with
      [] -> res
    | (c::tail) -> match c with Name cname -> simplifyListOfConcepts_ (List.filter (not_subsumes_concept_names cname) tail)
                                                                      (c::(List.filter (not_subsumes_concept_names cname) res))
                                       | _ -> simplifyListOfConcepts_ tail res
  in
  simplifyListOfConcepts_ l l

(* kate: replace-tabs on; indent-width 2; *)