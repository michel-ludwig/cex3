(***********************************************************)
(*  Copyright (C) 2010 - 2014                              *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
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

open NameMapping
(* open Ontology *)

type concept = Name of string | Top | And of concept list | Exists of string * concept
                                                          | ExistsRoleConjunction of string list * concept
                                                          | ExistsUniversalRole of concept
                                                          | Domain of string
                                                          | Range of string

type axiom = ConceptInclusion of concept * concept | ConceptEquality of concept * concept | RoleInclusion of string * string

module StringSet = TypeSet.Make(struct type t = string
                                  let to_string = (fun x -> x)
                                end)

module IntSet = TypeSet.Make(struct type t = int
                                    let to_string = string_of_int
                             end)

module StringPairSet = TypeSet.Make(struct type t = string * string
                                           let to_string = fun (s1, s2) -> "(" ^ s1 ^ ", " ^ s2 ^ ")"
                                    end)

module StringSetSet = TypeSet.Make(struct type t = StringSet.t
                                           let to_string = StringSet.to_string
                                    end)

let is_atomic c =
  match c with Top -> true
  | Name _ -> true
  | _ -> false

let is_concept_name c =
  match c with (Name _) -> true
  | _ -> false
  
let rec is_EL_ran_concept c =
  match c with
    Name _ -> true
  | Top -> true
  | And l -> List.for_all is_EL_ran_concept l
  | Exists(_, c) -> is_EL_ran_concept c
  | ExistsRoleConjunction(_, _) -> false
  | ExistsUniversalRole _ -> false
  | Domain _ -> true
  | Range _ -> true

let rec contains_universal_role c =
  match c with
    Name _ -> false
  | Top -> false
  | And l -> List.exists contains_universal_role l
  | Exists(_, c) -> contains_universal_role c
  | ExistsRoleConjunction(_, c) -> contains_universal_role c
  | ExistsUniversalRole _ -> true
  | Domain _ -> false
  | Range _ -> false

let rec contains_role_conjunction c =
  match c with
    Name _ -> false
  | Top -> false
  | And l -> List.exists contains_role_conjunction l
  | Exists(_, c) -> contains_role_conjunction c
  | ExistsRoleConjunction(_, _) -> true
  | ExistsUniversalRole c -> contains_role_conjunction c
  | Domain _ -> false
  | Range _ -> false

let emptyHash = Hashtbl.create 1

let createWhiteString n =
  let rec createWhiteString_ n acc =
    match n with 0 -> acc
                | _ -> createWhiteString_ (n-1) (acc ^ " ")
  in
  createWhiteString_ n ""

let rec concept_to_string_with_mapping c hash =
  match c with 
    Name cname -> lookupMapping hash cname
  | Top -> "(Top)"
  | And l -> (match l with [] -> failwith "Empty list." | _ -> "(and " ^ (concept_list_to_string_with_mapping l hash) ^ ")")
  | Exists(r, c) -> "(some " ^ (lookupMapping hash r) ^ " " ^ (concept_to_string_with_mapping c hash) ^ ")"
  | ExistsRoleConjunction(rlist, c) -> let roleListToConjunctionString l =
                                         "(and" ^ (List.fold_left (fun str rname -> str ^ " " ^ (lookupMapping hash rname)) "" l) ^ ")"
                                       in
                                       "(some " ^ (roleListToConjunctionString rlist) ^ " " ^ (concept_to_string_with_mapping c hash) ^ ")"
  | ExistsUniversalRole(c) -> "(some :universal " ^ (concept_to_string_with_mapping c hash) ^ ")"
  | Domain(r) -> "(domain " ^ (lookupMapping hash r) ^ ")"
  | Range(r) -> "(range " ^ (lookupMapping hash r) ^ ")"
and concept_list_to_string_with_mapping l hash =
  match l with [] -> ""
      |      c::[] -> concept_to_string_with_mapping c hash
      |       c::t -> (concept_to_string_with_mapping c hash) ^ " " ^ (concept_list_to_string_with_mapping t hash)


let rec pretty_concept_to_string_with_mapping_ c hash pos =
  match c with 
    Name cname -> lookupMapping hash cname
  | Top -> "(Top)"
  | And l -> (match l with [] -> failwith "Empty list."
                          | _ -> "(and " ^ (pretty_concept_list_to_string_with_mapping_ l hash (pos + 5)) ^ ")")
  | Exists(r, c) -> let pre = "(some " ^ (lookupMapping hash r) ^ " " in
                    pre ^ (pretty_concept_to_string_with_mapping_ c hash (pos + String.length pre)) ^ ")"
  | ExistsRoleConjunction(rlist, c) -> let roleListToConjunctionString l =
                                         "(and" ^ (List.fold_left (fun str rname -> str ^ " " ^ (lookupMapping hash rname)) "" l) ^ ")"
                                       in
                                       let pre = "(some " ^ (roleListToConjunctionString rlist) ^ " " in
                                       pre ^ (pretty_concept_to_string_with_mapping_ c hash (pos + String.length pre)) ^ ")"
  | ExistsUniversalRole(c) -> "(some :universal " ^ (concept_to_string_with_mapping c hash) ^ ")"
  | Domain(r) -> "(domain " ^ (lookupMapping hash r) ^ ")"
  | Range(r) -> "(range " ^ (lookupMapping hash r) ^ ")"
and pretty_concept_list_to_string_with_mapping_ l hash pos =
  match l with [] -> ""
      |      c::[] -> pretty_concept_to_string_with_mapping_ c hash pos
      |       c::t -> (pretty_concept_to_string_with_mapping_ c hash pos) ^ "\n" ^
                      (createWhiteString pos) ^ (pretty_concept_list_to_string_with_mapping_ t hash pos)

let pretty_concept_to_string_with_mapping c hash =
  pretty_concept_to_string_with_mapping_ c hash 0

let print_concept_list_with_mapping l hash = 
  print_string (concept_list_to_string_with_mapping l hash)

let print_concept_with_mapping c hash = 
  print_string (concept_to_string_with_mapping c hash)

let pretty_print_concept_with_mapping c hash = 
  print_string (pretty_concept_to_string_with_mapping c hash)

let concept_to_string c =
  concept_to_string_with_mapping c emptyHash

let concept_list_to_string l =
  concept_list_to_string_with_mapping l emptyHash

let print_concept c =
  print_concept_with_mapping c emptyHash

let print_concept_list l =
  print_string (concept_list_to_string l)

let axiom_to_string_with_mapping ax hash =
  match ax with
    ConceptInclusion(c1, c2) -> ("(implies " ^ (concept_to_string_with_mapping c1 hash) ^ " " ^ (concept_to_string_with_mapping c2 hash) ^ ")")
  | ConceptEquality(c1, c2) -> ("(equivalent " ^ (concept_to_string_with_mapping c1 hash) ^ " " ^ (concept_to_string_with_mapping c2 hash) ^ ")")
  | RoleInclusion(r1, r2) -> ("(implies-role " ^ (lookupMapping hash r1) ^ " " ^ (lookupMapping hash r2) ^ ")")

let pretty_axiom_to_string_with_mapping ax hash =
  match ax with
    ConceptInclusion(c1, c2) -> ("(implies " ^ (pretty_concept_to_string_with_mapping_ c1 hash 9) ^ "\n" ^
                                 (createWhiteString 9) ^ (pretty_concept_to_string_with_mapping_ c2 hash 9) ^ ")")
  | ConceptEquality(c1, c2) -> ("(equivalent " ^ (pretty_concept_to_string_with_mapping_ c1 hash 12) ^ "\n" ^
                                (createWhiteString 12) ^ (pretty_concept_to_string_with_mapping_ c2 hash 12) ^ ")")
  | RoleInclusion(r1, r2) -> ("(implies-role " ^ (lookupMapping hash r1) ^ "n" ^
                               (createWhiteString 14) ^ (lookupMapping hash r2) ^ ")")

let print_axiom_with_mapping ax hash =
  print_string (axiom_to_string_with_mapping ax hash)

let output_axiom_with_mapping outChannel ax hash =
  output_string outChannel (axiom_to_string_with_mapping ax hash)

let pretty_output_axiom_with_mapping outChannel ax hash =
  output_string outChannel (pretty_axiom_to_string_with_mapping ax hash)

let axiom_to_string ax =
  axiom_to_string_with_mapping ax emptyHash

let print_axiom ax =
  print_axiom_with_mapping ax emptyHash

let output_axiom outChannel ax =
  output_axiom_with_mapping outChannel ax emptyHash

let rec signature_of_concept c =
  match c with
    Top -> (StringSet.empty, StringSet.empty)
  | Name cname -> (StringSet.singleton cname, StringSet.empty)
  | Exists(r, c) -> let (concepts, roles) = signature_of_concept c in
                    (concepts, StringSet.add roles r)
  | ExistsRoleConjunction(l, c) -> let (concepts, roles) = signature_of_concept c in
                                   (concepts, StringSet.union (StringSet.from_list l) roles) 
  | ExistsUniversalRole(c) -> signature_of_concept c
  | Domain(r)-> (StringSet.empty, StringSet.singleton r)
  | Range(r) -> (StringSet.empty, StringSet.singleton r)
  | And(l) -> List.fold_left (fun (concepts, roles) c -> let (cConcepts, cRoles) = signature_of_concept c in
                                                         (StringSet.union concepts cConcepts,
                                                          StringSet.union roles cRoles))
                             (StringSet.empty, StringSet.empty)
                             l

let signature_of_axiom ax =
  match ax with
   ConceptInclusion(c1, c2) -> let (concepts1, roles1) = signature_of_concept c1 in
                               let (concepts2, roles2) = signature_of_concept c2 in
                               (StringSet.union concepts1 concepts2,
                                StringSet.union roles1 roles2)
 | ConceptEquality(c1, c2) -> let (concepts1, roles1) = signature_of_concept c1 in
                              let (concepts2, roles2) = signature_of_concept c2 in
                              (StringSet.union concepts1 concepts2,
                               StringSet.union roles1 roles2)
 | RoleInclusion(r1, r2) -> (StringSet.empty, StringSet.add (StringSet.singleton r2) r1)

let signature_of_axiom_list l =
  List.fold_left (fun (concepts, roles) ax -> let (axConcepts, axRoles) = signature_of_axiom ax in
                                              (StringSet.union concepts axConcepts,
                                               StringSet.union roles axRoles))
                 (StringSet.empty, StringSet.empty)
                 l

let roles_occurring_in_axiom_list l =
  let (_, roles) = signature_of_axiom_list l in
  roles


(* kate: replace-tabs on; indent-width 2; *)