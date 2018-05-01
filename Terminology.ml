(***********************************************************)
(*  Copyright (C) 2014                                     *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
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

open Types
open Utilities

exception FoundIt

type t = {
  preConceptHash : (string, StringSet.t) Hashtbl.t;
  postConceptHash : (string, StringSet.t) Hashtbl.t;
  directLinksHash : (string, StringSet.t) Hashtbl.t;
  associatedConjunctionsHash : (string, StringSet.t) Hashtbl.t;
  fillerToClosingEdgeHash : (string, StringPairSet.t) Hashtbl.t;
  roleDifferenceInformation : RoleDifference.t;
  nonConjunctiveHash : (string, StringSet.t) Hashtbl.t;
  existentialDefinitionHash : (string, string * string) Hashtbl.t;
  containsRangeAxioms : bool;
  rhsHash : (string, concept) Hashtbl.t;
  domainHash : (string, concept) Hashtbl.t;
  rangeHash : (string, concept) Hashtbl.t;
  conceptNames : StringSet.t;
  roleNames : StringSet.t;
}

let create ont roles =
  let normalisedOnt = Normalisation.normalise_ELHr_terminology ont in

  let (concepts, roles) = Types.signature_of_axiom_list normalisedOnt in
  
  let containsRangeAxiomsRef = ref false in
  begin
  try
    List.iter (fun axiom -> match axiom with
                              ConceptInclusion(Range(_), _) -> raise FoundIt;
                            | _ -> ())
              normalisedOnt;
  with FoundIt ->
    containsRangeAxiomsRef := true
  end;

(*  let nrDefinitions = ref 0 in
  let nrSimpleInclusions = ref 0 in
  let nrConjunctiveDef = ref 0 in
  List.iter (fun axiom -> match axiom with
                            ConceptEquality(_, And(_)) -> begin
                                                          incr nrDefinitions;
                                                          incr nrConjunctiveDef;
                                                          end
                          | ConceptEquality(_, _) -> incr nrDefinitions
                          | ConceptInclusion(_, _) -> incr nrSimpleInclusions
                          | _ -> ())
            normalisedOnt;
  print_endline("Number of simple inclusions: " ^ (string_of_int (!nrSimpleInclusions)));
  print_endline("Number of definitions: " ^ (string_of_int (!nrDefinitions)));
  print_endline("Number of conjunctive definitions: " ^ (string_of_int (!nrConjunctiveDef)));*)

(*print_endline("Normalised terminology1:");
List.iter (fun ax -> print_endline(axiom_to_string ax)) normalisedOnt;*)

  let (pre_concept_hash, post_concept_hash) = 
    (fun _ -> (* we use a function here to 'hide' 'ont1_sat', which consumes a lot of memory *)
          let owlOnt = Ontology.axioms_ELHr_to_Ontology normalisedOnt in

          assert(OntologyLanguage.is_horn owlOnt);
          
          Ontology.add_domain_range_concept_definitions roles owlOnt;

          print_endline("Saturating ontology 1...");
          let (ont_sat, ont_index) = ReasonerTBox.saturate owlOnt in
          let (pre_concept_hash, post_concept_hash) = execute_measure_time "Computing pre and post(concepts) for ontology 1..."
                                                                           (fun _ -> ReasonerTBox.compute_pre_post_for_concepts ont_sat); in
          (pre_concept_hash, post_concept_hash)
    ) ()
  in

  compact_heap ();

  let associatedConjunctionsHashtbl = Hashtbl.create 50 in
  let directLinksHashtbl = Hashtbl.create 50 in
  let fillerToClosingEdgeHashtbl = Hashtbl.create 50 in
  let nonConjunctiveHashtbl = Hashtbl.create 50 in
  let existentialDefinitionHashtbl = Hashtbl.create 50 in
  let rhsHashtbl = Hashtbl.create 50 in
  let domainHashtbl = Hashtbl.create 50 in
  let rangeHashtbl = Hashtbl.create 50 in

  let roleDifferenceInformationLocal = RoleDifference.create () in

  let add_to_unique_hash hash r c =
    if Hashtbl.mem hash r then
      failwith "Hash entry already present!"
    else
      Hashtbl.add hash r c
  in

  let add_to_domain_hash r c =
    add_to_unique_hash domainHashtbl r c
  in

  let add_to_range_hash r c =
    add_to_unique_hash rangeHashtbl r c
  in

  List.iter (fun ax -> match ax with
                           ConceptInclusion(Name(cname), (Name(dname) as rhs)) -> Hashtbl.add rhsHashtbl cname rhs;
                                                                                  StringSet.update_hash_with_element directLinksHashtbl cname dname
                         | ConceptInclusion(Name(cname), (And(l) as rhs)) -> Hashtbl.add rhsHashtbl cname rhs;
                                                                             List.iter (fun c -> match c with
                                                                                                   Name dname -> StringSet.update_hash_with_element directLinksHashtbl cname dname
                                                                                                 | _ -> failwith "Wrong constructor in 'Terminology.create()'!")
                                                                                       l
                         | ConceptInclusion(Name(cname), (Exists(_, _)  as rhs)) -> Hashtbl.add rhsHashtbl cname rhs
                         | ConceptEquality(Name(cname), (And(l) as rhs)) -> Hashtbl.add rhsHashtbl cname rhs;
                                                                            let conjuncts = List.rev_map (fun c -> match c with
                                                                                                                     Name dname -> dname
                                                                                                                   | _ -> failwith "Wrong constructor in 'Terminology.create()'!")
                                                                                                         l
                                                                            in
                                                                            List.iter (fun dname -> StringSet.update_hash_with_element associatedConjunctionsHashtbl dname cname)
                                                                                      conjuncts;
                                                                            Hashtbl.add nonConjunctiveHashtbl cname (StringSet.from_list conjuncts)
                         | ConceptEquality(Name(cname), (Exists(r, Name(dname)) as rhs)) -> Hashtbl.add rhsHashtbl cname rhs;
                                                                                            StringPairSet.update_hash_with_element fillerToClosingEdgeHashtbl dname (r, cname);
                                                                                            Hashtbl.add existentialDefinitionHashtbl cname (r, dname)
                         | ConceptInclusion(Domain(r), c) -> (add_to_domain_hash r c; Hashtbl.add rhsHashtbl (RoleMapping.map_role_to_domain_binding r) c;)
                         | ConceptInclusion(Range(r), c) -> (add_to_range_hash r c; Hashtbl.add rhsHashtbl (RoleMapping.map_role_to_range_binding r) c;)
                         | RoleInclusion(r1, r2) -> RoleDifference.processRoleInclusion roleDifferenceInformationLocal r1 r2
                         | _ -> failwith "Wrong constructor in 'Terminology.create()'!")
             normalisedOnt;

(*Hashtbl.iter (fun cname set -> let r = StringSet.for_all (fun dname -> not (Hashtbl.mem nonConjunctiveHashtbl dname))
                                                         set
                               in
                               if not r then
                                  failwith "Problem discovered")
nonConjunctiveHashtbl;*)
             
 {preConceptHash = pre_concept_hash;
  postConceptHash = post_concept_hash;
  directLinksHash = directLinksHashtbl;
  associatedConjunctionsHash = associatedConjunctionsHashtbl;
  fillerToClosingEdgeHash = fillerToClosingEdgeHashtbl;
  roleDifferenceInformation = roleDifferenceInformationLocal;
  nonConjunctiveHash = nonConjunctiveHashtbl;
  existentialDefinitionHash = existentialDefinitionHashtbl;
  containsRangeAxioms = !containsRangeAxiomsRef;
  rhsHash = rhsHashtbl;
  domainHash = domainHashtbl;
  rangeHash = rangeHashtbl;
  conceptNames = concepts;
  roleNames = roles;
  }

let get_domain_restrictions t r =
  try
    Some (Hashtbl.find t.domainHash r)
  with Not_found ->
    None

let get_range_restrictions t r =
  try
    Some (Hashtbl.find t.rangeHash r)
  with Not_found ->
    None

let contains_range_axioms t =
 t.containsRangeAxioms
  
let get_role_difference_information t =
  t.roleDifferenceInformation

let get_direct_edges t cname =
  try
    Hashtbl.find t.directLinksHash cname 
  with Not_found ->
    StringSet.empty

let get_associated_conjunctions t cname =
  try
    Hashtbl.find t.associatedConjunctionsHash cname 
  with Not_found ->
    StringSet.empty

let get_associated_closing_edges t cname =
  try
    Hashtbl.find t.fillerToClosingEdgeHash cname 
  with Not_found ->
    StringPairSet.empty

let is_primitive t cname =
  not (Hashtbl.mem t.rhsHash cname)
    
let get_concept_rhs t cname =
  try
    Hashtbl.find t.rhsHash cname
  with Not_found ->
    failwith ("Asked for RHS of a pseudo-primitive concept name! (" ^ cname ^ ")")

let non_conjunctive t cname =
  try
    Hashtbl.find t.nonConjunctiveHash cname
  with Not_found ->
    StringSet.singleton cname

let pre_concepts t cname =
  try
    Hashtbl.find t.preConceptHash cname
  with Not_found ->
    StringSet.singleton cname

let post_concepts t cname =
  try
    Hashtbl.find t.postConceptHash cname
  with Not_found ->
    StringSet.singleton cname  

let entails t cname1 cname2 =
  StringSet.mem (post_concepts t cname1) cname2

let entailed_concept_names t cname =
  post_concepts t cname

let entailed_by_concept_names t cname =
  pre_concepts t cname
  
let entailed_by_sigma_concept_names t sigma cname =
  let sigmaConceptNames = Sigma.get_concept_names sigma in
  StringSet.fold (fun set dname -> if StringSet.mem sigmaConceptNames dname then
                                     StringSet.add set dname
                                   else
                                     set)
                 StringSet.empty
                 (pre_concepts t cname)

let for_all_entailed_by_sigma_concept_names f t sigma cname =
  let sigmaConceptNames = Sigma.get_concept_names sigma in
  StringSet.for_all (fun dname -> not (StringSet.mem sigmaConceptNames dname)
                                  || f(dname))
                    (pre_concepts t cname)

let for_all_entailed_by_sigma_domain_as_concept_names f t sigma cname =
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.for_all (fun dname -> not (RoleMapping.is_role_domain_binding dname)
                                  || not (StringSet.mem sigmaRoleNames (RoleMapping.map_domain_binding_to_role dname))
                                  || f(dname))
                    (pre_concepts t cname)

let find_entailed_sigma_concept_names f t sigma cname =
  let sigmaConceptNames = Sigma.get_concept_names sigma in
  StringSet.find_object (fun dname -> if not (StringSet.mem sigmaConceptNames dname) then
                                        None
                                      else
                                        f(dname))
                        (post_concepts t cname)

let find_entailed_by_sigma_concept_names f t sigma cname =
  let sigmaConceptNames = Sigma.get_concept_names sigma in
  StringSet.find_object (fun dname -> if not (StringSet.mem sigmaConceptNames dname) then
                                        None
                                      else
                                        f(dname))
                        (pre_concepts t cname)

let find_entailed_by_sigma_domain_as_concept_names f t sigma cname =
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.find_object (fun dname -> if not (RoleMapping.is_role_domain_binding dname)
                                      || not (StringSet.mem sigmaRoleNames (RoleMapping.map_domain_binding_to_role dname))
                                      then
                                        None
                                      else
                                        f(dname))
                        (pre_concepts t cname)

let find_entailed_by_sigma_range_as_concept_names f t sigma cname =
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.find_object (fun dname -> if not (RoleMapping.is_role_range_binding dname)
                                      || (not (StringSet.mem sigmaRoleNames (RoleMapping.map_range_binding_to_role dname)))
                                      then
                                        None
                                      else
                                        f(dname))
                        (pre_concepts t cname)

let entailed_by_domain_concept_names t r =
  entailed_concept_names t (RoleMapping.map_role_to_domain_binding r)

let entailed_by_range_concept_names t r =
  entailed_concept_names t (RoleMapping.map_role_to_range_binding r)

let is_entailed_by_domain t r cname =
  entails t (RoleMapping.map_role_to_domain_binding r) cname
  
let is_entailed_by_range t r cname =
  entails t (RoleMapping.map_role_to_range_binding r) cname

let iter_entailed_by_range f t cname =
  StringSet.iter (fun cname -> if RoleMapping.is_role_range_binding cname then
                                 f (RoleMapping.map_range_binding_to_role cname))
                 (pre_concepts t cname)
  
let is_defined_as_existential t cname =
  Hashtbl.mem t.existentialDefinitionHash cname

let get_definition_of_existential t cname =
  Hashtbl.find t.existentialDefinitionHash cname
  
let is_defined_as_conjunction t cname =
  Hashtbl.mem t.nonConjunctiveHash cname

let get_definition_of_conjunction t cname =
  Hashtbl.find t.nonConjunctiveHash cname

let entailed_by_sigma_domain_as_concept_names t sigma cname =
  let pre = pre_concepts t cname in
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.fold (fun set cname -> if RoleMapping.is_role_domain_binding cname
                                      && StringSet.mem sigmaRoleNames (RoleMapping.map_domain_binding_to_role cname)
                                   then
                                     StringSet.add set cname
                                   else
                                     set)
                 StringSet.empty
                 pre

let entailed_by_sigma_range_as_concept_names t sigma cname =
  let pre = pre_concepts t cname in
  StringSet.fold (fun set cname -> if RoleMapping.is_role_range_binding cname
                                      && StringSet.mem (Sigma.get_role_names sigma) (RoleMapping.map_range_binding_to_role cname)
                                   then
                                     StringSet.add set cname
                                   else
                                     set)
                 StringSet.empty
                 pre

let concept_name_list_to_string_list l =
  let rec _concept_name_list_to_string_list l acc =
    match l with
      [] -> acc
    | ((Name cname)::tl) -> _concept_name_list_to_string_list tl (cname::acc)
    | _ -> failwith ("Invalid argument in 'concept_name_list_to_string_list'!")
  in
  _concept_name_list_to_string_list l []

                 
let handle_normalised_right_hand_side_expression c topCase conjunctiveCase someTopCase someConceptNameCase =
  match c with
    Top -> topCase ()
  | Name cname -> conjunctiveCase [cname]
  | And l -> conjunctiveCase (concept_name_list_to_string_list l)
  | Exists(r, Top) -> someTopCase r
  | Exists(r, Name cname) -> someConceptNameCase r cname
  | _ -> failwith ("Invalid constructor in 'handle_normalised_right_hand_side_expression'! (" ^ (concept_to_string c) ^ ")")

let get_signature t =
  (t.conceptNames, t.roleNames)
  
(* kate: replace-tabs on; indent-width 2; *)