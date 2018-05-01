(***********************************************************)
(*  Copyright (C) 2010-2014                                *)
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

open Ontology
open Owl2
open Types

let newSymbolCounter = ref 0
let conceptHash = Hashtbl.create 50

let newRenaming _ =
  let res = ":CHK_REN" ^ (string_of_int !newSymbolCounter) in
  incr newSymbolCounter;
  res

let setupRenaming ont1 ont2 c =
  if not (Hashtbl.mem conceptHash c) then
    begin
    let name = match c with 
                Name cname -> let conceptNameClassExpression1 = concept_name_class_expression ont1 cname in
                              let conceptNameClassExpression2 = concept_name_class_expression ont2 cname in
                              (* We have to ensure that 'cname' occurs in both ontologies as otherwise *)
                              (* 'ReasonerTBox.find_implied' could fail with a 'Not_found' exception.  *)
                              Ontology.add_ClassExpressionAxiom ont1 (subsumption_axiom ont1 conceptNameClassExpression1
                                                                                             conceptNameClassExpression1);
                              Ontology.add_ClassExpressionAxiom ont2 (subsumption_axiom ont2 conceptNameClassExpression2
                                                                                             conceptNameClassExpression2);
                              cname
              | _ -> let ren = newRenaming () in
                     let renamingClassExpression1 = concept_name_class_expression ont1 ren in
                     let renamingClassExpression2 = concept_name_class_expression ont2 ren in
                     let owlConcept1 = concept_ELran_to_OWL ont1 c in
                     let owlConcept2 = concept_ELran_to_OWL ont2 c in
                     Ontology.add_ClassExpressionAxiom ont1 (definition_axiom ont1 renamingClassExpression1
                                                                                   owlConcept1);
                     Ontology.add_ClassExpressionAxiom ont2 (definition_axiom ont2 renamingClassExpression2
                                                                                   owlConcept2);
                     ren
    in
    Hashtbl.add conceptHash c name
    end

let lookupRenaming c =
  try
    Hashtbl.find conceptHash c 
  with Not_found -> failwith "Renaming not found!"

let setupRenamings nameMapping ont1 ont2 l =
  List.fold_left (fun newList (lhs, rhs) -> if (is_EL_ran_concept lhs) && (is_EL_ran_concept rhs) then
                                              begin
                                              setupRenaming ont1 ont2 lhs;
                                              setupRenaming ont1 ont2 rhs;
                                              (lhs, rhs)::newList
                                              end
                                            else
                                              begin
                                              print_endline("\tSkipping " ^ (concept_to_string_with_mapping lhs nameMapping)
                                                            ^ " => " ^ (concept_to_string_with_mapping rhs nameMapping));
                                              newList
                                              end)
                 []
                 l

let checkCounterExample post_concept_hash1 post_concept_hash2 lhs rhs =
  let result = ref true in
  let lhsCName = lookupRenaming lhs in
  let rhsCName = lookupRenaming rhs in
  if not (StringSet.mem (Ontology.post_concepts post_concept_hash1 lhsCName) rhsCName) then
    begin
    print_endline("For counter-example " ^ (concept_to_string lhs) ^ " " ^ (concept_to_string rhs) ^ ": ");
    print_endline("FAILED: T1 does not imply this counter-example!");
    result := false;
    end;
  if StringSet.mem (Ontology.post_concepts post_concept_hash2 lhsCName) rhsCName then
    begin
    print_endline("For counter-example " ^ (concept_to_string lhs) ^ " " ^ (concept_to_string rhs) ^ ": ");
    print_endline("FAILED: T2 implies this counter-example!");
    result := false;
    end;

  !result

let checkConceptCounterExamples nameMapping axioms1 axioms2 l =
  let ont1 = Ontology.axioms_ELHr_to_Ontology axioms1 in
  let ont2 = Ontology.axioms_ELHr_to_Ontology axioms2 in
  print_endline "Setting up renamings...";
  (* we can only handle EL^\ran counter-examples *)
  let elRanCexList = setupRenamings nameMapping ont1 ont2 l in
  if not (OntologyLanguage.is_horn ont1) then
    begin
    failwith "Ontology1 is not Horn!";
    end;
  if not (OntologyLanguage.is_horn ont2) then
    begin
    failwith "Ontology2 is not Horn!";
    end;
(* Owl2IO.print_ontology_ch ont1 stdout; *)
  print_endline "Saturating extended ontology 1...";
  let post_concept_hash1 = 
    (fun _ -> (* we use a function here to 'hide' 'ont1_sat', which consumes a lot of memory *)
        print_endline("Saturating ontology 1...");
        let (ont1_sat, _) = ReasonerTBox.saturate ont1 in
        Utilities.compact_heap();
        let post_concept_hash1 = ReasonerTBox.compute_post_for_concepts ont1_sat; in
        post_concept_hash1
    ) () in
  Utilities.compact_heap();
  print_endline "Saturating extended ontology 2...";
  let post_concept_hash2 = 
    (fun _ -> (* we use a function here to 'hide' 'ont2_sat', which consumes a lot of memory *)
        print_endline("Saturating ontology 2...");
        let (ont2_sat, _) = ReasonerTBox.saturate ont2 in
        Utilities.compact_heap();
        let post_concept_hash2 = ReasonerTBox.compute_post_for_concepts ont2_sat; in
        post_concept_hash2
    ) () in
  Utilities.compact_heap();
  print_endline "Checking counter-examples...";
  (* we check all the counter-examples for their correctness *)
  let result = List.for_all (fun (lhs, rhs) -> checkCounterExample post_concept_hash1 post_concept_hash2 lhs rhs) elRanCexList in
  if result then
    print_endline "All checks passed."
  else
    print_endline "Some checks failed!"
  
(* kate: replace-tabs on; indent-width 2; *)