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

open Types
open Utilities


type renaming_type = Equality | Subsumption
exception CycleFound of StringSet.t

let symbol_counter = ref (-1)
let renamingHash = Hashtbl.create 50

(* WARNING: We assume that the character ':' cannot occur in regular concept names! *)
let new_concept_name _ =
  incr symbol_counter;
  ":RN" ^ (string_of_int !symbol_counter)

let find_renaming_class_expression typ exp =
  try
    let conceptName = Hashtbl.find renamingHash (typ, exp) in
    (conceptName, Name conceptName)
  with Not_found ->
    let newConceptName = new_concept_name () in
    Hashtbl.add renamingHash (typ, exp) newConceptName;
    (newConceptName, Name newConceptName)

let addToLeftHandSideSet leftHandSideSet cname =
  leftHandSideSet := StringSet.add !leftHandSideSet cname

(* Splits expression of the form A = some r. Top into A \sqsubseteq some r. Top and dom(r) \sqsubseteq A *)
let splitDomainExistsExpression r newTotalCName newExpression =
  [ConceptInclusion(Name newTotalCName, newExpression);
   ConceptInclusion(Domain(r), Name newTotalCName)]

let rename_expression typ newExpression =
(*    try
      (concept_name_class_expression ont (Hashtbl.find renamingHash (ont, typ, newExpression)), [])
    with Not_found ->*)
      begin
      let (newTotalCName, newTotalCNameExpression) = find_renaming_class_expression typ newExpression in
      let newAxiom = match typ with
                      Equality -> (match newExpression with (* we don't want to have expressions of the form A = some r. Top in the *)
                                     Exists(r, Top) -> splitDomainExistsExpression r newTotalCName newExpression
                                   | _ -> begin                                      (* terminology *)
                                            [ConceptEquality(newTotalCNameExpression, newExpression)]
                                          end)
                    | Subsumption -> [ConceptInclusion(newTotalCNameExpression, newExpression)]
      in
      (newTotalCNameExpression, newAxiom)
      end

let rec rename typ exp renameExists renameAnd =
(*    try
      (concept_name_class_expression ont (Hashtbl.find renamingHash (ont, typ, exp)), [])
    with Not_found ->*)
      begin
      match exp with
        Name _ -> (exp, [])
      | Top -> (Top, [])
      | And l ->
            let (newL, newAxiomList) = List.fold_left (fun (l', axiomList) c -> let (newConcept, newAxiomList) = rename typ c true false in
                                                                                (newConcept::l', List.rev_append newAxiomList axiomList))
                                                      ([], [])
                                                      l
            in
            let newExpression = And newL
            in
              if renameAnd then
                let (newTotalCNameExpression, newAxioms) = rename_expression typ newExpression
                in
                (newTotalCNameExpression, List.rev_append newAxioms newAxiomList)
              else
                (newExpression, newAxiomList)
      | Exists (r, e) ->
          let (newE, newAxioms1) = rename typ e true true in
          let newExpression = Exists(r, newE)
          in
          if is_atomic newE && (not renameExists) then
            (newExpression, newAxioms1)
          else (* rename 'newExpression' *)
            let (newTotalCNameExpression, newAxioms2) = rename_expression typ newExpression
            in
            (newTotalCNameExpression, List.rev_append newAxioms1 newAxioms2)
      | _ -> failwith "Wrong constructor encountered."
      end

(* 'rename_complex_exists_rC t occurrenceHash exp' renames expressions of the form (some r C)  *)
(* in 'exp' if they don't occur on the top-most level. If C is not a concept name or Top, then *)
(* it is renamed as well.                                                                      *)
(* The expression that should replace 'exp' is returned, together with a list of axioms        *)
(* which define the newly introduced concept names. The new concept names are also inserted    *)
(* into 'occurenceHash'.                                                                       *)
let rename_complex_exists_rC typ exp =
  rename typ exp false false

let rename_complex typ exp =
  rename typ exp true true

let normalise_subsumption_axiom conceptName exp =
  let (newExp, l) = rename_complex_exists_rC Subsumption exp in 
  (ConceptInclusion (Name conceptName, newExp))::l

let normalise_equivalence_axiom conceptName exp =
  let (a, l) = rename_complex_exists_rC Equality exp in
  (match a with (* we don't want to have expressions of the form A = some r. Top in the terminology *)
     Exists(r, Top) -> List.rev_append (splitDomainExistsExpression r conceptName a) l
   | _ -> begin
            (ConceptEquality (Name conceptName, a))::l
          end)

let normalise_domain_axiom r exp =
  let (a, l) = rename_complex Subsumption exp
  in
  (ConceptInclusion(Domain r, a))::l

let normalise_range_axiom r exp =
  let (a, l) = rename_complex Subsumption exp
  in
  (ConceptInclusion(Range r, a))::l

let new_symbols_introduced _ = !symbol_counter + 1

let rec merge_conjunctions_in_concept_and_remove_top_from_conjuncts c =
  match c with
    Top -> Top
  | Name _ as c -> c
  | Exists(r, c) -> Exists(r, merge_conjunctions_in_concept_and_remove_top_from_conjuncts c)
  | ExistsRoleConjunction(l, c) -> ExistsRoleConjunction(l, merge_conjunctions_in_concept_and_remove_top_from_conjuncts c)
  | ExistsUniversalRole(c) -> ExistsUniversalRole(merge_conjunctions_in_concept_and_remove_top_from_conjuncts c)
  | Domain(_) as c -> c
  | Range(_) as c -> c
  | And(l) -> let l' = List.fold_left (fun newL c -> (merge_conjunctions_in_concept_and_remove_top_from_conjuncts c)::newL)
                                      []
                                      l in
              let l'' = List.fold_left (fun newL c -> match c with
                                                        Top -> newL
                                                      | And(lp) -> List.rev_append lp newL
                                                      | _ as c' -> c'::newL)
                                       []
                                       l'
              in
              match l'' with
               [] -> Top
              | [c] -> c
              | _ -> And(l'')

let merge_conjunctions_in_axiom_and_remove_top_from_conjuncts ax =
  match ax with
    ConceptInclusion(c1, c2) -> ConceptInclusion(merge_conjunctions_in_concept_and_remove_top_from_conjuncts c1,
                                                 merge_conjunctions_in_concept_and_remove_top_from_conjuncts c2)
 | ConceptEquality(c1, c2) -> ConceptEquality(merge_conjunctions_in_concept_and_remove_top_from_conjuncts c1,
                                              merge_conjunctions_in_concept_and_remove_top_from_conjuncts c2)
 | RoleInclusion(_, _) as ax -> ax

let is_normalised_EL_concept c =
 match c with
   Top -> true
 | Name _ -> true
 | Exists(_, c) -> Types.is_atomic c
 | And l -> List.for_all Types.is_concept_name l
 | _ -> false

let is_normalised_ELHr_axiom ax =
  match ax with
     ConceptInclusion(Name(_), c) -> is_normalised_EL_concept c
    | ConceptEquality(Name(_), Exists(_, Top)) -> false
    | ConceptEquality(Name(_), c) -> is_normalised_EL_concept c
    | ConceptInclusion(Domain(_), c) -> is_normalised_EL_concept c
    | ConceptInclusion(Range(_), c) -> is_normalised_EL_concept c
    | RoleInclusion(_, _) -> true
    | _ -> false

let conjunction_of_concept_names_to_StringSet l =
  List.fold_left (fun set c -> match c with (Name cname) -> StringSet.add set cname
                                            | _ -> failwith "Invalid argument in 'conjunction_of_concept_names_to_StringSet'!")
                 StringSet.empty
                 l

let stringSet_to_list_of_concept_names set =
  StringSet.fold (fun l cname -> (Name cname)::l)
                 []
                 set
    
let extract_conjunctive_equalities l =
  List.fold_left (fun (conjL, newL) ax -> match ax with
                                            ConceptEquality(Name(cname), And(l)) -> ((cname, conjunction_of_concept_names_to_StringSet l)::conjL, newL)
                                          | _ -> (conjL, ax::newL))
                  ([], [])
                  l

let break_conjunctive_cycles (l : (string * StringSet.t) list )=
  let hash = Hashtbl.create 50 in

  let rec breakCycles visited cname =
    if StringSet.mem visited cname then
      raise (CycleFound (StringSet.empty));
    let newVisited = StringSet.add visited cname in
    if not (Hashtbl.mem hash cname) then
      StringSet.singleton cname
    else
      begin
      let conjuncts = Hashtbl.find hash cname in
      let newConjuncts = StringSet.fold (fun newSet dname -> try
                                                               StringSet.union newSet (breakCycles newVisited dname)
                                                             with CycleFound(set) ->
                                                               raise (CycleFound(StringSet.union (StringSet.remove conjuncts dname)
                                                                                                set)))
                         StringSet.empty
                         conjuncts;
      in
      newConjuncts
      end
  in
  List.iter (fun (cname, set) -> Hashtbl.add hash cname set)
            l;
  let construct_conjunction set =
    match stringSet_to_list_of_concept_names set with
     [] -> Top
    | [c] -> c
    | l -> And l
  in
  List.fold_left (fun newList (cname, _) -> try
                                              let set = breakCycles StringSet.empty cname in
                                              (ConceptEquality(Name cname, construct_conjunction set))::newList
                                            with CycleFound(set) ->
                                              (ConceptInclusion(Name cname, construct_conjunction set))::newList)
                 []
                 l

let mergeDomainAndRangeAxioms l =
  let updateListHash hash rname c =
    if not (Hashtbl.mem hash rname) then
      Hashtbl.add hash rname (ref [c])
    else
      begin
      let l = Hashtbl.find hash rname in
      l := c::!l
      end
  in

  let domainHash = Hashtbl.create 50 in
  let rangeHash = Hashtbl.create 50 in
  
  let addToDomainHash rname c =
    updateListHash domainHash rname c
  in

  let addToRangeHash rname c =
    updateListHash rangeHash rname c
  in
  
  let axiomsWithoutDomainRangeAxioms = List.fold_left (fun newL ax -> match ax with
                                                                        ConceptInclusion(Domain(r), c) -> addToDomainHash r c; newL
                                                                      | ConceptInclusion(Range(r), c) -> addToRangeHash r c; newL
                                                                      | _ -> ax::newL)
                                                       []
                                                       l
  in
  Hashtbl.fold (fun rname conjuncts l -> (ConceptInclusion(Domain(rname), And !conjuncts))::l)
               domainHash
               (Hashtbl.fold (fun rname conjuncts l -> (ConceptInclusion(Range(rname), merge_conjunctions_in_concept_and_remove_top_from_conjuncts (And (!conjuncts))))::l)
                             rangeHash
                             axiomsWithoutDomainRangeAxioms)


let normalise_ELHr_terminology l =
  let mergedConjunctionsAxioms = List.fold_left (fun newL ax -> (merge_conjunctions_in_axiom_and_remove_top_from_conjuncts ax)::newL)
                                                []
                                                l
  in
  let normalisedAxioms1 = List.fold_left (fun l' ax -> let l'' = match ax with
                                                                   ConceptInclusion(Name(cname), c) -> normalise_subsumption_axiom cname c
                                                                 | ConceptEquality(Name(cname), c) -> normalise_equivalence_axiom cname c
                                                                 | ConceptInclusion(Domain(r), c) -> normalise_domain_axiom r c
                                                                 | ConceptInclusion(Range(r), c) -> normalise_range_axiom r c
                                                                 | RoleInclusion(_, _) as ax -> [ax]
                                                                 | _ -> failwith "Wrong constructor in 'normalise_ELHr_terminology'!"
                                                       in
                                                       List.rev_append l'' l')
                                         []
                                         mergedConjunctionsAxioms
  in

  let normalisedAxioms2 = mergeDomainAndRangeAxioms normalisedAxioms1 in
  
  let (conj, nonConj) = extract_conjunctive_equalities normalisedAxioms2 in
  let normalisedAxioms3 = List.rev_append (break_conjunctive_cycles conj) nonConj in
  (* Sanity check *)
  List.iter (fun ax -> if not(is_normalised_ELHr_axiom ax) then
                         begin
                         print_endline("ERROR: Axiom has not been normalised correctly: " ^ (axiom_to_string ax));
                         end)
            normalisedAxioms3;
  normalisedAxioms3

(* kate: replace-tabs on; indent-width 2; *)