(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  Copyright (C) 2010-2014                                *)
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
(**********************************************************)

open Owl2;;
open RoleMapping;;
open Settings;;
open Sigma;;
open Types;;
open Utilities;;

exception CounterExampleFound of concept;;
exception UniversalCounterExampleFound of concept;;
exception ElementFound of unit;;

type topConceptName = Top | ConceptName of string
type rangeConceptNamePair = topConceptName * topConceptName

exception ImpossibleToSatify
exception Found of (rangeConceptNamePair * StringSet.t)
exception NoMatchFound of StringSet.t

let string_of_topConceptName t =
  match t with Top -> "Top"
            |  ConceptName cname -> cname

let string_of_rangeConceptNamePair pair =
  match pair with (c1, c2) -> "(" ^ (string_of_topConceptName c1) ^ ", " ^ (string_of_topConceptName c2) ^ ")"
  
module RangeConceptNamePairSet = TypeSet.Make(struct type t = rangeConceptNamePair
                                                let to_string = string_of_rangeConceptNamePair
                                              end)

let string_of_concept_option o =
  match o with None -> "None"
            |  Some c -> concept_to_string c

let string_of_simulation_pair (p1, p2) =
  "(" ^ (string_of_rangeConceptNamePair p1) ^ ", " ^ (string_of_rangeConceptNamePair p2) ^ ")"
 
module SimulationPairSet = TypeSet.Make(struct type t = rangeConceptNamePair * rangeConceptNamePair
                                              let to_string = string_of_simulation_pair
                                        end)

module ConceptSet = TypeSet.Make(struct type t = concept
                                   let to_string = concept_to_string
                                 end)
                                        
let buildAnd l =
  match l with
    [] -> Types.Top
  | [c] -> c
  | l -> And l

(*
let checkSimulation pRoot p'Root sigma postConceptsFunction ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 leftHandSideSet1
                                                    ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 leftHandSideSet2 =

  debug_newline();
  debug_endline("checkSimulation for " ^ (string_of_rangeConceptNamePair pRoot) ^ " and " ^ (string_of_rangeConceptNamePair p'Root));

  let primitiveHash = Hashtbl.create 50 in
  let is0Hash = Hashtbl.create 50 in

  let impliedConceptNamesByPair pair post_hash =
    match pair with (Top, Top)                            -> StringSet.empty
                |  (Top, ConceptName cname)               -> postConceptsFunction post_hash cname
                |  (ConceptName cname, Top)               -> postConceptsFunction post_hash cname
                |  (ConceptName cname, ConceptName dname) -> StringSet.union (postConceptsFunction post_hash cname)
                                                                             (postConceptsFunction post_hash dname)
  in

  let impliedConceptNamesByPair1 pair =
    impliedConceptNamesByPair pair post_concept_hash1
  in

  let impliedConceptNamesByPair2 pair =
    impliedConceptNamesByPair pair post_concept_hash2
  in

  let impliedConceptNames cname post_hash =
    postConceptsFunction post_hash cname
  in

  let impliedConceptNames2 cname =
    impliedConceptNames cname post_concept_hash2
  in

  let is_primitive1 cname =
    IndexTBox.is_primitive ont1_index leftHandSideSet1 cname
  in

  let is_primitive2 cname =
    IndexTBox.is_primitive ont2_index leftHandSideSet2 cname
  in

(* WARNING: FIXME  *)
  let post_role1 rname =
(*     RoleDifference.postRole (Ontology.get_RoleDifferenceInformation ont1) rname *)
    StringSet.empty
  in

  let implied_concept_names_from_domain_restriction2 rname =
    postConceptsFunction post_concept_hash2 (RoleMapping.map_role_to_domain_binding rname)
  in

  (* unused so far *)
  (*let post_role2 rname =
    RoleDifference.postRole (Ontology.get_RoleDifferenceInformation ont2) rname
  in

*)

(* WARNING: FIXME  *)
  let post_role_sigma2 rname =
    StringSet.empty
(*     RoleDifference.postRoleSigma sigma (Ontology.get_RoleDifferenceInformation ont2) rname *)
  in

  (* functions computing the non-conjunctive sets; *)
  (* for (implies A (and B_1 ... A ... B_n)) or (equivalent A (and B_1 ... A ... B_n)), and *)
  (* 'cname' == A, this function returns [B_1, ..., B_n] *)
  let non_conjunctive_implied cname ont occurrenceHash leftHandSideSet ont_index =
    let visited = ref StringSet.empty in
    let toBeVisited = ref [cname] in
    let rec non_conjunctive_implied_ acc =
      match !toBeVisited with
        [] -> acc
      | (cname::tl) -> begin
                       toBeVisited := tl;
                       visited := StringSet.add !visited cname;
                       match IndexTBox.normalised_get_conjunctive_rhs ont_index ont occurrenceHash leftHandSideSet cname with
                         None -> non_conjunctive_implied_ (StringSet.add acc cname)
                       | Some l -> begin
                                   List.iter (fun dname -> if not (StringSet.mem !visited dname) then
                                                             begin
                                                             toBeVisited := dname::!toBeVisited;
                                                             end)
                                             l;
                                   non_conjunctive_implied_ acc
                                   end
                    end
    in
    non_conjunctive_implied_ StringSet.empty
  in

  let non_conjunctive_implied2 cname =
    non_conjunctive_implied cname ont2 occurrenceHash2 leftHandSideSet2 ont2_index
  in

  let non_conjunctive_implied2_set cnameSet =
    StringSet.fold (fun newSet cname -> StringSet.union newSet (non_conjunctive_implied2 cname))
                   StringSet.empty
                   cnameSet
  in

  let updateCex cexRef c =
    match c with Types.Top -> () (* Types.Top is always present *)
              |  _ -> (match !cexRef with [Types.Top] -> cexRef := [c]
                                        | _ -> if not (List.mem c !cexRef) then
                                                 cexRef := c::(!cexRef))
  in

  let rec isO p p' =
    debug_endline ("isO " ^ (string_of_rangeConceptNamePair p) ^ ", " ^ (string_of_rangeConceptNamePair p'));
    (* hash optimisation *)
    try
      Hashtbl.find is0Hash (p,p')
    with Not_found ->
      let res =

        let findSimulationElementInT1 p' =
          let cexRef = ref [Types.Top] in
          let rec findSimulationElementInT1_ p =
            match isO p p' with
              None -> raise (ElementFound())
            | Some newCex -> begin
                             updateCex cexRef newCex;
                             let impliedConceptNamesByPair1 = impliedConceptNamesByPair1 p in
                             StringSet.iter (fun aname -> if not (is_primitive1 aname) then
                                                            begin
                                                            let rhsInTBox1 = IndexTBox.get_concept_rhs_in_terminology ont1_index ont1 occurrenceHash1 aname in
                                                            handle_normalised_right_hand_side_expression rhsInTBox1
                                                            (* Top *)
                                                            (fun _ -> failwith "Handling of Top is not implemented.")
                                                            (* cname = And(.ls.) *)
                                                            (fun ls -> ())
                                                            (* some r Top *)
                                                            (fun r1 -> findSimulationElementInT1_ (ConceptName (map_role_to_range_binding r1), Top))
                                                            (* some rname aname *)
                                                            (fun r1 aname -> findSimulationElementInT1_(ConceptName (map_role_to_range_binding r1), ConceptName aname))
                                                            end)
                                            impliedConceptNamesByPair1
                             end
          in
          try
            findSimulationElementInT1_ pRoot;
            match !cexRef with [c] -> (Some c) | _ -> (Some (And !cexRef))
          with ElementFound _ -> None
        in

        let isPrimO p p' =
          debug_endline ("   isPrimO " ^ (string_of_rangeConceptNamePair p) ^ " " ^ (string_of_rangeConceptNamePair p'));
          (* hash optimisation *)
          try
            Hashtbl.find primitiveHash (p,p')
          with Not_found ->
            let result =
              let impliedConceptNamesByPair2 = impliedConceptNamesByPair2 p' in
              let impliedConceptNamesByPair1 = impliedConceptNamesByPair1 p in
              try
                StringSet.iter (fun cname -> if Sigma.is_concept_name_contained sigma cname
                                                &&  not (StringSet.mem impliedConceptNamesByPair1 cname) then
                                                raise (CounterExampleFound (Name cname)))
                               impliedConceptNamesByPair2;
                None
              with CounterExampleFound cex -> (Some cex)
            in
              debug_endline ("......" ^ (string_of_concept_option result));
              Hashtbl.add primitiveHash (p, p') result;
              result
        in

        let impliedConceptNamesByPair1 = impliedConceptNamesByPair1 p in

        let checkRoleCondition cname =
          debug_endline ("...checkRoleCondition for " ^ cname);

          let findMatchingRoleSuccessorInTBox1 p2' rSet =
            debug_endline ("   findMatchingRoleSuccessorInTBox1 " ^ (string_of_rangeConceptNamePair p2') ^ " rSet= " ^ (StringSet.to_string rSet));
            let cexRef = ref [Types.Top] in
            if !debug then
              begin
              print_string("Implied concept names in TBox1: "); StringSet.print_set impliedConceptNamesByPair1; print_newline();
              end;
            let found = StringSet.exists (fun b ->  debug_endline("analysing in T1" ^ b);
                                                    if not (is_primitive1 b) then
                                                      begin
                                                      let rhsInTBox1 = IndexTBox.get_concept_rhs_in_terminology ont1_index ont1 occurrenceHash1 b in
                                                      let candidate =  handle_normalised_right_hand_side_expression rhsInTBox1
                                                                       (* Top *)
                                                                       (fun _ -> failwith "Handling of Top is not implemented.")
                                                                       (* cname = And(.ls.) *)
                                                                       (fun ls -> None)
                                                                       (* some r Top *)
                                                                       (fun r1 -> Some(r1, (ConceptName (map_role_to_range_binding r1), Top)))
                                                                       (* some rname aname *)
                                                                       (fun r1 aname -> Some (r1, (ConceptName (map_role_to_range_binding r1), ConceptName aname)))
                                                      in
                                                      match candidate with
                                                        None -> false
                                                      | Some(r1, candidateP) -> begin
                                                                                if (StringSet.subset rSet (post_role1 r1)) then
                                                                                  begin
                                                                                  match isO candidateP p2' with
                                                                                    None -> true
                                                                                  | Some cex -> begin
                                                                                                  if !debug then
                                                                                                    begin
                                                                                                    print_string "CEX found "; print_concept cex; print_newline();
                                                                                                    end;
                                                                                                  updateCex cexRef cex;
                                                                                                  false
                                                                                                end
                                                                                  end
                                                                                else
                                                                                  false
                                                                                end
                                                      end
                                                    else
                                                      false)
                                         impliedConceptNamesByPair1
            in
            if not found then
              begin
              if StringSet.cardinal rSet == 1 then
                raise (CounterExampleFound (Exists((StringSet.choose rSet), (match !cexRef with [c] -> c | _ -> (And !cexRef)))))
              else
                raise (CounterExampleFound (ExistsRoleConjunction(StringSet.to_list rSet, (match !cexRef with [c] -> c | _ -> (And !cexRef)))))
              end
          in
          (* if 'cname' is primitive in T2, then it cannot imply concepts of the form (some r C)) *)
          (* as T2 is a terminology  *)
          if not (is_primitive2 cname) then
            begin
            let rhsInTBox2 = IndexTBox.get_concept_rhs_in_terminology ont2_index ont2 occurrenceHash2 cname in
            let candidate = handle_normalised_right_hand_side_expression rhsInTBox2
                            (* Top *)
                            (fun _ -> failwith "Handling of Top is not implemented.")
                            (* cname = And(.ls.) *)
                            (fun ls -> None)
                            (* some r Top *)
                            (fun r1' -> Some (post_role_sigma2 r1', (ConceptName (map_role_to_range_binding r1'), Top)))
                            (* some rname aname *)
                            (fun r1' aname -> Some (post_role_sigma2 r1', (ConceptName (map_role_to_range_binding r1'), ConceptName aname)))
            in
            match candidate with
              None -> ()
            | Some(postRoleSigmaT2, p2') -> if is_conjunctive_query_case() then
                                              begin
                                              if StringSet.is_empty postRoleSigmaT2 then
                                                begin
                                                debug_endline("no sigma successor for " ^ (string_of_rangeConceptNamePair p2'));
                                                match findSimulationElementInT1 p2' with
                                                  None -> ()
                                                | Some cex -> (* for counter-examples that start with (some :universal C) *)
                                                              (* we don't have to go back through the role chain *)
                                                              raise (UniversalCounterExampleFound (ExistsUniversalRole cex))
                                                end
                                              else
                                                findMatchingRoleSuccessorInTBox1 p2' postRoleSigmaT2
                                              end
                                            else
                                              StringSet.iter (fun r -> findMatchingRoleSuccessorInTBox1 p2' (StringSet.singleton r))
                                                             postRoleSigmaT2
            end
        in

        let findAllExists2 cnameSet =
          let rec findAllExists2_ cnameSet acc =
            let newCNames = StringSet.fold (fun newSet cname -> let nonConjunctiveImplied = non_conjunctive_implied2 cname in
                                                                let domainRestrictions = StringSet.fold (fun newSet dname -> match IndexTBox.get_role_successor_in_terminology ont2_index ont2 occurrenceHash2 dname with
                                                                                                                                None -> newSet
                                                                                                                              | Some r -> StringSet.union newSet
                                                                                                                                                          (non_conjunctive_implied2_set (implied_concept_names_from_domain_restriction2 r)))
                                                                                                        StringSet.empty
                                                                                                        nonConjunctiveImplied
                                                                in
                                                                StringSet.union newSet (StringSet.union (StringSet.difference nonConjunctiveImplied acc)
                                                                                       (StringSet.difference domainRestrictions acc)))
                                           StringSet.empty
                                           cnameSet
            in
            if not (StringSet.is_empty newCNames) then
              findAllExists2_ newCNames (StringSet.union acc newCNames)
            else
              acc
          in
          findAllExists2_ cnameSet StringSet.empty
        in
        match isPrimO p p' with
          Some cex -> Some cex
          (* for performance reasons we don't want to go through all the concept names implied by *)
          (* the 'proper' concept name of the pair p' (i.e. its second component) as this would   *)
          (* also include concept names A defined as (equivalent A (some r B)) in the terminology *)
          (* T2 whose role successor 'r' has already been handled before, i.e. the role successor *)
          (* 'r' would be handled twice. *)
        | None -> let cnamesWithExist = match p' with
                                          (Top, Top) -> StringSet.empty
                                        | (Top, ConceptName cname) -> findAllExists2 (StringSet.singleton cname)
                                        | (ConceptName rangeConceptName, Top) -> findAllExists2 (impliedConceptNames2 rangeConceptName)
                                        | (ConceptName rangeConceptName, ConceptName cname) -> findAllExists2 (StringSet.union (impliedConceptNames2 rangeConceptName) (StringSet.singleton cname))
                  in
                  try
                    StringSet.iter checkRoleCondition cnamesWithExist;
                    None
                  with CounterExampleFound cex -> (Some cex)
      in

      Hashtbl.add is0Hash (p, p') res;
      res
  in
  try
    isO pRoot p'Root
  with
    UniversalCounterExampleFound cex -> (Some cex)



*)


























































let checkCEx sigma nameMapping outputChannel ont1 ont2 =

  let roleDifference1 = Terminology.get_role_difference_information ont1 in
  let roleDifference2 = Terminology.get_role_difference_information ont2 in

  let preRoleSigmaT1 r =
    RoleDifference.preRoleSigma sigma roleDifference1 r
  in

  let preRoleT2 r =
    RoleDifference.preRole roleDifference2 r
  in

  let entails1 cname dname =
    Terminology.entails ont1 cname dname
  in

  let entails2 cname dname =
    Terminology.entails ont2 cname dname
  in

  let impliedConceptNamesByPair ont pair =
    match pair with (Top, Top)                            -> StringSet.empty
                |  (Top, ConceptName cname)               -> Terminology.entailed_concept_names ont cname
                |  (ConceptName cname, Top)               -> Terminology.entailed_concept_names ont cname
                |  (ConceptName cname, ConceptName dname) -> StringSet.union (Terminology.entailed_concept_names ont dname)
                                                                             (Terminology.entailed_concept_names ont cname)
  in

  let impliedConceptNamesByPair1 pair =
    impliedConceptNamesByPair ont1 pair
  in

  let impliedConceptNamesByPair2 pair =
    impliedConceptNamesByPair ont2 pair
  in

  let post_role1 rname =
    RoleDifference.postRole roleDifference1 rname
  in
  
  let post_role2 rname =
    RoleDifference.postRole roleDifference2 rname
  in

  let post_role_sigma1 rname =
    RoleDifference.postRoleSigma sigma roleDifference1 rname
  in
  
  let post_role_sigma2 rname =
    RoleDifference.postRoleSigma sigma roleDifference2 rname
  in


  let cexHash = Hashtbl.create 50 in

  let add_cex p cex =
(* print_endline ("adding cex for " ^ (string_of_simulation_pair p)); *)
    if not (Hashtbl.mem cexHash p) then
      Hashtbl.add cexHash p cex
  in

  let add_to_simulation simulationSet p =
debug_endline ("ADDING " ^ (string_of_simulation_pair p));
    simulationSet := SimulationPairSet.add !simulationSet p
  in
  
  let remove_from_simulation simulationSet p =
    simulationSet := SimulationPairSet.remove !simulationSet p
  in
  
  let is_contained_in_simulation simulationSet p =
    SimulationPairSet.mem !simulationSet p
  in

  let nonSimulationSet = ref SimulationPairSet.empty in
  
  let add_to_non_simulation nonSimulationSet p =
debug_endline ("ADDING NON" ^ (string_of_simulation_pair p));
    nonSimulationSet := SimulationPairSet.add !nonSimulationSet p
  in

  let is_contained_in_non_simulation nonSimulationSet p =
    SimulationPairSet.mem !nonSimulationSet p
  in

  let add_to_queue_set queueSet p =
    queueSet := SimulationPairSet.add !queueSet p
  in
  
  let remove_from_queue_set queueSet p =
    queueSet := SimulationPairSet.remove !queueSet p
  in
  
  let is_contained_in_queue_set queueSet p =
    SimulationPairSet.mem !queueSet p
  in

 
  let find_entailed_sigma_concept_names_by_pair ont f sigma pair =
    match pair with (Top, Top)                            -> None
                |  (Top, ConceptName cname)               -> Terminology.find_entailed_sigma_concept_names f ont sigma cname
                |  (ConceptName cname, Top)               -> Terminology.find_entailed_sigma_concept_names f ont sigma cname
                |  (ConceptName cname, ConceptName dname) -> match Terminology.find_entailed_sigma_concept_names f ont sigma dname with
                                                               None -> Terminology.find_entailed_sigma_concept_names f ont sigma cname
                                                             | (Some _) as res -> res
  in

  let find_entailed_sigma_concept_names_by_pair1 f sigma pair =
    find_entailed_sigma_concept_names_by_pair ont1 f sigma pair
  in

  let check_entailment_for_pair ont pair toBeChecked =
    match pair with (Top, Top)                            -> false
                |  (Top, ConceptName cname)               -> Terminology.entails ont cname toBeChecked
                |  (ConceptName cname, Top)               -> Terminology.entails ont cname toBeChecked
                |  (ConceptName cname, ConceptName dname) -> Terminology.entails ont dname toBeChecked
                                                             || Terminology.entails ont cname toBeChecked
  in
  
  let check_entailment_for_pair2 pair toBeChecked =
    check_entailment_for_pair ont2 pair toBeChecked
  in

  let check_local_conditions (p1, p2) =
(* print_endline("checking local conditions for " ^ (string_of_simulation_triple (c1, c2, rOption))); *)
   (match find_entailed_sigma_concept_names_by_pair1 (fun cname -> if not (check_entailment_for_pair2 p2 cname) then
                                                                     Some cname
                                                                   else
                                                                     None)
                                                      sigma p1
    with None -> true
    | Some cname -> add_cex (p1, p2) (Name cname); false)
  in
  
  
  let fulfillsRoleConditions (p1, p2) rSet =
(*     let (range1, c1) = p1 in *)
    let (range2, _) = p2 in
    let r2 = match range2 with
               ConceptName cname -> RoleMapping.map_range_binding_to_role cname
             | _ -> failwith ("Invalid argument given in 'fulfillsRoleConditions'! " ^ (string_of_simulation_pair (p1, p2)))
    in
    let postRole2 = post_role2 r2 in
    if not (StringSet.subset rSet postRole2) then
      begin
(*      let cex = if StringSet.cardinal rSet == 1 then
                  Exists((StringSet.choose rSet), Types.Top)
                else
                  ExistsRoleConjunction(StringSet.to_list rSet, Types.Top)
      in
      add_cex (p1, p2) cex;
      add_to_non_simulation nonSimulationSet (p1, p2);*)
      false
      end
    else
      true
  in

  let relevantHash = Hashtbl.create 50 in

(*  let findRelevantRoleSuccessors ont p =
(*print_endline("");
print_endline("");
print_endline("");
print_endline("");
print_endline("new");*)
    let toReturn = ref RangeConceptNamePairSet.empty in

    let visited = ref ConceptSet.empty in
    let toBeVisited = ref [] in

    let schedule_visit c =
(* print_endline("scheduling to be added " ^ (concept_to_string c)); *)
      toBeVisited := c::(!toBeVisited);
    in


    let _ = match p with
              (Top, Top)                             -> ()
            | (Top, ConceptName dname)               -> schedule_visit (Name dname)
            | (ConceptName cname, Top)               -> List.iter (fun c -> schedule_visit c)
                                                                  (Terminology.get_range_restrictions ont (RoleMapping.map_range_binding_to_role cname))
            | (ConceptName cname, ConceptName dname) -> begin
                                                        schedule_visit (Name dname);
                                                        List.iter (fun c -> schedule_visit c)
                                                                  (Terminology.get_range_restrictions ont (RoleMapping.map_range_binding_to_role cname))
                                                        end
    in

    while not (Utilities.list_is_empty !toBeVisited) do
      let c = List.hd !toBeVisited in
      toBeVisited := List.tl !toBeVisited;
(* print_endline("visiting " ^ (concept_to_string c)); *)
      if not (ConceptSet.mem !visited c) then
        begin
        visited := ConceptSet.add !visited c;
        
        match c with
          Name cname -> begin
                        if not (Terminology.is_primitive ont cname) then
                          schedule_visit (Terminology.get_concept_rhs ont cname)
                        end
        | Exists(r, Types.Top) -> begin
                                  toReturn := RangeConceptNamePairSet.add !toReturn (ConceptName (map_role_to_range_binding r), Top);
                                  List.iter (fun c -> schedule_visit c)
                                            (Terminology.get_domain_restrictions ont r)
                                  end
        | Exists(r, Name(dname)) -> begin
                                    toReturn := RangeConceptNamePairSet.add !toReturn (ConceptName (map_role_to_range_binding r), ConceptName dname);
                                    List.iter (fun c -> schedule_visit c)
                                              (Terminology.get_domain_restrictions ont r)
                                    end
        | And l -> List.iter (fun d -> schedule_visit d)
                             l
        | _ -> failwith ("Invalid case encountered in 'findRelevantRoleSuccessors'! " ^ (concept_to_string c))
        end
    done;
(*     print_endline("return size "); *)
    !toReturn
(* true *)
  in*)

let findRelevantRoleSuccessorsForConceptName ont postRole (c : string) =
    try
      Hashtbl.find relevantHash (ont, c)
    with Not_found ->
    begin

(*print_endline("");
print_endline("");
print_endline("");
print_endline("");
print_endline("new");*)
(*    let toReturn = ref RangeConceptNamePairSet.empty in

    let visited = ref ConceptSet.empty in
    let toBeVisited = ref [] in

    let schedule_visit c =
(* print_endline("scheduling to be added " ^ (concept_to_string c)); *)
      toBeVisited := c::(!toBeVisited);
    in


    schedule_visit (Name c);

    while not (Utilities.list_is_empty !toBeVisited) do
      let c = List.hd !toBeVisited in
      toBeVisited := List.tl !toBeVisited;
(* print_endline("visiting " ^ (concept_to_string c)); *)
      if not (ConceptSet.mem !visited c) then
        begin
        visited := ConceptSet.add !visited c;
        
        match c with
          Name cname -> begin
                        if not (Terminology.is_primitive ont cname) then
                          schedule_visit (Terminology.get_concept_rhs ont cname)
                        end
        | Exists(r, Types.Top) -> begin
                                  toReturn := RangeConceptNamePairSet.add !toReturn (ConceptName (map_role_to_range_binding r), Top);
                                  List.iter (fun c -> schedule_visit c)
                                            (Terminology.get_domain_restrictions ont r)
                                  end
        | Exists(r, Name(dname)) -> begin
                                    toReturn := RangeConceptNamePairSet.add !toReturn (ConceptName (map_role_to_range_binding r), ConceptName dname);
                                    List.iter (fun c -> schedule_visit c)
                                              (Terminology.get_domain_restrictions ont r)
                                    end
        | And l -> List.iter (fun d -> schedule_visit d)
                             l
        | _ -> failwith ("Invalid case encountered in 'findRelevantRoleSuccessors'! " ^ (concept_to_string c))
        end
    done;*)
    let getRelevantDomainSuccessors r oldToBeVisited =
      StringSet.fold (fun l s -> match Terminology.get_domain_restrictions ont s with
                                   None -> l
                                 | Some _ -> (map_role_to_domain_binding s)::l)
                     oldToBeVisited
                     (postRole r)
    in
    
    let rec handle l (toReturn : RangeConceptNamePairSet.t) visited =
      match l with
        [] -> toReturn
      | (cname::toBeVisited) ->
        if StringSet.mem visited cname then
          handle toBeVisited toReturn visited
        else
          begin
          let newVisited = StringSet.add visited cname in

          if not (Terminology.is_primitive ont cname) then
(*         begin *)
(*           begin *)
          match Terminology.get_concept_rhs ont cname with
            Name cname -> handle (cname::toBeVisited) toReturn newVisited
          | Exists(r, Types.Top) -> begin
                                    handle (getRelevantDomainSuccessors r toBeVisited)
                                           (RangeConceptNamePairSet.add toReturn (ConceptName (map_role_to_range_binding r), Top)) newVisited
                                    end
          | Exists(r, Name(dname)) -> begin
                                      handle (getRelevantDomainSuccessors r toBeVisited)
                                             (RangeConceptNamePairSet.add toReturn (ConceptName (map_role_to_range_binding r), ConceptName dname)) newVisited
                                      end
          | And l -> let newToBeVisited = List.fold_left (fun ls d -> match d with
                                                                        Name dname -> dname::ls
                                                                      | _ -> failwith ("Invalid conjunctive case encountered in 'findRelevantRoleSuccessors'! " ^ (concept_to_string d)))
                                                         toBeVisited
                                                         l
                     in
                     handle newToBeVisited toReturn  newVisited
          | _ -> failwith ("Invalid case encountered in 'findRelevantRoleSuccessors'! " ^ c)
(*           end; *)
          else
            begin
            handle toBeVisited toReturn newVisited
            end;
(*         end *)
          end
    in
    
    let toReturn = handle [c] RangeConceptNamePairSet.empty StringSet.empty in
    
(*     print_endline("return size "); *)
Hashtbl.add relevantHash (ont, c) toReturn;
    toReturn
(* true *)
    end
  in



  let relevantRoleSuccessorHash = Hashtbl.create 50 in

  let findRelevantRoleSuccessors ont postRole p =
    try
      Hashtbl.find relevantRoleSuccessorHash (ont, p)
    with Not_found ->
     begin
     let r = match p with
              (Top, Top)                             -> RangeConceptNamePairSet.empty
            | (Top, ConceptName dname)               -> findRelevantRoleSuccessorsForConceptName ont postRole dname
            | (ConceptName cname, Top)               -> findRelevantRoleSuccessorsForConceptName ont postRole cname
            | (ConceptName cname, ConceptName dname) -> RangeConceptNamePairSet.union (findRelevantRoleSuccessorsForConceptName ont postRole cname)
                                                                                      (findRelevantRoleSuccessorsForConceptName ont postRole dname)
     in
     Hashtbl.add relevantRoleSuccessorHash (ont, p) r;
     r
     end
  in

  let findRelevantRoleSuccessors1 p =
    findRelevantRoleSuccessors ont1 post_role1 p
  in

  let findRelevantRoleSuccessors2 p =
    findRelevantRoleSuccessors ont2 post_role2 p
  in
  
  let nextSuccessors reachableNodes2 (p1, p2) previousOptionRef failingPairs =

    let findMatchingRoleSuccessorPair directlyEntailed2 p1 rSet =
  (*     print_endline("here1"); *)
      try
        RangeConceptNamePairSet.iter (fun p2 ->  if not (is_contained_in_non_simulation nonSimulationSet (p1, p2))
                                                    && check_local_conditions(p1, p2)
                                                    && fulfillsRoleConditions (p1, p2) rSet
                                                then
                                                  raise (Found (p2, rSet))
    (*               && (not (Terminology.is_defined_as_existential ont1 cname1) *)
        )
                                    directlyEntailed2;
        raise (NoMatchFound rSet)
      with Found(p2, depInfo) ->
        begin
        ((p1, p2), depInfo)
        end
    in

    let findMatchingUniversalPair p1 =
      try
        RangeConceptNamePairSet.iter (fun p2 ->  if not (is_contained_in_non_simulation nonSimulationSet (p1, p2))
                                                    && check_local_conditions(p1, p2)
                                                then
                                                  raise (Found (p2, StringSet.empty)))
                                    reachableNodes2;
        raise (NoMatchFound StringSet.empty)
      with Found(p2, _) ->
        begin
        ((p1, p2), StringSet.empty)
        end
    in

    let constructCounterExample p1' rSet choices =
      let add_to_list_uniquely l e =
        if not (Utilities.list_contains l e) then
          e::l
        else
          l
      in
    
      if !constructCounterExamples then
        begin
        if StringSet.is_empty rSet then
          begin
          let l = RangeConceptNamePairSet.fold (fun newL p2' -> let cex = Hashtbl.find cexHash (p1', p2') in
                                                                add_to_list_uniquely newL cex)
                                               []
                                               choices
          in
          add_cex (p1, p2) (ExistsUniversalRole(buildAnd l))
          end
        else
          begin
          let l = RangeConceptNamePairSet.fold (fun newL p2' -> (*print_endline("cex2a");*)
                                                                if not (fulfillsRoleConditions (p1',p2') rSet) then
                                                                  if StringSet.cardinal rSet == 1 then
                                                                    add_to_list_uniquely newL (Exists((StringSet.choose rSet), Types.Top))
                                                                  else
                                                                    add_to_list_uniquely newL (ExistsRoleConjunction(StringSet.to_list rSet, Types.Top))
                                                                else
                                                                  begin
                                                                  let cex = Hashtbl.find cexHash (p1', p2') in
                                                                  if StringSet.cardinal rSet == 1 then
                                                                    add_to_list_uniquely newL (Exists((StringSet.choose rSet), cex))
                                                                  else
                                                                    add_to_list_uniquely newL (ExistsRoleConjunction(StringSet.to_list rSet, cex))
                                                                end
                                                    (*print_endline("cex2");*)
                                                                )
                                              []
                                              choices
          in
          (* the case where the local conditions are not fulfilled has been handled in  *)
          (* the main simulation computation procedure, so we concentrate here on missing  *)
          (* role successors *)
          if Utilities.list_is_empty l then
            begin
            if StringSet.cardinal rSet == 1 then
              add_cex (p1, p2) (Exists((StringSet.choose rSet), Types.Top))
            else
              add_cex (p1, p2) (ExistsRoleConjunction(StringSet.to_list rSet, Types.Top))
            end
          else
            add_cex (p1, p2) (buildAnd l)
          end
        end;    
    in
    
    match !previousOptionRef with
      None -> begin
              let directlyEntailedPairs1 = findRelevantRoleSuccessors1 p1 in
(*print_endline("directlyEntailedPairs1 size " ^ (string_of_int (RangeConceptNamePairSet.cardinal directlyEntailedPairs1)) ^ " for " ^ (string_of_rangeConceptNamePair p1));
print_endline("directlyEntailedPairs1");
RangeConceptNamePairSet.print_set directlyEntailedPairs1;
print_endline("");*)
              let directlyEntailedPairs2 = findRelevantRoleSuccessors2 p2 in
(* print_endline("directlyEntailedPairs2 size " ^ (string_of_int (RangeConceptNamePairSet.cardinal directlyEntailedPairs2))); *)
              try
                let possibilities = RangeConceptNamePairSet.fold
                                       (fun l p1' -> let r1 = match p1' with
                                                                (ConceptName rangeConceptName, _) -> RoleMapping.map_range_binding_to_role rangeConceptName
                                                              | _ -> failwith "Invalid argument given in 'nextSuccessors'! " ^ (string_of_rangeConceptNamePair p1')
                                                     in
                                                     try
                                                       if is_conjunctive_query_case() then
                                                         begin
                                                         let postRole1 = post_role_sigma1 r1 in
                                                         if StringSet.is_empty postRole1 then
                                                           (findMatchingUniversalPair p1')::l
                                                         else
                                                           (findMatchingRoleSuccessorPair directlyEntailedPairs2 p1' postRole1)::l
                                                         end
                                                       else
                                                         begin
                                                         StringSet.fold (fun l' s -> (findMatchingRoleSuccessorPair directlyEntailedPairs2 p1' (StringSet.singleton s))::l')
                                                                        l
                                                                        (post_role_sigma1 r1)
                                                         end
                                                     with NoMatchFound(rSet) ->
                                                       begin
                                                       constructCounterExample p1' rSet directlyEntailedPairs2;
                                                       raise ImpossibleToSatify
                                                       end)
                                       []
                                       directlyEntailedPairs1 in
                previousOptionRef := Some (directlyEntailedPairs1, directlyEntailedPairs2);
                Some (possibilities)
              with ImpossibleToSatify ->
                None
              end
    | Some (_, directlyEntailedPairs2) -> begin
                                          try
                                            let l = List.map (fun ((p1', _), rSet) -> try
                                                                                        if StringSet.is_empty rSet then
                                                                                          findMatchingUniversalPair p1'
                                                                                        else
                                                                                          findMatchingRoleSuccessorPair directlyEntailedPairs2 p1' rSet
                                                                                      with NoMatchFound(rSet) ->
                                                                                        begin
                                                                                        constructCounterExample p1' rSet directlyEntailedPairs2;
                                                                                        raise ImpossibleToSatify
                                                                                        end)
                                                               failingPairs
                                            in
                                            Some l
                                          with ImpossibleToSatify ->
                                            None
                                          end
  in

  let forwardReachableHash = Hashtbl.create 50 in
  
  let findForwardReachableNodes ont postRole p =
(*     try *)
(*       Hashtbl.find forwardReachableHash (ont, p) *)
(*     with Not_found -> *)
      begin
(*    let visited = ref RangeConceptNamePairSet.empty in
    let toBeVisited = ref (RangeConceptNamePairSet.singleton p) in
    
    while not (RangeConceptNamePairSet.is_empty !toBeVisited) do
      let p' = RangeConceptNamePairSet.choose !toBeVisited in
      toBeVisited := RangeConceptNamePairSet.remove !toBeVisited p';

      visited := RangeConceptNamePairSet.add !visited p';

      toBeVisited := RangeConceptNamePairSet.union !toBeVisited (findDirectlyEntailedPairs ont p');
    done;*)
    
    let rec handle toBeVisited visited =
      if RangeConceptNamePairSet.is_empty toBeVisited then
        visited
      else
        begin
        let p' = RangeConceptNamePairSet.choose toBeVisited in
        let newToBeVisited = RangeConceptNamePairSet.remove toBeVisited p' in

        if not (RangeConceptNamePairSet.mem visited p') then
          begin
          let newVisited = RangeConceptNamePairSet.add visited p' in
          handle (RangeConceptNamePairSet.union newToBeVisited (findRelevantRoleSuccessors ont postRole p'))
                 newVisited
          end
        else
          handle newToBeVisited visited
        end
    in
    handle (RangeConceptNamePairSet.singleton p) RangeConceptNamePairSet.empty
(*       Hashtbl.add forwardReachableHash (ont, p) !visited; *)
(*    !visited *)
     end
  in

  let findForwardReachableNodes1 p =
    findForwardReachableNodes ont1 post_role1 p
  in
  
  let findForwardReachableNodes2 p =
    findForwardReachableNodes ont2 post_role2 p
  in

  let simulationSet = ref SimulationPairSet.empty in
  let queueSet = ref SimulationPairSet.empty in

  let nrCounterExamples = ref 0 in
  let constructedCounterExamples = ref [] in
  let counterExampleWitnesses = ref StringSet.empty
  in

  let (concepts1, roles1) = Terminology.get_signature ont1 in
  let (concepts2, roles2) = Terminology.get_signature ont2 in

(*  print_endline("Computing relevant successors1");
  StringSet.iter (fun cname -> (*print_endline("cname " ^ cname); *)ignore (findRelevantRoleSuccessorsForConceptName ont1 cname))
                 concepts1;*)

execute_measure_time ("Computing relevant successors1...")
                     (fun _ ->   StringSet.iter (fun cname -> (*print_endline("cname " ^ cname);*) ignore (findRelevantRoleSuccessorsForConceptName ont1 post_role1 cname))
                                                concepts1);
(* exit(0);                  *)
execute_measure_time ("Computing relevant successors2...")
(*   StringSet.iter (fun cname -> ignore (findRelevantRoleSuccessorsForConceptName ont2 cname)) *)
                     (fun _ ->   StringSet.iter (fun cname -> (*print_endline("cname " ^ cname); *)ignore (findRelevantRoleSuccessorsForConceptName ont2 post_role2 cname))
                                                concepts2);
(*                  concepts2; *)

let toBeProcessedList = ref [] in
let dependencyHash = Hashtbl.create 50 in
let successorInformationHash = Hashtbl.create 50 in
let failingHash = Hashtbl.create 50 in
  
  let compute_simulation (p1, p2) reachableNodes2 =
(*     nonSimulationSet := SimulationPairSet.empty; *)
(*     simulationSet := SimulationPairSet.empty;  *) (* clearing this will make it slower *) 
    queueSet := SimulationPairSet.empty;
    SimulationComputation.compute_simulation toBeProcessedList dependencyHash successorInformationHash failingHash
                                             (is_contained_in_non_simulation nonSimulationSet)
                                             (add_to_non_simulation nonSimulationSet)
                                             (is_contained_in_queue_set queueSet)
                                             (add_to_queue_set queueSet)
                                             (remove_from_queue_set queueSet)
                                             (is_contained_in_simulation simulationSet)
                                             (add_to_simulation simulationSet)
                                             (remove_from_simulation simulationSet)
                                             check_local_conditions
                                             (nextSuccessors reachableNodes2)
                                             string_of_simulation_pair
                                             (p1, p2)
  in
  
  
  let difference_witness_found c =
    incr nrCounterExamples;
    counterExampleWitnesses := StringSet.add !counterExampleWitnesses c;
    if !constructCounterExamples then
      begin
      let cex = Hashtbl.find cexHash ((Top, ConceptName c), (Top, ConceptName c)) in
      output_axiom_with_mapping outputChannel (ConceptInclusion(Name c, cex)) nameMapping; output_string outputChannel "\n";
      Statistics.collectForLHSCounterExample c cex;
      constructedCounterExamples := (Name c, cex)::(!constructedCounterExamples)
      end
    else
      print_endline ("LHS counter-example found for " ^ c ^ ".")  
  in
  (* add CQ case here... *)
let i = ref (-1) in  
  StringSet.iter (fun c -> incr i;
(*   print_endline((string_of_int !i) ^ ": " ^ c); *)
                           let reachableNodes2 = if is_conjunctive_query_case() then
                                                   findForwardReachableNodes2 (Top, ConceptName c)
                                                 else
                                                   RangeConceptNamePairSet.empty
                           in
                           compute_simulation ((Top, ConceptName c), (Top, ConceptName c)) reachableNodes2;
                           let contained = is_contained_in_simulation simulationSet ((Top, ConceptName c), (Top, ConceptName c)) in
                           if not (contained) then
                             begin
                             difference_witness_found c
                             end
(*                           else
                             begin
                             if is_conjunctive_query_case() then
                               begin
                               let forwardReachbleNodes1 = findForwardReachableNodes1 (Top, ConceptName c) in
                               let forwardReachbleNodes2 = findForwardReachableNodes2 (Top, ConceptName c) in
(*print_endline ("size1: " ^ (string_of_int (RangeConceptNamePairSet.cardinal forwardReachbleNodes1)));
print_endline ("size2: " ^ (string_of_int (RangeConceptNamePairSet.cardinal forwardReachbleNodes2)));*)
                               ignore (RangeConceptNamePairSet.for_all
                                             (fun p1 -> let found = RangeConceptNamePairSet.exists (fun p2 -> compute_simulation (p1, p2);
                                                                                                             is_contained_in_simulation simulationSet (p1, p2))
                                                                                                   forwardReachbleNodes2
                                                       in
                                                       if not found then
                                                         begin
                                                         let l = RangeConceptNamePairSet.fold (fun newL p2' -> let cex = Hashtbl.find cexHash (p1, p2') in
   (*                                                                                                              if not (Utilities.list_contains newL cex) then *)
                                                                                                                 cex::newL
                                                                                                              (*else
                                                                                                                newL*))
                                                                                               []
                                                                                               forwardReachbleNodes2
                                                         in
                                                         add_cex ((Top, ConceptName c), (Top, ConceptName c)) (ExistsUniversalRole(buildAnd l));
                                                         difference_witness_found c;
                                                         false
                                                         end
                                                       else
                                                         true)
                                             forwardReachbleNodes1)
                               end
                             end*)
                             )
                 (Sigma.get_concept_names sigma);
  print_endline("Number of differences with atomic LHS: " ^ (string_of_int !nrCounterExamples));
  
  
  let nrDomainCounterExamples = ref 0 in
  let constructedDomainCounterExamples = ref [] in
  let domainCounterExampleWitnessesRef = ref StringSet.empty
  in
  StringSet.iter (fun r -> let p = (Top, ConceptName (map_role_to_domain_binding r)) in
                           let reachableNodes2 = if is_conjunctive_query_case() then
                                                   findForwardReachableNodes2 p
                                                 else
                                                   RangeConceptNamePairSet.empty
                           in
                           compute_simulation (p, p) reachableNodes2;
                           let contained = is_contained_in_simulation simulationSet (p, p) in
                           if not (contained) then
                             begin
                             incr nrDomainCounterExamples;
                             domainCounterExampleWitnessesRef := StringSet.add !domainCounterExampleWitnessesRef r;
                             if !constructCounterExamples then
                               begin
                               let cex = Hashtbl.find cexHash (p, p) in
                               output_axiom_with_mapping outputChannel (ConceptInclusion(Domain r, cex)) nameMapping; output_string outputChannel "\n";
                               Statistics.collectForDomainCounterExample r cex;
                               constructedDomainCounterExamples := (Domain r, cex)::(!constructedDomainCounterExamples)
                               end
                             else
                               print_endline ("Role domain counter-example found for " ^ r ^ ".")
                             end)
                 (Sigma.get_role_names sigma);

  print_endline("Number of differences w.r.t. the domain of roles: " ^ (string_of_int !nrDomainCounterExamples));

  let nrRangeCounterExamples = ref 0 in
  let constructedRangeCounterExamples = ref [] in
  let rangeCounterExampleWitnessesRef = ref StringSet.empty
  in
  StringSet.iter (fun r -> let p = (ConceptName (map_role_to_range_binding r), Top) in
                           let reachableNodes2 = if is_conjunctive_query_case() then
                                                   findForwardReachableNodes2 p
                                                 else
                                                   RangeConceptNamePairSet.empty
                           in
                           compute_simulation (p, p) reachableNodes2;
                           let contained = is_contained_in_simulation simulationSet (p, p) in
                           if not (contained) then
                             begin
                             incr nrRangeCounterExamples;
                             rangeCounterExampleWitnessesRef := StringSet.add !rangeCounterExampleWitnessesRef r;
                             if !constructCounterExamples then
                               begin
                               let cex = Hashtbl.find cexHash (p, p) in
                               output_axiom_with_mapping outputChannel (ConceptInclusion(Range r, cex)) nameMapping; output_string outputChannel "\n";
                               Statistics.collectForRangeCounterExample r cex;
                               constructedRangeCounterExamples := (Range r, cex)::(!constructedRangeCounterExamples)
                               end
                             else
                               print_endline ("Role range counter-example found for " ^ r ^ ".")
                             end)
                 (Sigma.get_role_names sigma);

  print_endline("Number of differences w.r.t. the range of roles: " ^ (string_of_int !nrRangeCounterExamples));

  ((!constructedCounterExamples, !counterExampleWitnesses, !nrCounterExamples),
   (!constructedDomainCounterExamples, !domainCounterExampleWitnessesRef, !nrDomainCounterExamples),
   (!constructedRangeCounterExamples, !rangeCounterExampleWitnessesRef, !nrRangeCounterExamples))
  
(*

let checkRoleDomainCEx sigma nameMapping outputChannel ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 leftHandSideSet1
                                                       ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 leftHandSideSet2 =
  let nrCounterExamples = ref 0 in
  let constructedCounterExamples = ref [] in
  let counterExampleWitnessesRef = ref StringSet.empty
  in
  StringSet.iter (fun r -> let res = checkSimulation (Top, ConceptName (map_role_to_domain_binding r)) (Top, ConceptName (map_role_to_domain_binding r))
                                                     sigma Ontology.post_proper_concepts_with_domain_bindings
                                                     ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 leftHandSideSet1
                                                     ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 leftHandSideSet2
                            in
                            match res with
                              None -> ()
                            | Some cex -> incr nrCounterExamples;
                                          counterExampleWitnessesRef := StringSet.add !counterExampleWitnessesRef r;
                                          if !constructCounterExamples then
                                            begin
                                            output_axiom_with_mapping outputChannel (ConceptInclusion(Domain r, cex)) nameMapping; output_string outputChannel "\n";
                                            Statistics.collectForDomainCounterExample r cex;
                                            constructedCounterExamples := (Domain r, cex)::(!constructedCounterExamples)
                                            end
                                          else
                                            print_endline ("Role domain counter-example found for " ^ r ^ "."))
                 (Sigma.get_role_names sigma);
  print_endline("Number of differences w.r.t. the domain of roles: " ^ (string_of_int !nrCounterExamples));
  (!constructedCounterExamples, !counterExampleWitnessesRef, !nrCounterExamples)

let checkRoleRangeCEx sigma nameMapping outputChannel ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 leftHandSideSet1
                                                      ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 leftHandSideSet2 =
  let nrCounterExamples = ref 0 in
  let constructedCounterExamples = ref [] in
  let counterExampleWitnessesRef = ref StringSet.empty
  in
  StringSet.iter (fun r -> let res = checkSimulation (ConceptName (map_role_to_range_binding r), Top) (ConceptName (map_role_to_range_binding r), Top)
                                                     sigma Ontology.post_proper_concepts_with_domain_bindings
                                                     ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 leftHandSideSet1
                                                     ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 leftHandSideSet2
                            in
                            match res with
                              None -> ()
                            | Some cex -> incr nrCounterExamples;
                                          counterExampleWitnessesRef := StringSet.add !counterExampleWitnessesRef r;
                                          if !constructCounterExamples then
                                            begin
                                            output_axiom_with_mapping outputChannel (ConceptInclusion(Range r, cex)) nameMapping; output_string outputChannel "\n";
                                            Statistics.collectForRangeCounterExample r cex;
                                            constructedCounterExamples := (Range r, cex)::(!constructedCounterExamples)
                                            end
                                          else
                                            print_endline ("Role range counter-example found for " ^ r ^ "."))
                 (Sigma.get_role_names sigma);
  print_endline("Number of differences w.r.t. the range of roles: " ^ (string_of_int !nrCounterExamples));
  (!constructedCounterExamples, !counterExampleWitnessesRef, !nrCounterExamples)
*)
(* kate: replace-tabs on; indent-width 2; *)