(***********************************************************)
(*  Copyright (C) 2013-2014                                *)
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

open Debug

let compute_simulation toBeProcessedList dependencyHash successorInformationHash failingHash
                       isContainedInNonSimulation addToNonSimulation isContainedInQueue addToQueueSet removeFromQueueSet isContainedInSimulation addToSimulation removeFromSimulation checkLocalConditions
                       nextSuccessors
                       pairToString (init : 'a) =

(* let toBeProcessedList : 'a list ref = ref [] in *)
(*let dependencyHash : ('a, ('a * 'd) list ref) Hashtbl.t = Hashtbl.create 50
let successorInformationHash : ('a, 'b) Hashtbl.t = Hashtbl.create 50
let failingHash : ('a, ('a * 'd) list ref) Hashtbl.t = Hashtbl.create 50*)

let add_dependency pair dep =
  try
    let lRef = Hashtbl.find dependencyHash pair in
    if not (Utilities.list_contains !lRef dep) then
      begin
      lRef := dep::!lRef
      end
  with Not_found ->
    Hashtbl.add dependencyHash pair (ref [dep])
 in

let get_dependencies pair =
  try
    !(Hashtbl.find dependencyHash pair)
  with Not_found ->
    []
 in

let clear_dependencies pair =
  Hashtbl.remove dependencyHash pair
 in

let update_successors_hash pair l =
  Hashtbl.remove successorInformationHash pair;
  Hashtbl.add successorInformationHash pair l
 in

let pop_from_queue _ =
  let pair = List.hd !toBeProcessedList in
  toBeProcessedList := List.tl !toBeProcessedList;
  removeFromQueueSet pair;
  pair
 in

let push_to_queue pair =
  if not (isContainedInQueue pair) then
    begin
(* print_endline("ADDING TO QUEUE " ^ (pairToString pair)); *)
    toBeProcessedList := pair::!toBeProcessedList;
    addToQueueSet pair
    end
(*   else *)
(* print_endline("QUEUE CONTAINS ALREADY " ^ (pairToString pair)); *)
 in

let add_to_failing_hash pair (failingPair, depInfo)  =
  try
    let lRef = Hashtbl.find failingHash pair in
    if not (Utilities.list_contains !lRef (failingPair, depInfo)) then
      begin
      lRef := (failingPair, depInfo)::!lRef
      end
  with Not_found ->
    Hashtbl.add failingHash pair (ref [(failingPair, depInfo)])
in

let remove_from_failing_hash pair =
  Hashtbl.remove failingHash pair
in

let get_failing_pairs pair =
  try
    !(Hashtbl.find failingHash pair)
  with Not_found ->
    []
in
 
(* 'a is the type of a simulation pair or triple *)
  Hashtbl.clear dependencyHash;
  Hashtbl.clear successorInformationHash;    
  toBeProcessedList := [];

  let check_next_pair pair failingPairs =
    (* check whether we have seen this pair already... *)
    if not (Hashtbl.mem successorInformationHash pair) then
      if not (checkLocalConditions pair) then
        begin
        debug_endline ("\tLocal simulation conditions are not fulfilled.");
        None
        end
      else
        begin
        let r = ref None in
        update_successors_hash pair r;
        nextSuccessors pair r failingPairs
        end
    else
      nextSuccessors pair (Hashtbl.find successorInformationHash pair) failingPairs
  in

  push_to_queue init;

  let nrIterations = ref 0 in
  while not (Utilities.list_is_empty !toBeProcessedList) do
(* Utilities.execute_measure_time "Next iteration" *)
(* (fun _ -> *)
    incr nrIterations;
    let pair = pop_from_queue () in
    
    let failingPairs = get_failing_pairs pair in
    remove_from_failing_hash pair;
    
(*    debug_endline ("");
    debug_endline ("List:");
    List.iter (fun p -> print_string((pairToString p) ^ ", "))
              !toBeProcessedList;*)
    debug_endline("");
    debug_endline ("");
    debug_endline ("Chosen: " ^ (pairToString pair));
    
    if not (isContainedInSimulation pair) then
      begin
      match check_next_pair pair failingPairs with
	None -> (* this pair cannot be in a simulation *)
(*                 removeFromSimulation pair; *)
                addToNonSimulation pair;
                debug_endline ("\tcheck_next_pair returned None!");
                List.iter (fun (parent, depInfo) -> removeFromSimulation parent;
                                                    if not (isContainedInNonSimulation parent) then
                                                      begin
                                                      add_to_failing_hash parent (pair, depInfo);
                                                      push_to_queue parent
                                                      end)
                          (get_dependencies pair);
                clear_dependencies pair
      | Some (l) -> begin
                    debug_endline ("\tcheck_next_pair returned the following dependencies:");
(*                     debug_endline ("\t" ^ List.fold_left (fun s p -> s ^ " " ^ (pairToString p)) "" l); *)
                    List.iter (fun (pair', depInfo) -> add_dependency pair' (pair, depInfo);
                                                       push_to_queue pair')

                              l;
                    addToSimulation pair;
                    end
      end
    else
      debug_endline ("\tContained in simulation already!");
(* ); *)
  done;
(*   print_endline("toBeProcessedList is empty now!"); *)
(*   print_endline("Number of iterations: " ^ (string_of_int !nrIterations));; *)

(* kate: replace-tabs on; indent-width 2; *)
