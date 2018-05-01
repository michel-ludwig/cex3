(***********************************************************)
(*  Copyright (C) 2010-2011                                *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
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

open Settings
open Types
open Utilities

let counterExampleLHSToRHSHash = Hashtbl.create 50
let numberRHSCounterExamplesWithAtomicLHS = ref 0
let numberLHSCounterExamplesWithAtomicRHS = ref 0
let numberRHSCounterExamplesWithAtMostOneAtomicConjunct = ref 0
let numberLHSCounterExamplesNotInELRan = ref 0
let numberLHSCounterExamplesWithUniversalRole = ref 0
let numberLHSCounterExamplesWithRoleConjunction = ref 0

let collectForLHSCounterExample lhs c =
  if is_atomic c then
    begin
    incr numberLHSCounterExamplesWithAtomicRHS;
    end
  else if not (is_EL_ran_concept c) then
    begin
    incr numberLHSCounterExamplesNotInELRan
    end;
  if contains_universal_role c then
    begin
    incr numberLHSCounterExamplesWithUniversalRole
    end;
  if contains_role_conjunction c then
    begin
    incr numberLHSCounterExamplesWithRoleConjunction
    end

let collectForDomainCounterExample lhs c =
  ()

let collectForRangeCounterExample lhs c =
  ()

let countAtomicConcepts c =
  let rec countAtomicConcepts_ l acc =
    match l with [] -> acc
        | h::tl -> (match h with Top -> countAtomicConcepts_ tl (acc + 1)
                               | Exists(_, _) -> countAtomicConcepts_ tl acc
                               | Domain _ -> countAtomicConcepts_ tl acc
                               | Range _ -> countAtomicConcepts_ tl acc
                               | Name _ -> countAtomicConcepts_ tl (acc + 1)
                               | And ls -> (countAtomicConcepts_ (List.rev_append ls tl) acc)
                               | ExistsRoleConjunction(_, _) -> countAtomicConcepts_ tl acc
                               | ExistsUniversalRole _ -> countAtomicConcepts_ tl acc)
  in
  countAtomicConcepts_ [c] 0

let containsAtMostOneAtomicConjunct c =
(* print_string("for "); print_concept c; print_endline(" : " ^ (string_of_int (countAtomicConcepts c))); *)
  ((countAtomicConcepts c) <= 1)

let collectForRHSCounterExample c rhs =
  StringSet.update_hash_with_element counterExampleLHSToRHSHash c rhs;
  if is_atomic c then
    begin
    incr numberRHSCounterExamplesWithAtomicLHS;
    end;
  if containsAtMostOneAtomicConjunct c then
    begin
    incr numberRHSCounterExamplesWithAtMostOneAtomicConjunct;
    end

let print_statistics _ =
  if !extendedStatistics && !constructCounterExamples then
    begin
      let numberOfLHSOfAtomicRHSCounterExamplesWithMultipleRHS = ref 0 in
      let maxNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS = ref 0 in
      let totalNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS = ref 0.0
      in
      Hashtbl.iter (fun _ rhsSet -> let setSize = StringSet.cardinal rhsSet in
                                    if setSize > 1 then
                                      begin
                                      incr numberOfLHSOfAtomicRHSCounterExamplesWithMultipleRHS;
                                      maxNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS := max !maxNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS setSize;
                                      totalNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS := !totalNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS +. (float setSize);
                                      end)
                    counterExampleLHSToRHSHash;
      print_endline ("Number of atomic RHS counter-examples with atomic LHS: " ^ (string_of_int !numberRHSCounterExamplesWithAtomicLHS));
      print_endline ("Number of atomic RHS counter-examples with at most one atomic conjunct: " ^ (string_of_int !numberRHSCounterExamplesWithAtMostOneAtomicConjunct));
      print_endline ("Number of LHS of atomic RHS counter-examples which appear multiple times: " ^ (string_of_int !numberOfLHSOfAtomicRHSCounterExamplesWithMultipleRHS));
      print_endline ("Maximal number of RHS associated with a LHS of atomic RHS counter-examples which appears multiple times: " ^ (string_of_int !maxNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS));
      print_endline ("Average number of RHS associated with a LHS of atomic RHS counter-examples which appears multiple times: " ^ (string_of_float (!totalNumberOfRHSForAtomicRHSCounterExamplesWithSameLHS /. (float_of_int !numberOfLHSOfAtomicRHSCounterExamplesWithMultipleRHS))));
      print_endline ("Number of atomic LHS counter-examples with atomic RHS: " ^ (string_of_int !numberLHSCounterExamplesWithAtomicRHS));
      print_endline ("Number of atomic LHS counter-examples that are not in EL^ran: " ^ (string_of_int !numberLHSCounterExamplesNotInELRan));
      print_endline ("Number of atomic LHS counter-examples with universal role: " ^ (string_of_int !numberLHSCounterExamplesWithUniversalRole));
      print_endline ("Number of atomic LHS counter-examples with role conjunction: " ^ (string_of_int !numberLHSCounterExamplesWithRoleConjunction));
    end

(* kate: replace-tabs on; indent-width 2; *) 
