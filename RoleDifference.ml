(***********************************************************)
(*  Copyright (C) 2010-2011                                *)
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
open Utilities

type roleHashes = { postHash : (string, string) Hashtbl.t;
                    preHash : (string, string) Hashtbl.t;
                    preHashResult : (string, StringSet.t) Hashtbl.t;
                    postHashResult : (string, StringSet.t) Hashtbl.t;
                  }

type t = roleHashes

let create ont = { postHash = Hashtbl.create 50;
                   preHash = Hashtbl.create 50;
                   preHashResult = Hashtbl.create 50;
                   postHashResult = Hashtbl.create 50;
                 }

let printHash h =
     print_endline "Hash:";
     Hashtbl.iter (fun r -> fun s -> print_endline ("(" ^ r ^ "," ^ s ^ ")")) h

let processRoleInclusion t r s =
      if String.compare r s = 0 then
        () (* We don't do anything if r = s as this will lead to infinite recursion *)
           (* and crashes. *)
      else
        begin
        Hashtbl.add t.postHash r s;
        Hashtbl.add t.preHash s r
        end

let preRole t s =
  (* we have to keep track of which roles we have seen already in order to avoid *)
  (* an infinite loop in the case of 'r => s' and 's => r', for example *)
  try
    Hashtbl.find t.preHashResult s
  with Not_found ->
    let rec preRole_ seenSet s =
      if StringSet.mem seenSet s then
        StringSet.empty
      else
        begin
        let newSeenSet = StringSet.add seenSet s in
        List.fold_left (fun set child -> StringSet.union (preRole_ newSeenSet child)
                                                         (StringSet.add set child))
                       (StringSet.empty)
                       (Hashtbl.find_all t.preHash s)
        end
    in
    let toReturn = StringSet.add (preRole_ StringSet.empty s) s  in
    Hashtbl.add t.preHashResult s toReturn; (* don't forget this! *)
    toReturn

let preRoleSigma sigma t s =
  StringSet.intersection (Sigma.get_role_names sigma) (preRole t s) 

let iter_preRoleSigma f sigma t s =
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.iter (fun r -> if StringSet.mem sigmaRoleNames r then
                             f(r))
                 (preRole t s)

let for_all_preRoleSigma f sigma t s =
  let sigmaRoleNames = Sigma.get_role_names sigma in
  StringSet.for_all (fun r -> if StringSet.mem sigmaRoleNames r then
                                f(r)
                              else
                                true)
                    (preRole t s)
                 
let postRole t r =
  (* we have to keep track of which roles we have seen already in order to avoid *)
  (* an infinite loop in the case of 'r => s' and 's => r', for example *)
  try
    Hashtbl.find t.postHashResult r
  with Not_found ->
    let rec postRole_ seenSet s =
      if StringSet.mem seenSet s then
        StringSet.empty
      else
        begin
        let newSeenSet = StringSet.add seenSet s in
        List.fold_left (fun set child -> StringSet.union (postRole_ newSeenSet child)
                                                         (StringSet.add set child))
                       (StringSet.empty)
                       (Hashtbl.find_all t.postHash s)
        end
    in
    let toReturn = StringSet.add (postRole_ StringSet.empty r) r  in
    Hashtbl.add t.postHashResult r toReturn; (* don't forget this! *)
    toReturn

let postRoleSigma sigma t r =
  StringSet.intersection (Sigma.get_role_names sigma) (postRole t r) 

let computeRoleDifferences sigma t t' = 
  let sigmaRoles = Sigma.get_role_names sigma in
  let makePair r set =
    let i = ref 0 in
    (StringSet.fold (fun l x -> incr i; ((x, r)::l)) [] set, !i)
  in
  StringSet.fold (fun (l, oldCount) -> fun r -> let (l2, newCount) = makePair r (StringSet.difference (preRoleSigma sigma t' r) (preRoleSigma sigma t r)) in
                                                (List.rev_append l l2, oldCount + newCount))
                 ([], 0)
                 sigmaRoles

(* kate: replace-tabs on; indent-width 2; *)