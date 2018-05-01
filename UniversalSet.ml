(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  Copyright (C) 2010                                     *)
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

type cname = string

module PS = Set.Make(
   struct 
     type t = cname
     let compare = compare
   end
)
;;
exception EnumerateUniversal;;
type t = Universal | Data of PS.t

(* printing debug info *)
let print_entry s = print_string (s ^ ", ")

let print_set uset = match uset with
 | Universal -> print_string "universal"
 | Data(set) -> PS.iter print_entry set
;;

(* "constructors" *)
let empty = Data(PS.empty);;
let singleton value = Data (PS.singleton value);;
let universal = Universal;;

let cardinal uset = match uset with
  | Universal -> max_int
  | Data(set) -> PS.cardinal set

(* filling up *)
let add value uset = match uset with
  | Universal -> Universal (* nothing can be added to a universal set*)
  | Data(set) -> Data (PS.add value set)
;;

let rec add_list lst uset = match lst with
  | head::tail -> add_list tail (add head uset)
  | [] -> uset

(* set operations *)
let inter uset1 uset2 = match (uset1, uset2) with
 | (Universal, _) -> uset2;
 | (_, Universal) -> uset1;
 | (Data(set1), Data(set2)) -> Data(PS.inter set1 set2)
;;


let union uset1 uset2 = match (uset1, uset2) with
 | (Universal, _) -> Universal
 | (_, Universal) -> Universal
 | (Data(set1), Data(set2)) -> Data(PS.union set1 set2)
;;

(* iterators *)
let fold fn uset init =  match uset with
 | Universal -> raise EnumerateUniversal
 | Data(set) -> PS.fold fn set init
;;

let iter fn uset =  match uset with
 | Universal -> raise EnumerateUniversal
 | Data(set) -> PS.iter fn set
;;

(* tests *)

let is_universal uset = match uset with 
  | Universal -> true 
  | _ -> false
;;

let is_empty uset = match uset with
  | Universal -> false 
  | Data(set) -> PS.is_empty set;;

let mem value uset = match uset with
  | Universal -> true
  | Data(set) -> PS.mem value set
;;

(* kate: replace-tabs on; indent-width 2; *)
