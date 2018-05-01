(***********************************************************)
(*  Copyright (C) 2010 - 2011                              *)
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

(* Abstract type used to represent the role information recorded for an *)
(* ontology w.r.t. this module *)
type t

val empty : t

val is_empty : t -> bool

val is_singleton : t -> bool

val mem : t -> string -> bool

val add : t -> string -> t

val singleton : string -> t

val from_list : string list -> t

val to_list : t -> string list

val choose : t -> string

val fold : ('a -> string -> 'a) -> 'a -> t -> 'a

val fold_non_empty : (string -> 'a) -> ('a -> string -> 'a) -> t -> 'a

val iter : (string -> unit) -> t -> unit

(*  "difference S1 S2" computes the set of all the strings 's' such *)
(*  that 's' belongs to S1 but not to S2 *)
val difference : t -> t -> t

(*  "intersection S1 S2" computes the set of all the strings 's' such *)
(*  that 's' belongs to S1 and S2 *)
val intersection : t -> t -> t

(*  "union S1 S2" computes the set of all the strings 's' such *)
(*  that 's' belongs to S1 or S2 *)
val union : t -> t -> t

val print_set : t -> unit

val update_stringset_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

val update_stringset_hash_with_string : ('a, t) Hashtbl.t -> 'a -> string -> unit

val find_in_hash_no_exception : ('a, t) Hashtbl.t -> 'a -> t

val pair_mem_hash : ('a, t) Hashtbl.t -> 'a -> string -> bool

val cardinal : t -> int

val for_all : (string -> bool) -> t -> bool

val exists : (string -> bool) -> t -> bool

val subset : t -> t -> bool

val equal : t -> t -> bool

val find_object : (string -> 'a option) -> t -> 'a option

(* computes the intersection of the sets obtained by applying the function given as first argument on all *)
(* the list elements *)
val compute_list_intersection : (string -> t) -> string list -> t

(* 'find_in_intersection f set1 set2 l' returns a string s iff f(s) is true, *)
(* s belongs to set1 and set2 *)
val find_in_intersection : (string -> bool) -> t -> t -> string option

(* 'find_in_intersection_except f set1 set2 l' returns a string s iff f(s) is true, *)
(* s belongs to set1 and set2 but none of the sets in l *)
val find_in_intersection_except : (string -> bool) -> t -> t -> t list -> string option

(* kate: replace-tabs on; indent-width 2; *) 
