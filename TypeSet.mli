(***********************************************************)
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
(***********************************************************)

(* Abstract type used to represent sets which contain elements of an arbitrary type *)
(* It should allow for fast membership lookup *)

module type TypeSet = sig
  type t

  type element

  val empty : t

  val is_empty : t -> bool

  val is_singleton : t -> bool

  val mem : t -> element -> bool

  val add : t -> element -> t

  val remove : t -> element -> t

  val singleton : element -> t

  val from_list : element list -> t

  val to_list : t -> element list

  val choose : t -> element

  val fold : ('a -> element -> 'a) -> 'a -> t -> 'a

  val map_to_list : (element -> 'b) -> t -> 'b list
  
  val fold_non_empty : (element -> 'a) -> ('a -> element -> 'a) -> t -> 'a

  val filter : (element -> bool) -> t -> t

  val iter : (element -> unit) -> t -> unit

  (*  "difference S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 but not to S2 *)
  val difference : t -> t -> t

  (*  "intersection S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 and S2 *)
  val intersection : t -> t -> t

  (*  "union S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 or S2 *)
  val union : t -> t -> t

  val print_set : t -> unit

  val to_string : t -> string

  val update_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val replace_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val diff_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val update_hash_with_element : ('a, t) Hashtbl.t -> 'a -> element -> unit

  val find_in_hash_no_exception : ('a, t) Hashtbl.t -> 'a -> t

  val contained_in_hash : ('a, t) Hashtbl.t -> 'a -> element -> bool

  val pair_mem_hash : ('a, t) Hashtbl.t -> 'a -> element -> bool

  val cardinal : t -> int

  val for_all : (element -> bool) -> t -> bool

  val exists : (element -> bool) -> t -> bool

  (* Returns true iff the first argument is a subset of the second argument  *)
  val subset : t -> t -> bool

  val find_object : (element -> 'a option) -> t -> 'a option

  (* computes the intersection of the sets obtained by applying the function given as first argument on all *)
  (* the list elements *)
  val compute_list_intersection : (element -> t) -> element list -> t

  (* 'find_in_intersection f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 and set2 *)
  val find_in_intersection : (element -> bool) -> t -> t -> element option

  (* 'find_in_intersection_except f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 and set2 but none of the sets in l *)
  val find_in_intersection_except : (element -> bool) -> t -> t -> t list -> element option

  (* 'find_in_difference f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 but not to set2 *)
  val find_in_difference : (element -> bool) -> t -> t -> element option
end

module type Element = sig
  type t
  val to_string : t -> string
end

module Make(Element : Element) : TypeSet with type element = Element.t

(* kate: replace-tabs on; indent-width 2; *) 
