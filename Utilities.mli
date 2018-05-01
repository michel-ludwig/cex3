(***********************************************************)
(*  Copyright (C) 2010 - 2014                              *)
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

val hashTableContains : ('a, 'b) Hashtbl.t -> 'a * 'b -> bool

val print_string_list : string list -> unit

val compute_time_difference : Unix.process_times -> Unix.process_times -> Unix.process_times
val print_total_time : Unix.process_times -> unit
val print_total_time_difference : Unix.process_times -> Unix.process_times -> unit

val singleton_list_to_element : 'a list -> 'a

val list_is_empty : 'a list -> bool

val list_contains : 'a list -> 'a -> bool

val string_is_empty : string -> bool

val execute_measure_time : string -> (unit -> 'a) -> 'a

val compact_heap : unit -> unit

(* Contrary to 'Hashtbl.add', this function does not add a binding *)
(* to a given hashtable 'h' if it is present in 'h' already *)
val add_to_hash : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit

val add_unique_value : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit

val find_option : ('a, 'b) Hashtbl.t -> 'a -> 'b option

val list_find_object : (string -> 'a option) -> string list -> 'a option

(* return true iff 'str1' begins with 'str2' *)
val begins_with : string -> string -> bool

val is_original_concept_name : string -> bool

(* Memory usage is determined by using the information provided in /proc/self/status *)
val print_maximum_memory_usage : unit -> unit

val get : 'a option -> 'a

val is_some : 'a option -> bool

(* kate: replace-tabs on; indent-width 2; *)