(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  This library is free software; you can redistribute it *)
(*  and/or modify it under the terms of the GNU Lesser     *)
(*  General Public License as published by the Free        *)
(*  Software Foundation; either version 2.1 of the         *)
(*  License, or (at your option) any later version.        *)
(*                                                         *)
(*  This library is distributed in the hope that it will   *)
(*  be useful, but WITHOUT ANY WARRANTY; without even the  *)
(*  implied warranty of MERCHANTABILITY or FITNESS FOR A   *)
(*  PARTICULAR PURPOSE. See the GNU Lesser General Public  *)
(*  License for more details.                              *)
(*                                                         *)
(*  You should have received a copy of the GNU Lesser      *)
(*  General Public License along with this library; if     *)
(*  not write to the Free Software Foundation, Inc., 51    *)
(*  Franklin Street, Fifth Floor, Boston, MA  02110-1301   *)
(*  USA                                                    *)
(***********************************************************)

(* sets of integers based on a variation of Patricia trees *)

module type S = sig
  type elt = int
  type s = private
    | SEmpty
    | SNode of s * elt * s
  type t = private
    | Empty
    | Node of elt * s
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val mem_s: elt -> elt -> s -> bool
  val add: elt -> t -> t
  val add_f : elt -> (unit -> unit) -> (unit -> unit) -> t -> t
  val singleton: elt -> t
  val is_singleton : t -> bool
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val iter2 : (elt -> unit) -> t -> t -> unit
  val iter_diff : (elt -> unit) -> t -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val choose: t -> elt
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val right: t -> t -> t -> bool
end

include S
