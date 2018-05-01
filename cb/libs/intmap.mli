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

(* maps on integers based on a variation of Patricia trees *)

module type S = sig
  type key = int
  type 'a t
  exception Remove
  val empty: 'a t
  val is_empty: 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val is_singleton : 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val add_f : key -> (unit -> 'a) -> ('a -> 'a) -> 'a t -> 'a t
  val replace : key -> 'a -> 'a t -> 'a t
  val replace_f : key -> ('a -> 'a) -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val remove: key -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2 : (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val filter : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val diff : 'a t -> 'b t -> 'a t (** todo: extend with a diff parameter *)
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val choose : 'a t -> (key * 'a)
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val right: ('a -> 'a -> bool) -> ('a -> 'a -> 'a -> bool) -> 'a t -> 'a t -> 'a t -> bool
  module Set : Iset.S with type elt = key
  val iter_s : (key -> 'a -> unit) -> 'a t -> Set.t -> unit
end

include S
