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

(** functions for manipulations with polarities **)

  type t = Positive | Negative | Both
  val inverse : t -> t
  val str : t -> string
  val is_positive : t -> bool
  val is_negative : t -> bool

module Counter :
sig
  type p = t
  type t  
  val get_pos : t -> int
  val get_neg : t -> int
  val get_total : t -> int
  val zero : t
  val inverse : t -> t
  val symm : t -> t
  val to_elt : p -> t
  val succ : t -> p -> t
  val pred : t -> p -> t
  val sum : t -> t -> t  
end
