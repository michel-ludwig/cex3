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

(* Weak hash maps; inspired by Weak module from the standard library *)

open CommonTypes

module T : sig
  type 'a consed = private {
      data : 'a;
      tag : int;
    }
end

module type OHT = sig
  open T
  val compare : 'a consed -> 'a consed -> int
  val equal : 'a consed -> 'a consed -> bool
  val hash : 'a consed -> int
end
module OHT : OHT

open T
include OHT


module type HashedType =
sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S = sig
  type t
  type key
  val create: int -> t
  val cons: t -> key -> key consed
  val iter: (key consed -> unit) -> t -> unit
end

module Make(H: HashedType): (S with type key = H.t)
