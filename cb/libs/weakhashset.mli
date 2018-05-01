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

module type S = sig
  type elt
  (** The type of the elements stored in the table. *)
  type t
  (** The type of tables that contain elements of type [data].
  Note that weak hash tables cannot be marshaled using
  {!Pervasives.output_value } or the functions of the {!Marshal }
  module. *)
  val create : int -> t
  (** [create n] creates a new empty weak hash table, of initial
  size [n]. The table will grow as needed. *)
  val clear : t -> unit
  (** Remove all elements from the table. *)
  val merge : t -> elt -> elt
  (** [merge t x] returns an instance of [x] found in [t] if any,
  or else adds [x] to [t] and return [x]. *)
end

(** The output signature of the functor {!Weak.Make}. *)

module Make (H : Hashtbl.HashedType) : S with type elt = H.t;;
(** Functor building an implementation of the weak hash table structure. *)
