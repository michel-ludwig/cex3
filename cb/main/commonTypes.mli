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

module type OrderedType = sig type t val compare : t -> t -> int end
module type HashedType =
  sig type t val equal : t -> t -> bool val hash : t -> int end
module type OrderedHashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end
module type OrderedHashedTypeStr =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
    val str : t -> string
  end
module OrderedList :
  sig
    module type S = OrderedHashedTypeStr
    module Make :
      functor (O : OrderedHashedTypeStr) ->
        sig
          type t = O.t list
          val equal : t -> t -> bool
          val hash : t -> int
          val compare : t -> t -> int
          val str : t -> string
        end
  end
module OrderedPair :
  sig
    module type S = OrderedHashedTypeStr
    module Make :
      functor (O1 : OrderedHashedTypeStr) ->
        functor (O2 : OrderedHashedTypeStr) ->
          sig
            type t = O1.t * O2.t
            val equal : t -> t -> bool
            val hash : t -> int
            val compare : t -> t -> int
            val str : t -> string
          end
  end
