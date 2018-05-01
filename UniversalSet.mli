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

type t

val print_set : t -> unit

val empty : t
val singleton : string -> t
val universal : t

val cardinal : t -> int

val add : string -> t -> t

val add_list : string list -> t -> t

val inter : t -> t -> t

val union : t -> t -> t

val fold : (string -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (string -> unit) -> t -> unit

val is_universal : t -> bool

val is_empty : t -> bool

val mem : string -> t -> bool

(* kate: replace-tabs on; indent-width 2; *)