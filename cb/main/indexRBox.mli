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

open Owl2

type role_record = {
  (* the set of subproperties *)
  mutable subprop : Brole.Set.t;  
}

type t = {
  hrr : role_record ObjectProperty.HMap.t;
  (* the set of transitive atomic roles *)
  mutable trans_roles : ObjectProperty.Set.t;
  (* the set of functional roles *)
  mutable funct_roles : ObjectProperty.Set.t;
  (* the set of inverse functional roles *)
  mutable inv_funct_roles : ObjectProperty.Set.t;
}

val create : int -> t
val init : Ontology.t -> t
