(***********************************************************)
(*  Copyright (C) 2010 - 2014                              *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
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

open Types

(* Abstract type used to represent the role information recorded for an *)
(* ontology w.r.t. this module *)
type t

(* Creates a role information object *)
val create : unit -> t

(* "processRoleInclusion t r s" stores that the ontology corresponding to 't' *)
(* entails that 'r' <= 's'. This method is typically used during parsing. *)
val processRoleInclusion : t -> string -> string -> unit

(* "preRole t s" returns a list of all the role names r such that the *)
(* ontology corresponding to 't' entails "r <= s" *)
val preRole : t -> string -> StringSet.t

(* "preRoleSigma sigma t s" returns a list of all the role names r *)
(* such that the ontology corresponding to 't' entails "r <= s" and such that r *)
(* is in \Sigma*)
val preRoleSigma : Sigma.t -> t -> string -> StringSet.t

val iter_preRoleSigma : (string -> unit) -> Sigma.t -> t -> string -> unit

val for_all_preRoleSigma : (string -> bool) -> Sigma.t -> t -> string -> bool

(* "postRole t r" returns a list of all the role names s such that the *)
(* ontology corresponding to 't' entails "r <= s" *)
val postRole : t -> string -> StringSet.t

(* "postRoleSigma sigma t r" returns a list of all the role names s *)
(* such that the ontology corresponding to 't' entails "r <= s" and such that r *)
(* s in \Sigma*)
val postRoleSigma : Sigma.t -> t -> string -> StringSet.t

(* Returns as first component a list containing all the role inclusions (as pairs) using roles from *)
(* the first argument that are entailed by the second argument, but not by the third argument. *)
(* The second component is the number of role inclusions returned in the first component. *)
val computeRoleDifferences : Sigma.t -> t -> t -> (string * string) list * int

(* kate: replace-tabs on; indent-width 2; *)