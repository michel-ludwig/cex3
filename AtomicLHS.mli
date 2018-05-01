(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  Copyright (C) 2010-2014                                *)
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

open Types

val checkCEx : Sigma.t -> (string, string) Hashtbl.t -> out_channel
                       -> Terminology.t
                       -> Terminology.t
                       -> ((concept * concept) list * StringSet.t * int)
                       * ((concept * concept) list * StringSet.t * int)
                       * ((concept * concept) list * StringSet.t * int)


(*val checkRoleDomainCEx : Sigma.t -> (string, string) Hashtbl.t -> out_channel
                                 -> Ontology.t -> IndexTBox.t -> (string, StringSet.t) Hashtbl.t -> (string, StringSet.t) Hashtbl.t -> Ontology.occurrence_hash -> StringSet.t
                                 -> Ontology.t -> IndexTBox.t -> (string, StringSet.t) Hashtbl.t -> (string, StringSet.t) Hashtbl.t -> Ontology.occurrence_hash -> StringSet.t
                                 -> (concept * concept) list * StringSet.t * int

val checkRoleRangeCEx : Sigma.t -> (string, string) Hashtbl.t -> out_channel
                                -> Ontology.t -> IndexTBox.t -> (string, StringSet.t) Hashtbl.t -> (string, StringSet.t) Hashtbl.t -> Ontology.occurrence_hash -> StringSet.t
                                -> Ontology.t -> IndexTBox.t -> (string, StringSet.t) Hashtbl.t -> (string, StringSet.t) Hashtbl.t -> Ontology.occurrence_hash -> StringSet.t
                                -> (concept * concept) list * StringSet.t * int*)

(* kate: replace-tabs on; indent-width 2; *)
