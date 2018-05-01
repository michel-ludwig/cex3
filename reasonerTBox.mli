(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010                                     *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
(*  University of Liverpool                                *)
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
open Types

type t
val find_implied : t -> ClassExpression.t -> ClassExpression.HSet.t
val find_option_top : t -> ClassExpression.t option
val saturate : Ontology.t -> t * IndexTBox.t

(* This doesn't seem to be used and can be removed. *)
(* val sorted_concept_name_list : Ontology.t -> IndexTBox.t -> string list *)

val implied_concept_names : t -> string -> string list

val compute_post_for_concepts : t -> (string, StringSet.t) Hashtbl.t

val compute_pre_post_for_concepts : t -> (string, StringSet.t) Hashtbl.t * (string, StringSet.t) Hashtbl.t

(* kate: replace-tabs on; indent-width 2; *)
