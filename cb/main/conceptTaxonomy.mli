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

type t
val compute : ReasonerTBox.t -> Ontology.t -> t
val find_iter : t -> Class.t -> ((Class.t -> unit) -> unit) * ((Class.t -> unit) -> unit) * ((Class.t -> unit) -> unit)
val print_lisp_fast : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_lisp_slow : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_lisp : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_krss : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_fowl : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_fowl_impl : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
val print_statistics : ReasonerTBox.t -> Ontology.t -> out_channel -> unit
