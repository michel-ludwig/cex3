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

(** making a progress bar on an ANSI-complient terminal *)

(* initialize the progress bar with the maximal status value *)
val init: int -> unit
(* increment the current status value *)
val step: unit -> unit
(* setting the current status value *)
val set_state : int -> unit
(* setting the status to the maximal value *)
val set_max : unit -> unit
(* decrement the current status value *)
val back: unit -> unit
(* increment the maximal status value *)
val incr_max: unit -> unit
(* get the status of the progress bar: returns an integer between 0 and 100 *)
val get_status : unit -> int
(* get the running time the progress bar *)
val get_time : unit -> float
