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

(** functions for manipulations with polarities **)

type t =
  | Positive
  | Negative
  | Both

let inverse = function
  | Positive -> Negative
  | Negative -> Positive
  | Both -> Both

let str = function
  | Positive -> "Positive"
  | Negative -> "Negative"
  | Both -> "Both"

let is_positive = function
  | Positive -> true
  | Negative -> false
  | Both -> true

let is_negative = function
  | Positive -> false
  | Negative -> true
  | Both -> true

module Counter = struct
  type p = t
  type t = {
    pos : int;
    neg : int;    
  }
    
  let get_pos c = c.pos
  let get_neg c = c.neg  
  let get_total c = c.pos + c.neg
  
  let zero = { pos = 0; neg = 0}
  let inverse c = { pos = c.neg; neg = c.pos }
  let symm c = { pos = c.pos + c.neg; neg = c.pos + c.neg }
  let to_elt = function
    | Positive -> { zero with pos = 1}
    | Negative -> { zero with neg = 1}
    | Both -> {pos = 1; neg = 1}  
  let succ c = function
    | Positive -> { c with pos = succ c.pos }
    | Negative -> { c with neg = succ c.neg }
    | Both -> { pos = succ c.pos; neg = succ c.neg }
  let pred c = function
    | Positive -> { c with pos = pred c.pos }
    | Negative -> { c with neg = pred c.neg }
    | Both -> { pos = pred c.pos; neg = pred c.neg }
  let sum c1 c2 = {
    pos = c1.pos + c2.pos;
    neg = c1.neg + c2.neg;    
  }
    
end

