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

module IS = Intset_hc
module F = Format

let rec print_s f m = function
  | IS.SEmpty ->     
    F.pp_print_string f (string_of_int m);    
  | IS.SNode (l, mr, r) ->
    F.pp_open_box f 1;
    F.pp_print_string f "[";
    print_s f m l;
    F.pp_print_string f "]";
    F.pp_close_box f ();
    F.pp_open_box f 1;
    F.pp_print_string f " [";
    print_s f (m lxor mr) r;
    F.pp_print_string f "]";
    F.pp_close_box f ();
;;
let print_is f = function
  | IS.Empty -> F.pp_print_string f "[]";
  | IS.Node (m, s) -> print_s f m s;
;;        
  
