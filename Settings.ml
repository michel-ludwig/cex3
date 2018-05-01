(***********************************************************)
(*                                                         *)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
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



let debug = ref false;;
let verbose = ref false;;
let ignoreCase = ref false;;
let constructCounterExamples = ref false;;
let simplifyCounterExamplesSemantically = ref false;;
let extendedStatistics = ref false;;
let checkCounterExamples = ref false;;

let debug_string str = if !debug then print_string str else ();;
let debug_endline str = if !debug then print_endline str else ();;
let debug_newline () = if !debug then print_newline () else ();;

let debugE expr = if !debug then Lazy.force expr else ();;

type mode = InstanceCase | ConceptCase | ConjunctiveQueryCase

let mode = ref ConceptCase

type signature_mode = FromTBox1 | FromTBox2 | FromIntersection

let signature_mode = ref FromTBox1

let is_concept_case _ =
  (!mode = ConceptCase)

let is_instance_case _ =
  (!mode = InstanceCase)

let is_conjunctive_query_case _ =
  (!mode = ConjunctiveQueryCase)

(* kate: replace-tabs on; indent-width 2; *)
