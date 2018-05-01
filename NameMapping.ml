(***********************************************************)
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

open List
open Str

let lookupMapping hash c =
  try
    Hashtbl.find hash c
  with Not_found -> c

(* Mapping files are tabular separated tables, where the concept id is contained in the first column *)
(* and the third column contains the concept description. The first line is ignored. *)
let readMappingFiles fileNameList =
  let ret = Hashtbl.create 50 in

  let readMappingFile fileName =
    let inputChannel = open_in fileName
    in
    try
      (* we skip the first line *)
      let _ = input_line inputChannel in
      while true do
        let line = input_line inputChannel in
        let entriesList = Str.split (regexp "\t") line in
        if (length entriesList) >= 3 then
          begin
          let conceptID = List.nth entriesList 0 in
          let conceptFullySpecifiedName = List.nth entriesList 2 in
          if Hashtbl.mem ret conceptID then
            begin
            print_endline ("Concept ID " ^ conceptID ^ " occurs twice!");
            end
          else
            begin
            Hashtbl.add ret conceptID conceptFullySpecifiedName
            end
          end
        else
          print_endline ("Skipping entry: " ^ line);
      done
    with End_of_file -> ()
  in
  List.iter readMappingFile fileNameList;
  ret

(* kate: replace-tabs on; indent-width 2; *)