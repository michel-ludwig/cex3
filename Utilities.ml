(***********************************************************)
(*  Copyright (C) 2010-2014                                *)
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

exception StopIteration

let hashTableContains h (r, s) = List.mem s (Hashtbl.find_all h r)

let print_string_list l =
  let rec printStringList_ l =
    match l with
      []    -> ()
    | [x]   -> print_string x
    | x::xs -> print_string (x ^ ", "); printStringList_ xs
  in print_string "["; printStringList_ l; print_string "]"

let compute_time_difference t1 t2 =
  { Unix.tms_utime = t2.Unix.tms_utime -. t1.Unix.tms_utime;
    Unix.tms_stime = t2.Unix.tms_stime -. t1.Unix.tms_stime;
    Unix.tms_cutime = t2.Unix.tms_cutime -. t1.Unix.tms_cutime;
    Unix.tms_cstime = t2.Unix.tms_cstime -. t1.Unix.tms_cstime;
  }

let print_total_time t =
  Printf.printf "%.3f" (t.Unix.tms_utime +. t.Unix.tms_stime); flush stdout

let print_total_time_difference t1 t2 =
  print_total_time (compute_time_difference t1 t2)

let singleton_list_to_element l =
  match l with
    [e] -> e
  | _ -> failwith "Too many elements"

let list_is_empty l =
  match l with [] -> true
            |  _  -> false

let list_contains l a  =
  try
    List.iter (fun x -> if x = a then
                          raise StopIteration)
              l;
    false
  with StopIteration ->
    true
            
let string_is_empty s =
  (s = "")

let execute_measure_time str f =
  print_endline str;
  let start_time = Unix.times() in
  let r = f () in
  let end_time = Unix.times() in
  print_string("Done in "); print_total_time_difference start_time end_time; print_string("s."); print_newline();
  r

let compact_heap _ = 
  execute_measure_time "Compacting heap..." Gc.compact

let add_to_hash hash key value =
  let boundValues = Hashtbl.find_all hash key in
  if List.exists (fun x -> x = value) boundValues then
    ()
  else
    Hashtbl.add hash key value

let add_unique_value hash key value =
  if Hashtbl.mem hash key then
    ()
  else
    Hashtbl.add hash key value

let find_option hash key =
  try
    Some (Hashtbl.find hash key)
  with Not_found ->
    None
    
let list_find_object f l =
  let returnRef = ref None in
  try 
    List.iter (fun str -> let res = f str in
                          if res != None then
                            begin
                              returnRef := res;
                              raise StopIteration
                            end
                          else
                            ())
          l;
    !returnRef
  with StopIteration -> !returnRef

let begins_with str1 str2 =
  let str2length = String.length str2 in
  if String.length str1 < str2length then
    false
  else if (String.compare (String.sub str1 0 str2length) str2) = 0 then
    true
  else
    false

let is_original_concept_name str =
  not (begins_with str ":")

let print_maximum_memory_usage _ =
  if Sys.os_type = "Unix" then
    begin
    try
      let procIn = open_in "/proc/self/status" in
      let vmPeakRef = ref "" in
      try
        while true do
          let line = input_line procIn in
          if Str.string_match (Str.regexp "[vV][mM][pP][eE][aA][kK]:[\t ]*\\(.*\\)") line 0 then
            begin
            try
              vmPeakRef := Str.matched_group 1 line;
              raise End_of_file; (* we are done *)
            with Not_found -> ()
            end
        done
      with End_of_file -> ();
      if string_is_empty !vmPeakRef then
        print_endline "No information about memory usage available from the kernel (no VmPeak information found)."
      else
        print_endline ("Maximum (virtual) memory used (VmPeak): " ^ !vmPeakRef);
      close_in procIn
    with Sys_error s -> print_endline("No information about memory usage available from the kernel (/proc/self/status couldn't be opened: " ^ s ^ ").")
    end
    
let get o =
  match o with
    Some j -> j
  | None -> raise (Invalid_argument "Invalid argument given in 'Utilities.get'!")

let is_some o =
  match o with
    Some _ -> true
  | None -> false
  
(* kate: replace-tabs on; indent-width 2; *)