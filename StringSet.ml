(***********************************************************)
(*  Copyright (C) 2010 - 2011                              *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
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

open Cset
open Consed.T

module StringS = Consed.Make (struct
                               type t = string
                               let equal str1 str2 = (str1 = str2)
                               let hash = Hashtbl.hash
                              end)

module StringCset = Cset.Make(struct type t = string end)

let stringModuleLookupTable = StringS.create 127

type t = StringCset.t

let empty = StringCset.empty

let is_empty t =
  StringCset.is_empty t

let is_singleton t =
  StringCset.is_singleton t

let mem t (s :string) = StringCset.mem (StringS.cons stringModuleLookupTable s) t

let add t str =
  StringCset.add (StringS.cons stringModuleLookupTable str) t

let singleton s =
  add empty s

let from_list l =
  List.fold_left (fun set s -> add set s) empty l

let choose t =
  (StringCset.choose t).data

let fold f a t = 
  StringCset.fold (fun s -> fun v -> f v (s.data)) t a

let fold_non_empty firstCase generalCase t =
  match fold (fun res -> fun s -> match res with None -> Some (firstCase s)
                                              | (Some r) -> Some (generalCase r s))
             None t with
    Some r -> r
  | None -> raise Not_found

let to_list t =
  fold (fun ls s -> s::ls) [] t

let iter f t = 
  StringCset.iter (fun s -> f (s.data)) t
(*
let addSet t1 t2 =
  iter (fun x -> add t1 x) t2*)

let difference s1 s2 =
  StringCset.diff s1 s2

let intersection s1 s2 =
  StringCset.inter s1 s2

let union s1 s2 =
  StringCset.union s1 s2

let print_set t =
  print_string "{"; iter (fun x -> print_string (x ^ ", ")) t; print_string "}"

let update_stringset_hash hash a set =
  if not (Hashtbl.mem hash a) then
    Hashtbl.add hash a set
  else
    let current_set = Hashtbl.find hash a in
    Hashtbl.replace hash a (StringCset.union current_set set)

let update_stringset_hash_with_string hash (a: 'a) str =
  try
    let current_set = Hashtbl.find hash a in
    Hashtbl.replace hash a (add current_set str)
  with Not_found ->
    Hashtbl.add hash a (add empty str)

let find_in_hash_no_exception hash a =
  try
    Hashtbl.find hash a
  with Not_found ->
    Empty

let pair_mem_hash hash a b =
  try
    let set = Hashtbl.find hash a
    in
    mem set b
  with Not_found ->
    false

let cardinal s =
  StringCset.cardinal s

exception StopIteration

let for_all f s =
  try 
    iter (fun str -> if not (f str) then
                        raise StopIteration
                      else
                        ())
          s;
    true
  with StopIteration -> false

let exists f s =
  try 
    iter (fun str -> if f str then
                        raise StopIteration
                      else
                        ())
          s;
    false
  with StopIteration -> true

let subset t1 t2 =
  for_all (fun e -> mem t2 e) t1

let equal t1 t2 =
  (subset t1 t2) && (subset t2 t1)

let find_object f s =
  let returnRef = ref None in
  try 
    iter (fun str -> let res = f str in
                     if res != None then
                        begin
                          returnRef := res;
                          raise StopIteration
                        end
                      else
                        ())
          s;
    !returnRef
  with StopIteration -> !returnRef

let compute_list_intersection f l =
  let rec compute_list_intersection_ l acc = 
    match l with
      [] -> acc
    | s::ss -> compute_list_intersection_ ss (intersection acc (f s))

  in
  match l with
    [] -> empty
  | s::ss -> compute_list_intersection_ ss (f s)

exception Found of string

let find_in_intersection_except f set1 set2 l =
  let _find_in_intersection_except set1 set2 =
    try 
      iter (fun s -> if (f s) && (mem set2 s)
                              && (List.for_all (fun set -> not (mem set s)) l) then
                      raise (Found s))
           set1;
           None
    with Found s -> Some s
  in
  if cardinal set1 < cardinal set2 then
    _find_in_intersection_except set1 set2
  else
    _find_in_intersection_except set2 set1

let find_in_intersection f set1 set2  =
  find_in_intersection_except f set1 set2 []

(* kate: replace-tabs on; indent-width 2; *)