(***********************************************************)
(*  Copyright (C) 2010 - 2011                              *)
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

open Cset
open Consed.T

module type TypeSet = sig
  type t

  type element

  val empty : t

  val is_empty : t -> bool

  val is_singleton : t -> bool

  val mem : t -> element -> bool

  val add : t -> element -> t

  val remove : t -> element -> t

  val singleton : element -> t

  val from_list : element list -> t

  val to_list : t -> element list

  val choose : t -> element

  val fold : ('a -> element -> 'a) -> 'a -> t -> 'a

  val map_to_list : (element -> 'b) -> t -> 'b list
  
  val fold_non_empty : (element -> 'a) -> ('a -> element -> 'a) -> t -> 'a

  val filter : (element -> bool) -> t -> t

  val iter : (element -> unit) -> t -> unit

  (*  "difference S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 but not to S2 *)
  val difference : t -> t -> t

  (*  "intersection S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 and S2 *)
  val intersection : t -> t -> t

  (*  "union S1 S2" computes the set of all the elements 's' such *)
  (*  that 's' belongs to S1 or S2 *)
  val union : t -> t -> t

  val print_set : t -> unit

  val to_string : t -> string

  val update_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val replace_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val diff_hash : ('a, t) Hashtbl.t -> 'a -> t -> unit

  val update_hash_with_element : ('a, t) Hashtbl.t -> 'a -> element -> unit

  val find_in_hash_no_exception : ('a, t) Hashtbl.t -> 'a -> t

  val contained_in_hash : ('a, t) Hashtbl.t -> 'a -> element -> bool

  val pair_mem_hash : ('a, t) Hashtbl.t -> 'a -> element -> bool

  val cardinal : t -> int

  val for_all : (element -> bool) -> t -> bool

  val exists : (element -> bool) -> t -> bool

  val subset : t -> t -> bool

  val find_object : (element -> 'a option) -> t -> 'a option

  (* computes the intersection of the sets obtained by applying the function given as first argument on all *)
  (* the list elements *)
  val compute_list_intersection : (element -> t) -> element list -> t

  (* 'find_in_intersection f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 and set2 *)
  val find_in_intersection : (element -> bool) -> t -> t -> element option

  (* 'find_in_intersection_except f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 and set2 but none of the sets in l *)
  val find_in_intersection_except : (element -> bool) -> t -> t -> t list -> element option

  (* 'find_in_difference f set1 set2 l' returns an element s iff f(s) is true, *)
  (* s belongs to set1 but not to set2 *)
  val find_in_difference : (element -> bool) -> t -> t -> element option
end

module type Element = sig
  type t
  val to_string : t -> string
end

module Make(Element : Element) = struct

  type element = Element.t

  module ElementS = Consed.Make (struct
                                type t = element
                                let equal str1 str2 = (str1 = str2)
                                let hash = Hashtbl.hash
                                end)

  module ElementCset = Cset.Make(struct type t = element end)

  let elementModuleLookupTable = ElementS.create 127

  type t = ElementCset.t

  let empty = ElementCset.empty

  let is_empty t =
    ElementCset.is_empty t

  let is_singleton t =
    ElementCset.is_singleton t

  let mem t e = ElementCset.mem (ElementS.cons elementModuleLookupTable e) t

  let add t e =
    ElementCset.add (ElementS.cons elementModuleLookupTable e) t

  let remove t e =
    ElementCset.remove (ElementS.cons elementModuleLookupTable e) t

  let singleton s =
    add empty s

  let from_list l =
    List.fold_left (fun set s -> add set s) empty l

  let choose t =
    (ElementCset.choose t).data

  let fold f a t = 
    ElementCset.fold (fun s -> fun v -> f v (s.data)) t a

  let map_to_list f t =
    fold (fun l e -> (f e)::l)
         []
         t
    
  let fold_non_empty firstCase generalCase t =
    match fold (fun res -> fun s -> match res with None -> Some (firstCase s)
                                                | (Some r) -> Some (generalCase r s))
              None t with
      Some r -> r
    | None -> raise Not_found

  let filter f t =
    fold (fun set e -> if f e then
                         add set e
                       else
                         set)
         empty
         t

  let to_list t =
    fold (fun ls s -> s::ls) [] t

  let iter f t = 
    ElementCset.iter (fun s -> f (s.data)) t
  (*
  let addSet t1 t2 =
    iter (fun x -> add t1 x) t2*)

  let difference s1 s2 =
    ElementCset.diff s1 s2

  let intersection s1 s2 =
    ElementCset.inter s1 s2

  let union s1 s2 =
    ElementCset.union s1 s2

  let print_set t =
    print_string "{"; iter (fun x -> print_string ((Element.to_string x) ^ ", ")) t; print_string "}"

  let to_string t =
    "{" ^ (fold (fun str x -> str ^ (Element.to_string x) ^ ", ") "" t) ^ "}"

  let update_hash hash a set =
    if not (Hashtbl.mem hash a) then
      Hashtbl.add hash a set
    else
      let current_set = Hashtbl.find hash a in
      Hashtbl.replace hash a (ElementCset.union current_set set)

  let replace_hash hash a set =
      Hashtbl.replace hash a set

  let diff_hash hash a set =
    if Hashtbl.mem hash a then
      begin
      let current_set = Hashtbl.find hash a in
      Hashtbl.replace hash a (ElementCset.diff current_set set)
      end

  let update_hash_with_element hash (a: 'a) str =
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

  let contained_in_hash hash a e =
    try
      let current_set = Hashtbl.find hash a in
      mem current_set e
    with Not_found ->
      false

  let pair_mem_hash hash a b =
    try
      let set = Hashtbl.find hash a
      in
      mem set b
    with Not_found ->
      false

  let cardinal s =
    ElementCset.cardinal s

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

  exception Found of element

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

  let find_in_difference f set1 set2 =
    try 
      iter (fun s -> if (f s) && not (mem set2 s) then
                        raise (Found s))
            set1;
            None
    with Found s -> Some s

end

(* kate: replace-tabs on; indent-width 2; *)