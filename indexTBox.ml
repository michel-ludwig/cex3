(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010-2012                                *)
(*  Michel Ludwig (michel.ludwig@gmail.com)                *)
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

(* index for quick computations *)
open Owl2
open Consed.T
module O = Ontology
module OPE = ObjectPropertyExpression.Constructor
module CE = ClassExpression.Constructor

(* information stored a concept [C] *)
type concept_record = {
  (* a list of concepts [D] such that axioms [(implies C D)] or            *)
  (* [(equivalent C D)] occur in the ontology                              *)
  mutable c_impl : ClassExpression.Set.t;

  (* a map from concepts [D] to a negatively occurred binary conjunction   *)
  (* [(and C D)] = [(and D C)]; we internally introduce new conjunctions   *)
  (* to deal only with binary conjunctions                                 *)
  c_conj : ClassExpression.t ClassExpression.HMap.t;

  (* a map from an atomic role [r] to a set of positive and a set of       *)
  (* negative concepts [D] such that [C] implies [(all r D)]               *)
  mutable c_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;

  (* a map from an atomic role [r] to a set of positive and a set of       *)
  (* negative concepts [D] such that [C] implies [(all (inv r) D)]         *)
  mutable c_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ObjectProperty.Map.t;
}

(* information stored for an atomic role [r] *)
type role_record = {
  (* a map from concepts [C] to a list of positive and a list of negative  *)
  (* concepts [D] such that [C] implies [(all r D)]                        *)
  r_succ : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.HMap.t;

  (* a map from concepts [C] to a list of positive and a list of negative  *)
  (* concepts [D] such that [C] implies [(all (inv r) D)]                  *)

  r_succi : (ClassExpression.Set.t * ClassExpression.Set.t) ClassExpression.HMap.t;

  (* the set of atomic roles having a common functional supperrole with    *)
  (* [r]                                                                   *)
  mutable r_sibl : ObjectProperty.Set.t;

  (* the set of atomic roles whose inverse has a common functional         *)
  (* supperrole with r                                                     *)
  mutable r_isibl : ObjectProperty.Set.t;

  (* the set of atomic roles that have a common functional superrole with  *)
  (* inverse of r                                                          *)
  mutable r_sibli : ObjectProperty.Set.t;

  (* the set of atomic roles whose inverse has a common functional         *)
  (* superrole with the inverse of r                                       *)
  mutable r_isibli : ObjectProperty.Set.t;
}

let create_concept_record () = {
  c_impl = ClassExpression.Set.empty;
  c_conj = ClassExpression.HMap.create 1;
  c_succ = ObjectProperty.Map.empty;
  c_succi = ObjectProperty.Map.empty;
}

let create_role_record () = {
  r_succ = ClassExpression.HMap.create 1;
  r_succi = ClassExpression.HMap.create 1;
  r_sibl = ObjectProperty.Set.empty;
  r_isibl = ObjectProperty.Set.empty;
  r_sibli = ObjectProperty.Set.empty;
  r_isibli = ObjectProperty.Set.empty;
}

let empty_concept_record = create_concept_record ()
let empty_role_record = create_role_record ()

type t = {
  hcr : concept_record ClassExpression.HMap.t;
  hrr : role_record ObjectProperty.HMap.t;
}

(* required by the interface *)
let find_concept_record index c =
  try ClassExpression.HMap.find index.hcr c
  with Not_found -> empty_concept_record
;;
let find_role_record index r =
  try ObjectProperty.HMap.find index.hrr r
  with Not_found -> empty_role_record
;;

let cons_concept_record index c =
  try ClassExpression.HMap.find index c
  with Not_found ->
      let cr = create_concept_record () in
      ClassExpression.HMap.add index c cr;
      cr
;;

let cons_role_record index r =
  try ObjectProperty.HMap.find index r
  with Not_found ->
      let rr = create_role_record () in
      ObjectProperty.HMap.add index r rr;
      rr
;;

(* functions for adding bindings to records in concept and role indexes *)
let add_c_impl index c d =
  let cr = cons_concept_record index c in
  cr.c_impl <- ClassExpression.Set.add d cr.c_impl
;;

let add_c_conj index c d e =
  let cr = cons_concept_record index c in
  begin try if (ClassExpression.HMap.find cr.c_conj d) != e
    then invalid_arg ("IndexTBox.add_c_conj" ^ (Owl2IO.str_of_ClassExpression d))
  with Not_found -> () end;
  ClassExpression.HMap.replace cr.c_conj d e
;;

(* below [p] is a polarity: [true = positive], [false = negative] *)
let add_c_succ index c r d p =
  let cr = cons_concept_record index c in
  cr.c_succ <- ObjectProperty.Map.process r (function
      | None -> Some (
            if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
            else ClassExpression.Set.empty, ClassExpression.Set.singleton d
          )
      | Some (sp, sn) -> Some (
            if p then ClassExpression.Set.add d sp, sn
            else sp, ClassExpression.Set.add d sn
          )
    ) cr.c_succ
;;

let add_c_succi index c r d p =
  let cr = cons_concept_record index c in
  cr.c_succi <- ObjectProperty.Map.process r (function
      | None -> Some (
            if p then ClassExpression.Set.singleton d, ClassExpression.Set.empty
            else ClassExpression.Set.empty, ClassExpression.Set.singleton d
          )
      | Some (sp, sn) -> Some (
            if p then ClassExpression.Set.add d sp, sn
            else sp, ClassExpression.Set.add d sn
          )
    ) cr.c_succi
;;

let add_r_succ index r c d p =
  let rr = cons_role_record index r in
  let sp, sn = try ClassExpression.HMap.find rr.r_succ c
    with Not_found -> ClassExpression.Set.empty, ClassExpression.Set.empty
  in
  ClassExpression.HMap.replace rr.r_succ c
    (if p then ClassExpression.Set.add d sp, sn
      else sp, ClassExpression.Set.add d sn)
;;

let add_r_succi index r c d p =
  let rr = cons_role_record index r in
  let sp, sn = try ClassExpression.HMap.find rr.r_succi c
    with Not_found -> ClassExpression.Set.empty, ClassExpression.Set.empty
  in
  ClassExpression.HMap.replace rr.r_succi c
    (if p then ClassExpression.Set.add d sp, sn
      else sp, ClassExpression.Set.add d sn)
;;

let add_r_sibl index r s =
  let rr = cons_role_record index r in
  rr.r_sibl <- ObjectProperty.Set.add s rr.r_sibl
;;

let union_r_sibl index r ss =
  let rr = cons_role_record index r in
  rr.r_sibl <- ObjectProperty.Set.union ss rr.r_sibl
;;

let add_r_isibl index r s =
  let rr = cons_role_record index r in
  rr.r_isibl <- ObjectProperty.Set.add s rr.r_isibl
;;

let union_r_isibl index r ss =
  let rr = cons_role_record index r in
  rr.r_isibl <- ObjectProperty.Set.union ss rr.r_isibl
;;

let add_r_sibli index r s =
  let rr = cons_role_record index r in
  rr.r_sibli <- ObjectProperty.Set.add s rr.r_sibli
;;

let union_r_sibli index r ss =
  let rr = cons_role_record index r in
  rr.r_sibli <- ObjectProperty.Set.union ss rr.r_sibli
;;

let add_r_isibli index r s =
  let rr = cons_role_record index r in
  rr.r_isibli <- ObjectProperty.Set.add s rr.r_isibli
;;

let union_r_isibli index r ss =
  let rr = cons_role_record index r in
  rr.r_isibli <- ObjectProperty.Set.union ss rr.r_isibli
;;

let add_succ c_index r_index c r d p =
  add_c_succ c_index c r d p;
  add_r_succ r_index r c d p;
;;

let add_succi c_index r_index c r d p =
  add_c_succi c_index c r d p;
  add_r_succi r_index r c d p;
;;

let estimated_concept_index_size ont =
  O.total_SubClassOf ont + O.total_ObjectIntersectionOf ont

let estimated_role_index_size ont =
  Polarity.Counter.get_pos (O.count_ObjectSomeValuesFrom ont)

(* initialize the index from an ontology [ont] *)
let init ont =

  let concept_index = ClassExpression.HMap.create (estimated_concept_index_size ont) in
  let role_index = ObjectProperty.HMap.create (estimated_role_index_size ont) in

  let module A = ClassExpressionAxiom.Constructor in
  O.iter_record_ClassExpressionAxiom (fun ax -> match ax.data with
          | A.SubClassOf (ce1, ce2) ->
              add_c_impl concept_index ce1 ce2
          | A.EquivalentClasses ce_lst ->
              begin match ce_lst with
                | ce_c :: ce_rest ->
                    List.iter (fun ce ->
                            add_c_impl concept_index ce ce_c;
                            add_c_impl concept_index ce_c ce
                      ) ce_rest
                | _ -> invalid_arg "indexTBox.init"
              end
          | A.DisjointClasses _ -> ()
          | A.DisjointUnion _ -> ()
    ) ont;

  (* We call an atomic role [r] relevant if some concept [(some r C)] or   *)
  (* [(some (inv r) C)] occurs positively. We compute the set of relevant  *)
  (* atomic roles.                                                         *)

  let ar_ex = ref ObjectProperty.Set.empty in

  O.iter_record_ComplexClassExpression (fun c p ->
          if Polarity.Counter.get_pos p > 0 then
            match c.data with
            | CE.ObjectSomeValuesFrom (r, c1) -> (
                  match r.data with
                  | OPE.ObjectProperty ar -> ar_ex := ObjectProperty.Set.add ar !ar_ex
                  | OPE.InverseObjectProperty ar -> ar_ex := ObjectProperty.Set.add ar !ar_ex
                )
            | _ -> ()
    ) ont;

  (* insert propagation rules for bottom into the index *)
  if O.has_positive_Nothing ont || O.has_positive_ComplementOf ont then (
    let bot = O.cons_ClassExpression ont
        (ClassExpression.Constructor.Class Class.Constructor.Nothing) in
    ObjectProperty.Set.iter ( fun ar ->
        (* [bot] implies [(all ar bot)] and [(all (inv ar) bot)] *)
            add_succ concept_index role_index bot ar bot true;
            add_succi concept_index role_index bot ar bot true;
      ) !ar_ex;
  );

  let rt = ReasonerRBox.saturate ont in

  (* iterating over relevant atomic subproperties of [h]: iterate [fs]     *)
  (* over subproperties; iterate [fsi] over inversed subproperties; [fst]  *)
  (* over subtransitive subproperties, and [fsti] over subtransitive       *)
  (* inversed subproperties.                                               *)
  let iter_sa_si_sta_sti h fs fsi fst fsti =
    (* computing subproperties and subtransitive subproperties of [h] *)
    let (h_sa, h_si), (h_sta, h_sti) = (
        match h.data with
        | OPE.ObjectProperty ar ->
            ReasonerRBox.find_subproperties rt ar,
            ReasonerRBox.find_sub_trans rt ar
        | OPE.InverseObjectProperty ar ->
            Brole.Set.inv (ReasonerRBox.find_subproperties rt ar),
            Brole.Set.inv (ReasonerRBox.find_sub_trans rt ar)
      ) in
    ObjectProperty.Set.iter2 (fun ar -> fs ar) !ar_ex h_sa;
    ObjectProperty.Set.iter2 (fun ar -> fsi ar) !ar_ex h_si;
    ObjectProperty.Set.iter2 (fun ar -> fst ar) !ar_ex h_sta;
    ObjectProperty.Set.iter2 (fun ar -> fsti ar) !ar_ex h_sti;
  in

  O.iter_record_ComplexClassExpression (fun c p ->
          match c.data with
          | CE.ObjectIntersectionOf (c1, c2) when Polarity.Counter.get_neg p > 0 ->
              add_c_conj concept_index c1 c2 c;
              add_c_conj concept_index c2 c1 c;
          | CE.ObjectUnionOf (c1, c2) when Polarity.Counter.get_neg p > 0 ->
              add_c_impl concept_index c1 c;
              add_c_impl concept_index c2 c;
          | CE.ObjectComplementOf d when Polarity.Counter.get_pos p > 0 ->
              let bot = O.cons_ClassExpression ont
                  (ClassExpression.Constructor.Class Class.Constructor.Nothing) in
              add_c_conj concept_index c d bot;
              add_c_conj concept_index d c bot;
          | CE.ObjectSomeValuesFrom (h, d) when Polarity.Counter.get_neg p > 0 ->
              iter_sa_si_sta_sti h
                (* if [r] implies [h] then [d] implies [(all (inv r) c)] *)
                (fun r -> add_succi concept_index role_index d r c false)
                (* if [r] implies [(inv h)] then [d] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index d r c false)
                (* if [r] subtr [h] then [c] implies [(all (inv r) c)] *)
                (fun r -> add_succi concept_index role_index c r c false)
                (* if [r] subtr [(inv h)] then [c] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index c r c false);
          | CE.ObjectAllValuesFrom (h, d) when Polarity.Counter.get_pos p > 0 ->
              iter_sa_si_sta_sti h
                (* if [r] implies [h] then [c] implies [(all r d)] *)
                (fun r -> add_succ concept_index role_index c r d true)
                (* if [r] implies [(inv h)] then [c] implies [(all (inv r) *)
                (* d)]                                                     *)
                (fun r -> add_succi concept_index role_index c r d true)
                (* if [r] subtr [h] then [c] implies [(all r c)] *)
                (fun r -> add_succ concept_index role_index c r c true)
                (* if [r] subtr [(inv h)] then [c] implies [(all (inv r)   *)
                (* c)]                                                     *)
                (fun r -> add_succi concept_index role_index c r c true);
          | _ -> (); (**! to be extended for other constructors *)
    ) ont;

  (* computing functional superroles and siblings *)
  (*|  let m = ref ObjectProperty.Map.empty in*)
  ObjectProperty.Set.iter ( fun af ->
          let sibl_f = ref ObjectProperty.Set.empty in
          let isibl_f = ref ObjectProperty.Set.empty in
          let af_sa, af_si = ReasonerRBox.find_subproperties rt af in
          ObjectProperty.Set.iter2
            (fun ar -> sibl_f := ObjectProperty.Set.add ar !sibl_f)
            !ar_ex af_sa;
          ObjectProperty.Set.iter2
            (fun ar -> isibl_f := ObjectProperty.Set.add ar !isibl_f)
            !ar_ex af_si;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibl role_index ar !sibl_f;
                  union_r_isibl role_index ar !isibl_f;
            ) !sibl_f;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibli role_index ar !sibl_f;
                  union_r_isibli role_index ar !isibl_f;
            ) !isibl_f;
    ) (ReasonerRBox.find_funct_roles rt);

  ObjectProperty.Set.iter ( fun af ->
          let sibl_f = ref ObjectProperty.Set.empty in
          let isibl_f = ref ObjectProperty.Set.empty in
          let af_sa, af_si = ReasonerRBox.find_subproperties rt af in
          ObjectProperty.Set.iter2
            (fun ar -> sibl_f := ObjectProperty.Set.add ar !sibl_f)
            !ar_ex af_si;
          ObjectProperty.Set.iter2
            (fun ar -> isibl_f := ObjectProperty.Set.add ar !isibl_f)
            !ar_ex af_sa;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibl role_index ar !sibl_f;
                  union_r_isibl role_index ar !isibl_f;
            ) !sibl_f;
          ObjectProperty.Set.iter (fun ar ->
                  union_r_sibli role_index ar !sibl_f;
                  union_r_isibli role_index ar !isibl_f;
            ) !isibl_f;
    ) (ReasonerRBox.find_inv_funct_roles rt);
  (*|  Gc.compact ();  (* <- slow but useful in the long run *)*)
  {
    hcr = concept_index;
    hrr = role_index;
  }
;;

let print_statistics index =
  Printf.printf "Concept index contains:\n";
  Printf.printf "-----------------------\n";
  Printf.printf "\tConcept records: \t %n\n" (ClassExpression.HMap.length index.hcr);
  Printf.printf "\tRole records: \t %n\n" (ObjectProperty.HMap.length index.hrr)
;;

let get_concept_rhs_in_terminology t ont occurrenceHash cname =
  let expSet = (find_concept_record t (O.concept_name_class_expression ont cname)).c_impl in
  if Cset.cardinal expSet = 0 then
    begin
    raise Not_found
    end;
  (* we have to be a little careful here: for example, for an ontology          *)
  (* (equivalent A B) and (equivalent B (and C D)) and 'cname' = 'B', 'expSet'  *)
  (* would contain A, (and C D), but we only want (and C D).                    *)
  let redExpSet = Cset.fold (fun exp set ->
                              if not (is_concept_name_class_expression exp) then
                                set
                              else
                                let dname = begin
                                              match (concept_names_in_class_expression exp) with
                                                s::[] -> s
                                              | _ -> failwith "Wrong class expression"
                                            end
                                in
                                let occurrenceSet = begin
                                                      (try
                                                         Hashtbl.find occurrenceHash dname
                                                       with Not_found -> Types.StringSet.empty)
                                                    end
                                in
                                  if Types.StringSet.mem occurrenceSet cname then
                                    Cset.remove exp set
                                  else
                                    set)
                             expSet expSet

  in
  assert ((Cset.cardinal redExpSet) = 1);
  Cset.choose redExpSet

let get_rhs_of_definition_in_terminology t ont occurrenceHash cname =
  match (get_concept_rhs_in_terminology t ont occurrenceHash cname).data with
    ClassExpression.Constructor.ObjectSomeValuesFrom (rexpf, cexp)->
        (match rexpf.data with
          ObjectPropertyExpression.Constructor.ObjectProperty rexp ->
            (match (rexp.data, cexp.data) with
              (ObjectProperty.Constructor.IRI r_iri,
              ClassExpression.Constructor.Class cl) ->
              (match cl with Class.Constructor.IRI iri_aname ->
                 (match r_iri.data with IRI.Constructor.IRI r ->
                 (match iri_aname.data with IRI.Constructor.IRI aname ->
                   (r, aname)))
                | _ -> invalid_arg ("Not of the form (some r A) for " ^ cname))
              | _ -> invalid_arg ("Not of the form (some r A) for " ^ cname))
          | _ -> invalid_arg ("Not of the form (some r A) for " ^ cname))
   | _ -> invalid_arg ("Not of the form (some r A) for " ^ cname)

let get_role_successor_in_terminology t ont occurrenceHash cname =
  try
    match (get_concept_rhs_in_terminology t ont occurrenceHash cname).data with
      ClassExpression.Constructor.ObjectSomeValuesFrom (rexpf, _)->
        (match rexpf.data with
          ObjectPropertyExpression.Constructor.ObjectProperty rexp ->
            (match rexp.data with
                ObjectProperty.Constructor.IRI r_iri ->
                  (match r_iri.data with IRI.Constructor.IRI r -> Some r)
            | _ -> None)
        | _ ->  None)
    | _ -> None
  with Not_found -> None (* 'cname' is primitive *)

let is_primitive t leftHandSideSet cname =
  not (Types.StringSet.mem leftHandSideSet cname)

let is_pseudo_primitive _ occurrenceHash cname =
  not (Hashtbl.mem occurrenceHash cname)

let is_conjunctive t ont occurrenceHash cname =
  if not (Hashtbl.mem occurrenceHash cname) then
    false (* cname is pseudo-primitive *)
  else
    begin
      let rhs = get_concept_rhs_in_terminology t ont occurrenceHash cname
      in
      if is_atomic rhs then
        true
      else
        match rhs.data with
          | ClassExpression.Constructor.Class _ -> true
          | ClassExpression.Constructor.ObjectIntersectionOf _ -> true
          | _ -> false
    end

let normalised_get_conjunctive_rhs t ont occurrenceHash leftHandSideSet cname =
  if is_primitive t leftHandSideSet cname then
    None (* cname is primitive *)
  else
    begin
      let rhs = get_concept_rhs_in_terminology t ont occurrenceHash cname in
      match rhs.data with
        | ClassExpression.Constructor.Class _ -> Some (concept_names_in_class_expression rhs)
        | ClassExpression.Constructor.ObjectIntersectionOf _ -> Some (concept_names_in_class_expression rhs)
        | _ -> None
    end

let is_exists_definition t ont occurrenceHash cname =
  try
    match (get_concept_rhs_in_terminology t ont occurrenceHash cname).data with
      | ClassExpression.Constructor.ObjectSomeValuesFrom (rexpf, cexp)->
          (match rexpf.data with
            ObjectPropertyExpression.Constructor.ObjectProperty rexp ->
              (match (rexp.data, cexp.data) with
                (ObjectProperty.Constructor.IRI r,
                ClassExpression.Constructor.Class cl) ->
                (match cl with
                  | Class.Constructor.IRI _ -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false
  with Not_found -> false

(* kate: replace-tabs on; indent-width 2; *)
