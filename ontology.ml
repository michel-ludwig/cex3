(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010-2014                                *)
(*  Michel Ludwig (michel@tcs.inf.tu-dresden.de)           *)
(*  University of Liverpool, and                           *)
(*  TU Dresden                                             *)
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
open Consed.T
open Printf
open Types
open Utilities

module PC = Polarity.Counter
module F = Format

(**======================= ontology ========================**)

module OrderedClass =
struct
  type t = Class.t
  let compare c1 c2 = String.compare (Class.str_of c1) (Class.str_of c2)
end

module ClassMap = Map.Make (OrderedClass)
module ClassSet = Set.Make (OrderedClass)

type t = {
  cons_IRI : IRI.Cons.t;
  cons_NodeID : NodeID.Cons.t;
	cons_ObjectProperty : ObjectProperty.Cons.t;
  cons_Literal : Literal.Cons.t;
  cons_ObjectPropertyExpression : ObjectPropertyExpression.Cons.t;
  cons_DataPropertyExpression : DataPropertyExpression.Cons.t;
  cons_DataRange : DataRange.Cons.t;
  cons_ClassExpression : ClassExpression.Cons.t;
  cons_ClassExpressionAxiom : ClassExpressionAxiom.Cons.t;
  cons_ObjectPropertyAxiom : ObjectPropertyAxiom.Cons.t;
  cons_DataPropertyAxiom : DataPropertyAxiom.Cons.t;
  cons_DatatypeDefinition : DatatypeDefinition.Cons.t;
  cons_Key : Key.Cons.t;
  cons_Assertion : Assertion.Cons.t;
  cons_AnnotationSubject : AnnotationSubject.Cons.t;
  cons_AnnotationValue : AnnotationValue.Cons.t;
  cons_Annotation : Annotation.Cons.t;
  cons_AnnotationAxiom : AnnotationAxiom.Cons.t;
  
  (* ========== signature ========== *)
  mutable record_ObjectProperty : int ObjectProperty.Map.t;
  mutable count_ObjectPropertyIRI : int;
  mutable count_TopObjectProperty : PC.t;
  mutable count_BottomObjectProperty: PC.t;
  
  mutable record_Class : int ClassMap.t;
  mutable count_ClassIRI : int;
  mutable count_Thing : PC.t;
  mutable count_Nothing: PC.t;
  
  mutable record_Individual : int Individual.Map.t;
  mutable count_IndividualIRI : int;
  
  (* ========== structure ========== *)
  mutable record_ComplexObjectPropertyExpression : PC.t ObjectPropertyExpression.Map.t;
  mutable count_InverseObjectProperty : PC.t;
  
  mutable record_ComplexClassExpression : PC.t ClassExpression.Map.t;
  mutable count_ObjectIntersectionOf : PC.t;
  mutable count_ObjectUnionOf : PC.t;
  mutable count_ObjectComplementOf : PC.t;
  mutable count_ObjectOneOf : PC.t;
  mutable count_ObjectSomeValuesFrom : PC.t;
  mutable count_ObjectAllValuesFrom : PC.t;
  mutable count_ObjectHasValue : PC.t;
  mutable count_ObjectHasSelf : PC.t;
  mutable count_ObjectMinCardinality : PC.t;
  mutable count_ObjectMaxCardinality : PC.t;
  mutable count_ObjectExactCardinality : PC.t;
  mutable count_DataSomeValuesFrom : PC.t;
  mutable count_DataAllValuesFrom : PC.t;
  mutable count_DataHasValue : PC.t;
  mutable count_DataMinCardinality : PC.t;
  mutable count_DataMaxCardinality : PC.t;
  mutable count_DataExactCardinality : PC.t;
  
  (* ========== axioms ========== *)
  mutable record_ObjectPropertyAxiom : ObjectPropertyAxiom.Set.t;
  mutable count_SubPropertyOf : int;
  mutable count_EquivalentProperties : int;
  mutable count_InverseProperties : int;
  mutable count_FunctionalProperty : int;
  mutable count_InverseFunctionalProperty : int;
  mutable count_TransitiveProperty : int;
  mutable count_RoleComposition : int;
  
  mutable record_ClassExpressionAxiom : ClassExpressionAxiom.Set.t;
  mutable count_SubClassOf : int;
  mutable count_EquivalentClasses : int;
  
  mutable record_Assertion : Assertion.Set.t;
  mutable count_ClassAssertion : int;
  mutable count_PropertyAssertion : int;

  mutable conceptNameSet : StringSet.t;
  mutable originalConceptNameSet : StringSet.t;
  mutable roleNameSet : StringSet.t;
  mutable domainConceptNameSet : StringSet.t;
  mutable rangeConceptNameSet : StringSet.t;

  mutable preDomainHash : (string, StringSet.t) Hashtbl.t;
  mutable preRangeHash : (string, StringSet.t) Hashtbl.t;

  mutable domainRangeAxioms : ClassExpressionAxiom.Set.t;
}

(**=================== initialization ======================**)

let create () =	{

  cons_IRI = IRI.Cons.create 127;
	cons_NodeID = NodeID.Cons.create 127;
  cons_ObjectProperty = ObjectProperty.Cons.create 127;
  cons_Literal = Literal.Cons.create 127;
  cons_ObjectPropertyExpression = ObjectPropertyExpression.Cons.create 127;
  cons_DataPropertyExpression = DataPropertyExpression.Cons.create 127;
  cons_DataRange = DataRange.Cons.create 127;
  cons_ClassExpression = ClassExpression.Cons.create 127;
  cons_ClassExpressionAxiom = ClassExpressionAxiom.Cons.create 127;
  cons_ObjectPropertyAxiom = ObjectPropertyAxiom.Cons.create 127;
  cons_DataPropertyAxiom = DataPropertyAxiom.Cons.create 127;
  cons_DatatypeDefinition = DatatypeDefinition.Cons.create 127;
  cons_Key = Key.Cons.create 127;
  cons_Assertion = Assertion.Cons.create 127;
  cons_AnnotationSubject = AnnotationSubject.Cons.create 127;
  cons_AnnotationValue = AnnotationValue.Cons.create 127;
  cons_Annotation = Annotation.Cons.create 127;
  cons_AnnotationAxiom = AnnotationAxiom.Cons.create 127;
  
  (* ========== signature ========== *)
  record_ObjectProperty = ObjectProperty.Map.empty;
  count_ObjectPropertyIRI = 0;
  count_TopObjectProperty = PC.zero;
  count_BottomObjectProperty = PC.zero;
  
  record_Class = ClassMap.empty;
  count_ClassIRI = 0;
  count_Thing = PC.zero;
  count_Nothing = PC.zero;
  
  record_Individual = Individual.Map.empty;
  count_IndividualIRI = 0;
  
  (* ========== structure ========== *)
  record_ComplexObjectPropertyExpression = ObjectPropertyExpression.Map.empty;
  count_InverseObjectProperty = PC.zero;
  
  record_ComplexClassExpression = ClassExpression.Map.empty;
  count_ObjectIntersectionOf = PC.zero;
  count_ObjectUnionOf = PC.zero;
  count_ObjectComplementOf = PC.zero;
  count_ObjectOneOf = PC.zero;
  count_ObjectSomeValuesFrom = PC.zero;
  count_ObjectAllValuesFrom = PC.zero;
  count_ObjectHasValue = PC.zero;
  count_ObjectHasSelf = PC.zero;
  count_ObjectMinCardinality = PC.zero;
  count_ObjectMaxCardinality = PC.zero;
  count_ObjectExactCardinality = PC.zero;
  count_DataSomeValuesFrom = PC.zero;
  count_DataAllValuesFrom = PC.zero;
  count_DataHasValue = PC.zero;
  count_DataMinCardinality = PC.zero;
  count_DataMaxCardinality = PC.zero;
  count_DataExactCardinality = PC.zero;
  
  (* ========== axioms ========== *)
  record_ObjectPropertyAxiom = ObjectPropertyAxiom.Set.empty;
  count_SubPropertyOf = 0;
  count_EquivalentProperties = 0;
  count_InverseProperties = 0;
  count_FunctionalProperty = 0;
  count_InverseFunctionalProperty = 0;
  count_TransitiveProperty = 0;
  count_RoleComposition = 0;
  
  record_ClassExpressionAxiom = ClassExpressionAxiom.Set.empty;
  count_SubClassOf = 0;
  count_EquivalentClasses = 0;
  
  record_Assertion = Assertion.Set.empty;
  count_ClassAssertion = 0;
  count_PropertyAssertion = 0;

  conceptNameSet = StringSet.empty;
  originalConceptNameSet = StringSet.empty;
  roleNameSet = StringSet.empty;
  domainConceptNameSet = StringSet.empty;
  rangeConceptNameSet = StringSet.empty;

  preDomainHash = Hashtbl.create 50;
  preRangeHash = Hashtbl.create 50;

  domainRangeAxioms = ClassExpressionAxiom.Set.empty;
}

(**======================= consing =========================**)
let cons_IRI ont id = IRI.cons ont.cons_IRI id
let cons_NodeID ont id = NodeID.cons ont.cons_NodeID id
let cons_ObjectProperty ont op = ObjectProperty.cons ont.cons_ObjectProperty op
let cons_Literal ont lt = Literal.cons ont.cons_Literal lt
let cons_ObjectPropertyExpression ont ope = ObjectPropertyExpression.cons ont.cons_ObjectPropertyExpression ope
let cons_DataPropertyExpression ont dpe = DataPropertyExpression.cons ont.cons_DataPropertyExpression dpe
let cons_DataRange ont dr = DataRange.cons ont.cons_DataRange dr
let cons_ClassExpression ont ce = ClassExpression.cons ont.cons_ClassExpression ce
let cons_ClassExpressionAxiom ont ax = ClassExpressionAxiom.cons ont.cons_ClassExpressionAxiom ax
let cons_ObjectPropertyAxiom ont ax = ObjectPropertyAxiom.cons ont.cons_ObjectPropertyAxiom ax
let cons_DataPropertyAxiom ont ax = DataPropertyAxiom.cons ont.cons_DataPropertyAxiom ax
let cons_DatatypeDefinition ont ax = DatatypeDefinition.cons ont.cons_DatatypeDefinition ax
let cons_Key ont ax = Key.cons ont.cons_Key ax
let cons_Assertion ont ax = Assertion.cons ont.cons_Assertion ax
let cons_AnnotationSubject ont ax = AnnotationSubject.cons ont.cons_AnnotationSubject ax
let cons_AnnotationValue ont ax = AnnotationValue.cons ont.cons_AnnotationValue ax
let cons_Annotation ont ax = Annotation.cons ont.cons_Annotation ax
let cons_AnnotationAxiom ont ax = AnnotationAxiom.cons ont.cons_AnnotationAxiom ax

(**====================== iterators ========================**)

let iter_record_ObjectProperty f ont =
  ObjectProperty.Map.iter f ont.record_ObjectProperty
let iter_record_Class f ont =
  ClassMap.iter f ont.record_Class
let iter_record_Individual f ont =
  Individual.Map.iter f ont.record_Individual

let iter_record_ComplexObjectPropertyExpression f ont =
  ObjectPropertyExpression.Map.iter f ont.record_ComplexObjectPropertyExpression
let iter_record_ComplexClassExpression f ont =
  ClassExpression.Map.iter f ont.record_ComplexClassExpression

let iter_record_ObjectPropertyAxiom f ont =
  ObjectPropertyAxiom.Set.iter f ont.record_ObjectPropertyAxiom
let iter_record_ClassExpressionAxiom f ont =
  ClassExpressionAxiom.Set.iter f ont.record_ClassExpressionAxiom
let iter_record_Assertion f ont =
  Assertion.Set.iter f ont.record_Assertion

(**============== statistical information ================**)

let has_positive_Nothing ont =
  PC.get_pos ont.count_Nothing > 0
let has_positive_ComplementOf ont =
  PC.get_pos ont.count_ObjectComplementOf > 0
let has_negative_Thing ont =
  PC.get_neg ont.count_Thing > 0

let total_ObjectPropertyIRI ont = ont.count_ObjectPropertyIRI
let total_ClassIRI ont = ont.count_ClassIRI
let total_IndividualIRI ont = ont.count_IndividualIRI

let count_TopObjectProperty ont = ont.count_TopObjectProperty
let count_BottomObjectProperty ont = ont.count_BottomObjectProperty
let count_Thing ont = ont.count_Thing
let count_Nothing ont = ont.count_Nothing

let count_InverseObjectProperty ont = ont.count_InverseObjectProperty
let total_InverseObjectProperty ont = PC.get_total ont.count_InverseObjectProperty

let count_ObjectIntersectionOf ont = ont.count_ObjectIntersectionOf
let count_ObjectUnionOf ont = ont.count_ObjectUnionOf
let count_ObjectComplementOf ont = ont.count_ObjectComplementOf
let count_ObjectOneOf ont = ont.count_ObjectOneOf
let count_ObjectSomeValuesFrom ont = ont.count_ObjectSomeValuesFrom
let count_ObjectAllValuesFrom ont = ont.count_ObjectAllValuesFrom
let count_ObjectHasValue ont = ont.count_ObjectHasValue
let count_ObjectHasSelf ont = ont.count_ObjectHasSelf
let count_ObjectMinCardinality ont = ont.count_ObjectMinCardinality
let count_ObjectMaxCardinality ont = ont.count_ObjectMaxCardinality
let count_ObjectExactCardinality ont = ont.count_ObjectExactCardinality
let count_DataSomeValuesFrom ont = ont.count_DataSomeValuesFrom
let count_DataAllValuesFrom ont = ont.count_DataAllValuesFrom
let count_DataHasValue ont = ont.count_DataHasValue
let count_DataMinCardinality ont = ont.count_DataMinCardinality
let count_DataMaxCardinality ont = ont.count_DataMaxCardinality
let count_DataExactCardinality ont = ont.count_DataExactCardinality

let total_ObjectIntersectionOf ont = PC.get_total ont.count_ObjectIntersectionOf
let total_ObjectUnionOf ont = PC.get_total ont.count_ObjectUnionOf
let total_ObjectComplementOf ont = PC.get_total ont.count_ObjectComplementOf
let total_ObjectOneOf ont = PC.get_total ont.count_ObjectOneOf
let total_ObjectSomeValuesFrom ont = PC.get_total ont.count_ObjectSomeValuesFrom
let total_ObjectAllValuesFrom ont = PC.get_total ont.count_ObjectAllValuesFrom
let total_ObjectHasValue ont = PC.get_total ont.count_ObjectHasValue
let total_ObjectHasSelf ont = PC.get_total ont.count_ObjectHasSelf
let total_ObjectMinCardinality ont = PC.get_total ont.count_ObjectMinCardinality
let total_ObjectMaxCardinality ont = PC.get_total ont.count_ObjectMaxCardinality
let total_ObjectExactCardinality ont = PC.get_total ont.count_ObjectExactCardinality
let total_DataSomeValuesFrom ont = PC.get_total ont.count_DataSomeValuesFrom
let total_DataAllValuesFrom ont = PC.get_total ont.count_DataAllValuesFrom
let total_DataHasValue ont = PC.get_total ont.count_DataHasValue
let total_DataMinCardinality ont = PC.get_total ont.count_DataMinCardinality
let total_DataMaxCardinality ont = PC.get_total ont.count_DataMaxCardinality
let total_DataExactCardinality ont = PC.get_total ont.count_DataExactCardinality

let total_SubPropertyOf ont = ont.count_SubPropertyOf
let total_EquivalentProperties ont = ont.count_EquivalentProperties
let total_InverseProperties ont = ont.count_InverseProperties
let total_FunctionalProperty ont = ont.count_FunctionalProperty
let total_InverseFunctionalProperty ont = ont.count_InverseFunctionalProperty
let total_TransitiveProperty ont = ont.count_TransitiveProperty
let total_RoleComposition ont = ont.count_RoleComposition
let total_SubClassOf ont = ont.count_SubClassOf
let total_EquivalentClasses ont = ont.count_EquivalentClasses
let total_ClassAssertion ont = ont.count_ClassAssertion
let total_PropertyAssertion ont = ont.count_PropertyAssertion

let get_domain_concept_names ont =
  ont.domainConceptNameSet

let get_range_concept_names ont =
  ont.rangeConceptNameSet

(**==== insertion, computation of polarities and stats =====**)

let add_ObjectProperty_pc ont op pc =
  let module M = ObjectProperty.Map in
  let module C = ObjectProperty.Constructor in
  match op.data with
  | C.IRI iri -> (* adding only iris in the record *)
      (match iri.data with IRI.Constructor.IRI r -> 
        ont.roleNameSet <- StringSet.add ont.roleNameSet r;
        ont.domainConceptNameSet <- StringSet.add ont.domainConceptNameSet (RoleMapping.map_role_to_domain_binding r);
        ont.rangeConceptNameSet <- StringSet.add ont.rangeConceptNameSet (RoleMapping.map_role_to_range_binding r));
      ont.record_ObjectProperty <- M.add op
        (let count = try M.find op ont.record_ObjectProperty
            with Not_found ->
                ont.count_ObjectPropertyIRI <-
                succ ont.count_ObjectPropertyIRI;
                0
          in count + PC.get_total pc
        ) ont.record_ObjectProperty;
  | C.TopObjectProperty -> ont.count_TopObjectProperty <-
      PC.sum ont.count_TopObjectProperty pc
  | C.BottomObjectProperty -> ont.count_BottomObjectProperty <-
      PC.sum ont.count_BottomObjectProperty pc
;;

let add_Class_pc ont c pc =
  let module M = ClassMap in
  let module C = Class.Constructor in
  match c with
  | C.IRI iri -> (* adding only iris in the record *)
      (match iri.data with IRI.Constructor.IRI cname -> 
        ont.conceptNameSet <- StringSet.add ont.conceptNameSet cname;
        if is_original_concept_name cname then
          ont.originalConceptNameSet <- StringSet.add ont.originalConceptNameSet cname);
      ont.record_Class <- M.add c
        (let count = try M.find c ont.record_Class
            with Not_found ->
                ont.count_ClassIRI <-
                succ ont.count_ClassIRI;
                0
          in count + PC.get_total pc
        ) ont.record_Class
  | C.Thing -> ont.count_Thing <-
      PC.sum ont.count_Thing pc
  | C.Nothing -> ont.count_Nothing <-
      PC.sum ont.count_Nothing pc
;;

let add_Individual_pc ont i pc =
  let module M = Individual.Map in
  let module C = Individual.Constructor in
  (* adding only iris in the record *)
  ont.record_Individual <- M.add i
    (let count = try M.find i ont.record_Individual
        with Not_found ->
            ont.count_IndividualIRI <-
            succ ont.count_IndividualIRI;
            0
      in succ count
    ) ont.record_Individual
;;

let rec add_ObjectPropertyExpression_pc ont ope pc =
  let module M = ObjectPropertyExpression.Map in
  let module C = ObjectPropertyExpression.Constructor in
  begin match ope.data with
    | C.ObjectProperty _ -> ()
    | _ -> ont.record_ComplexObjectPropertyExpression <-
        M.process ope (function
            | Some pc_old -> Some (PC.sum pc_old pc)
            | None -> Some pc
          ) ont.record_ComplexObjectPropertyExpression;
  end;
  propagate_ObjectPropertyExpression_pc ont ope pc
and propagate_ObjectPropertyExpression_pc ont ope pc =
  let module M = ObjectPropertyExpression.Map in
  let module C = ObjectPropertyExpression.Constructor in
  match ope.data with
  | C.ObjectProperty op -> add_ObjectProperty_pc ont op pc
  | C.InverseObjectProperty op -> add_ObjectProperty_pc ont op pc;
      ont.count_InverseObjectProperty <-
      PC.sum ont.count_InverseObjectProperty pc
;;

let rec add_ClassExpression_pc ont ce pc =
  let module M = ClassExpression.Map in
  let module C = ClassExpression.Constructor in
  begin match ce.data with
    | C.Class _ -> ()
    | _ -> ont.record_ComplexClassExpression <-
        M.process ce (function
            | Some pc_old -> Some (PC.sum pc_old pc)
            | None -> Some pc
          ) ont.record_ComplexClassExpression;
  end;
  propagate_ClassExpression_pc ont ce pc
and propagate_ClassExpression_pc ont ce pc =
  let module C = ClassExpression.Constructor in
  match ce.data with
  | C.Class c -> add_Class_pc ont c pc
  | C.ObjectIntersectionOf (ce, cce) ->
      add_ClassExpression_pc ont ce pc;
      add_ClassExpression_pc ont cce pc;
      ont.count_ObjectIntersectionOf <-
      PC.sum ont.count_ObjectIntersectionOf pc  
  | C.ObjectUnionOf (ce, cce) ->
      add_ClassExpression_pc ont ce pc;
      add_ClassExpression_pc ont cce pc;
      ont.count_ObjectUnionOf <-
      PC.sum ont.count_ObjectUnionOf pc  
  | C.ObjectComplementOf de ->
      add_ClassExpression_pc ont de (PC.inverse pc);
      ont.count_ObjectComplementOf <-
      PC.sum ont.count_ObjectComplementOf pc
  | C.ObjectOneOf i_lst ->
      List.iter (fun i -> add_Individual_pc ont i pc) i_lst;
      ont.count_ObjectOneOf <-
      PC.sum ont.count_ObjectOneOf pc
  | C.ObjectSomeValuesFrom (ope, de) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      add_ClassExpression_pc ont de pc;
      ont.count_ObjectSomeValuesFrom <-
      PC.sum ont.count_ObjectSomeValuesFrom pc
  | C.ObjectAllValuesFrom (ope, de) ->
      add_ObjectPropertyExpression_pc ont ope (PC.inverse pc);
      add_ClassExpression_pc ont de pc;
      ont.count_ObjectAllValuesFrom <-
      PC.sum ont.count_ObjectAllValuesFrom pc
  | C.ObjectHasValue (ope, i) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      add_Individual_pc ont i pc;
      ont.count_ObjectHasValue <-
      PC.sum ont.count_ObjectHasValue pc
  | C.ObjectHasSelf ope ->
      add_ObjectPropertyExpression_pc ont ope pc;
      ont.count_ObjectHasSelf <-
      PC.sum ont.count_ObjectHasSelf pc
  | C.ObjectMinCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope pc;
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce pc;
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | C.ObjectMaxCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope (PC.inverse pc);
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce (PC.inverse pc);
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | C.ObjectExactCardinality (n, ope, ceo) ->
      add_ObjectPropertyExpression_pc ont ope (PC.symm pc);
      begin match ceo with
        | None -> ()
        | Some ce -> add_ClassExpression_pc ont ce (PC.symm pc);
      end;
      ont.count_ObjectMinCardinality <-
      PC.sum ont.count_ObjectMinCardinality pc
  | _ -> ()
;;

let extractRoleNameFromProperty e = match e.data with 
          ObjectProperty.Constructor.IRI iri -> (match iri.data with IRI.Constructor.IRI r -> r)
        | _ -> failwith "Constructor not supported."

let extractRoleNameFromExpression e = 
        match e.data with 
            ObjectPropertyExpression.Constructor.ObjectProperty e2 
                    -> extractRoleNameFromProperty e2
            | _ -> failwith "Constructor not supported."

let extractRoleNameFromExpressionList e = 
          match e with [exp] ->
               extractRoleNameFromExpression exp
        | _ -> failwith "Constructor not supported."

let add_ObjectPropertyAxiom ont opa =
  let module S = ObjectPropertyAxiom.Set in
  let module C = ObjectPropertyAxiom.Constructor in
  if not (S.mem opa ont.record_ObjectPropertyAxiom) then
    begin
      ont.record_ObjectPropertyAxiom <- S.add opa
        ont.record_ObjectPropertyAxiom;
      match opa.data with
      | C.SubObjectPropertyOf (ope_ch, ope) ->                                              
          List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative)) ope_ch;
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
          ont.count_SubPropertyOf <- succ ont.count_SubPropertyOf
      | C.EquivalentObjectProperties (ope_lst) ->
          List.iter (fun ope -> add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both)) ope_lst;
          ont.count_EquivalentProperties <- succ ont.count_EquivalentProperties
      | C.DisjointObjectProperties _ -> ()
      | C.InverseObjectProperties (ope1, ope2) ->
          add_ObjectPropertyExpression_pc ont ope1 (PC.to_elt Polarity.Both);
          add_ObjectPropertyExpression_pc ont ope2 (PC.to_elt Polarity.Both);
          ont.count_InverseProperties <- succ ont.count_InverseProperties
      | C.ObjectPropertyDomain _ -> ()
      | C.ObjectPropertyRange _ -> ()
      | C.FunctionalObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
          ont.count_FunctionalProperty <- succ ont.count_FunctionalProperty
      | C.InverseFunctionalObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Negative);
          ont.count_InverseFunctionalProperty <- succ ont.count_InverseFunctionalProperty
      | C.ReflexiveObjectProperty _ -> ()
      | C.IrreflexiveObjectProperty _ -> ()
      | C.SymmetricObjectProperty _ -> ()
      | C.AsymmetricObjectProperty _ -> ()
      | C.TransitiveObjectProperty ope ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Both);
          ont.count_TransitiveProperty <- succ ont.count_TransitiveProperty
    end
;;

let add_ClassExpressionAxiom ont cea =
  let module S = ClassExpressionAxiom.Set in
  let module C = ClassExpressionAxiom.Constructor in
  if not (S.mem cea ont.record_ClassExpressionAxiom) then
    begin
      ont.record_ClassExpressionAxiom <- S.add cea
        ont.record_ClassExpressionAxiom;
      match cea.data with
      | C.SubClassOf (ce1, ce2) ->
          add_ClassExpression_pc ont ce1 (PC.to_elt Polarity.Negative);
          add_ClassExpression_pc ont ce2 (PC.to_elt Polarity.Positive);
          ont.count_SubClassOf <- succ ont.count_SubClassOf
      | C.EquivalentClasses (c_lst) ->
          List.iter (fun ce -> add_ClassExpression_pc ont ce (PC.to_elt Polarity.Both)) c_lst;
          ont.count_EquivalentClasses <- succ ont.count_EquivalentClasses
      | C.DisjointClasses _ -> ()
      | C.DisjointUnion _ -> ()
    end
;;

let add_Assertion ont a =
  let module S = Assertion.Set in
  let module C = Assertion.Constructor in
  if not (S.mem a ont.record_Assertion) then
    begin
      ont.record_Assertion <- S.add a
        ont.record_Assertion;
      match a.data with
      | C.SameIndividual _ -> ()
      | C.DifferentIndividuals _ -> ()
      | C.ClassAssertion (ce, i) ->
          add_ClassExpression_pc ont ce (PC.to_elt Polarity.Positive);
          add_Individual_pc ont i Polarity.Negative;
      | C.ObjectPropertyAssertion (ope, i1, i2) ->
          add_ObjectPropertyExpression_pc ont ope (PC.to_elt Polarity.Positive);
          add_Individual_pc ont i1 (PC.to_elt Polarity.Negative);
          add_Individual_pc ont i2 (PC.to_elt Polarity.Negative);
      | C.NegativeObjectPropertyAssertion _ -> ()
      | C.DataPropertyAssertion _ -> ()
      | C.NegativeDataPropertyAssertion _ -> ()
    end
;;

(** =============== printing various information ================== **)

let print_statistics ont out =
  fprintf out "Ontology information:\n";
  fprintf out "==============================\n";
  fprintf out "Signature (element counts):\n";
  fprintf out "---------------------------\n";
  fprintf out "\tobject properties:\t\t %n\n" (total_ObjectPropertyIRI ont);
  fprintf out "\tclasses:\t\t\t %n\n" (total_ClassIRI ont);
  fprintf out "\tindividuals:\t\t\t %n\n" (total_IndividualIRI ont);
  fprintf out "\n";
  fprintf out "Structure (expression counts):\t\t(pos)\t(neg)\n";
  fprintf out "------------------------------\n";
  fprintf out "\tinverse object porperties:\t %n\t %n\n"
    (PC.get_pos ont.count_InverseObjectProperty)
    (PC.get_neg ont.count_InverseObjectProperty);
  fprintf out "\tobject complements:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectComplementOf)
    (PC.get_neg ont.count_ObjectComplementOf);
  fprintf out "\tobject intersections:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectIntersectionOf)
    (PC.get_neg ont.count_ObjectIntersectionOf);
  fprintf out "\tobject unions:\t\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectUnionOf)
    (PC.get_neg ont.count_ObjectUnionOf);
  fprintf out "\tobject some values from:\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectSomeValuesFrom)
    (PC.get_neg ont.count_ObjectSomeValuesFrom);
  fprintf out "\tobject all values from:\t\t %n\t %n\n"
    (PC.get_pos ont.count_ObjectAllValuesFrom)
    (PC.get_neg ont.count_ObjectAllValuesFrom);
  fprintf out "\t\"owl:Thing\":\t\t\t %s\t %s\n"
    (if PC.get_pos ont.count_Thing > 0 then "+" else "-")
    (if PC.get_neg ont.count_Thing > 0 then "+" else "-");
  fprintf out "\t\"owl:Nothing\":\t\t\t %s\t %s\n"
    (if PC.get_pos ont.count_Nothing > 0 then "+" else "-")
    (if PC.get_neg ont.count_Nothing > 0 then "+" else "-");
  fprintf out "\n";
  fprintf out "Theory (axiom counts):\n";
  fprintf out "----------------------\n";
  fprintf out "\tsub object properties:\t\t %n\n" ont.count_SubPropertyOf;
  fprintf out "\tequivalent object properties:\t %n\n" ont.count_EquivalentProperties;
  fprintf out "\tinverse object properties:\t %n\n" ont.count_InverseProperties;
  fprintf out "\tfunctional object properties:\t %n\n" ont.count_FunctionalProperty;
  fprintf out "\tinverse functional object properties:\t %n\n" ont.count_InverseFunctionalProperty;
  fprintf out "\ttransitive object properties:\t %n\n" ont.count_TransitiveProperty;
  fprintf out "\tobject property chains:\t %n\n" ont.count_RoleComposition;
  fprintf out "\tsub classes:\t\t\t %n\n" ont.count_SubClassOf;
  fprintf out "\tequivalent classes:\t\t %n\n" ont.count_EquivalentClasses;
  fprintf out "\tclass assertions:\t\t %n\n" ont.count_ClassAssertion;
  fprintf out "\tobject property assertions:\t %n\n" ont.count_PropertyAssertion;
  fprintf out "\n";
;;

(*|let print_rbox_axioms ont out =                          *)
(*|  iter_rbox_axioms (fun ax ->                            *)
(*|          Printf.fprintf out "%s\n" (ObjectPropertyAxiom.str ax)   *)
(*|    ) ont                                                *)
(*|;;                                                       *)
(*|                                                         *)
(*|let print_tbox_axioms ont out =                          *)
(*|  iter_tbox_axioms (fun ax ->                            *)
(*|          Printf.fprintf out "%s\n" (ClassExpressionAxiom.str ax)*)
(*|    ) ont;                                               *)
(*|;;                                                       *)
(*|                                                         *)
(*|let print_abox_axioms ont out =                          *)
(*|  iter_abox_axioms (fun ax ->                            *)
(*|          Printf.fprintf out "%s\n" (Assertion.str ax)   *)
(*|    ) ont;                                               *)
(*|;;                                                       *)
(*|                                                         *)
(*|let print_axioms ont out =                               *)
(*|  Printf.fprintf out "RBox axioms:\n";                   *)
(*|  Printf.fprintf out "------------\n";                   *)
(*|  print_rbox_axioms ont out;                             *)
(*|  Printf.fprintf out "\nTBox axioms:\n";                 *)
(*|  Printf.fprintf out "------------\n";                   *)
(*|  print_tbox_axioms ont out;                             *)
(*|  Printf.fprintf out "\nABox axioms:\n";                 *)
(*|  Printf.fprintf out "------------\n";                   *)
(*|  print_abox_axioms ont out;                             *)
(*|  Printf.fprintf out "\n";                               *)
(*|;;                                                       *)

let get_concept_names t =
  t.conceptNameSet

let get_original_concept_names t =
  t.originalConceptNameSet

let get_role_names t =
  t.roleNameSet

let compute_definitorial_depth_hash ont occurrenceHash =
  let conceptNameSet = get_concept_names ont in
  (* maps concept names to their definitorial depth *)
  let definitorialDepthHash = Hashtbl.create 50 in
  let rec compute_definitiorial_depth cname =
    try
      Hashtbl.find definitorialDepthHash cname
    with Not_found ->
      try
        let set = Hashtbl.find occurrenceHash cname in
        let depth = (StringSet.fold (fun i cname2 -> max i (compute_definitiorial_depth cname2)) 0 set) + 1 in
        Hashtbl.replace definitorialDepthHash cname depth; depth
      with Not_found -> 0 in
  let result = Hashtbl.create 50
  in
  StringSet.iter (fun cname ->
                    let depth = compute_definitiorial_depth cname in
                    StringSet.update_hash_with_element result depth cname)
                  conceptNameSet;
  result

(** **)

type occurrence_hash = (string, StringSet.t) Hashtbl.t

let create_occurrence_hash _ =
  Hashtbl.create 50

let insert_in_occurrence_hash occurrenceHash cname l =
  StringSet.update_hash occurrenceHash cname (StringSet.from_list l)


(**============= Accessing the 'pre' sets ================= **)

let pre_concepts pre_concepts_hash cname =
  try
    Hashtbl.find pre_concepts_hash cname
  with Not_found ->
    StringSet.singleton cname

let pre_sigma_concepts ont pre_concepts_hash sigma cname =
  let return = pre_concepts pre_concepts_hash cname in
  StringSet.intersection return (Sigma.get_concept_names sigma)

let pre_sigma_concepts_with_domain_range ont pre_concepts_hash sigma cname =
  let return = pre_concepts pre_concepts_hash cname in
  StringSet.union (StringSet.intersection return (Sigma.get_concept_names sigma))
                  (StringSet.union (StringSet.intersection return (Sigma.get_domain_concept_names sigma))
                                   (StringSet.intersection return (Sigma.get_range_concept_names sigma)))

let pre_domain ont pre_concepts_hash cname =
  try
    Hashtbl.find ont.preDomainHash cname
  with Not_found ->
    let return = StringSet.fold (fun set dname -> if StringSet.mem (get_domain_concept_names ont) dname then
                                                    StringSet.add set (RoleMapping.map_domain_binding_to_role dname)
                                                  else
                                                    set)
                                StringSet.empty
                                (pre_concepts pre_concepts_hash cname)
    in
    Hashtbl.add ont.preDomainHash cname return;
    return

let pre_sigma_domain ont pre_concepts_hash sigma cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (Sigma.get_domain_concept_names sigma) dname then
                                                  StringSet.add set (RoleMapping.map_domain_binding_to_role dname)
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return

let pre_domain_concept_names ont pre_concepts_hash cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (get_domain_concept_names ont) dname then
                                                  StringSet.add set dname
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return

let pre_sigma_domain_concept_names ont pre_concepts_hash sigma cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (Sigma.get_domain_concept_names sigma) dname then
                                                  StringSet.add set dname
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return

let pre_range ont pre_concepts_hash cname =
  try
    Hashtbl.find ont.preRangeHash cname
  with Not_found ->
    let return = StringSet.fold (fun set dname -> if StringSet.mem (get_range_concept_names ont) dname then
                                                    StringSet.add set (RoleMapping.map_range_binding_to_role dname)
                                                  else
                                                    set)
                                StringSet.empty
                                (pre_concepts pre_concepts_hash cname)
    in
    Hashtbl.add ont.preRangeHash cname return;
    return

let pre_sigma_range ont pre_concepts_hash sigma cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (Sigma.get_range_concept_names sigma) dname then
                                                  StringSet.add set (RoleMapping.map_range_binding_to_role dname)
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return

let pre_range_concept_names ont pre_concepts_hash cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (get_range_concept_names ont) dname then
                                                  StringSet.add set dname
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return


let pre_sigma_range_concept_names ont pre_concepts_hash sigma cname =
  let return = StringSet.fold (fun set dname -> if StringSet.mem (Sigma.get_range_concept_names sigma) dname then
                                                  StringSet.add set dname
                                                else
                                                  set)
                              StringSet.empty
                              (pre_concepts pre_concepts_hash cname)
  in
  return

let is_entailed_by_range ont pre_concepts_hash r cname =
  StringSet.mem (pre_concepts pre_concepts_hash cname) (RoleMapping.map_role_to_range_binding r)
                
  
let post_concepts post_concepts_hash cname =
  try
    Hashtbl.find post_concepts_hash cname
  with Not_found ->
    StringSet.singleton cname

let post_proper_concepts post_concepts_hash cname =
  let postSet = post_concepts post_concepts_hash cname in
  StringSet.fold (fun newSet s -> if RoleMapping.is_proper_concept_name s then
                                    StringSet.add newSet s
                                  else
                                    newSet)
                 StringSet.empty
                 postSet

let post_proper_concepts_with_domain_bindings post_concepts_hash cname =
  let postSet = post_concepts post_concepts_hash cname in
  StringSet.fold (fun newSet s -> if RoleMapping.is_proper_concept_name_or_role_domain_binding s then
                                    StringSet.add newSet s
                                  else
                                    newSet)
                 StringSet.empty
                 postSet

let post_sigma_concepts post_concepts_hash sigma cname =
  let return = post_concepts post_concepts_hash cname in
  StringSet.intersection return (Sigma.get_concept_names sigma)


let top_class _ =
  (Class.Constructor.Thing)

let top_class_expression ont =
  cons_ClassExpression ont (ClassExpression.Constructor.Class (top_class ()))

let concept_name_class ont cname = 
  Class.Constructor.IRI (cons_IRI ont (IRI.Constructor.IRI cname))

let concept_name_class_expression ont cname = 
  cons_ClassExpression ont (ClassExpression.Constructor.Class (concept_name_class ont cname))

let role_name_object_property ont rname =
  cons_ObjectProperty ont (ObjectProperty.Constructor.IRI (cons_IRI ont (IRI.Constructor.IRI rname)))

let role_name_object_property_expression ont (rname : string) =
  cons_ObjectPropertyExpression ont
    (ObjectPropertyExpression.Constructor.ObjectProperty (role_name_object_property ont rname))

let inverse_object_property_expression ont rExp =
  match rExp.data with 
    ObjectPropertyExpression.Constructor.ObjectProperty p ->
      cons_ObjectPropertyExpression ont (ObjectPropertyExpression.Constructor.InverseObjectProperty p)
  | ObjectPropertyExpression.Constructor.InverseObjectProperty p ->
      cons_ObjectPropertyExpression ont (ObjectPropertyExpression.Constructor.ObjectProperty p)

let exists_rC_class_expression ont r exp =
  cons_ClassExpression ont (ClassExpression.Constructor.ObjectSomeValuesFrom (r, exp))

let rec conjunction_class_expression ont l =
  match l with
    [] -> top_class_expression ont
  | [exp] -> exp
  | exp::l' ->
       cons_ClassExpression ont (ClassExpression.Constructor.ObjectIntersectionOf(exp, conjunction_class_expression ont l') )

let definition_axiom ont e1 e2 =
  cons_ClassExpressionAxiom ont (ClassExpressionAxiom.Constructor.EquivalentClasses [e1;e2])

let subsumption_axiom ont e1 e2 =
  cons_ClassExpressionAxiom ont (ClassExpressionAxiom.Constructor.SubClassOf (e1, e2))

let domain_class_expression ont rnameExp =
  cons_ClassExpression ont
    (ClassExpression.Constructor.ObjectSomeValuesFrom(rnameExp,
                                                      cons_ClassExpression ont
                                                        (ClassExpression.Constructor.Class (Class.Constructor.Thing))))
(* 'rnameExp' will be inverted *)
let range_class_expression ont rnameExp =
  cons_ClassExpression ont 
    (ClassExpression.Constructor.ObjectSomeValuesFrom(inverse_object_property_expression ont rnameExp,
                                                      cons_ClassExpression ont (ClassExpression.Constructor.Class (Class.Constructor.Thing))))

let extractRoleName e =
  match e.Consed.T.data with ObjectProperty.Constructor.IRI iri -> (match iri.data with IRI.Constructor.IRI r -> r)
  | _ -> failwith "Wrong constructor."

let extractObjectProperty e =
  match e.Consed.T.data with ObjectPropertyExpression.Constructor.ObjectProperty(op) -> op 
                           | ObjectPropertyExpression.Constructor.InverseObjectProperty(op) -> op

let domain_subsumption_axiom ont (r : Owl2.ObjectPropertyExpression.t) c =
  ClassExpressionAxiom.Constructor.SubClassOf(domain_class_expression ont r, c)

let range_subsumption_axiom ont r c =
  ClassExpressionAxiom.Constructor.SubClassOf(range_class_expression ont r, c)

let construct_ObjectIntersectionOf (e1, e2) =
  if ClassExpression.compare e1 e2 < 0 then ClassExpression.Constructor.ObjectIntersectionOf (e1, e2) else ClassExpression.Constructor.ObjectIntersectionOf (e2, e1)

let rec concept_to_OWL ont c =
  let rec conjunctiveConceptListToOWL l =
    match l with [] -> failwith "Empty list given as argument."
               | [c] -> concept_to_OWL ont c
               | (c::l) -> cons_ClassExpression ont
                             (construct_ObjectIntersectionOf(concept_to_OWL ont c, conjunctiveConceptListToOWL l))
  in
  match c with 
    Top -> top_class_expression ont
  | Name s -> concept_name_class_expression ont s
  | Domain r -> domain_class_expression ont (role_name_object_property_expression ont r)
  | Range r -> range_class_expression ont (role_name_object_property_expression ont r)
  | And l -> 
    (match l with [] -> failwith "Empty 'And' given as argument!"  (* all of this is necessary as otherwise strange bugs appear *)
                        | l -> conjunctiveConceptListToOWL l)
  | Exists(r, c) -> cons_ClassExpression ont
                      (ClassExpression.Constructor.ObjectSomeValuesFrom(role_name_object_property_expression ont r, concept_to_OWL ont c))
  | ExistsUniversalRole _ -> failwith "concept_to_OWL: (some :universal C) cannot be converted!"
  | ExistsRoleConjunction (_, _) -> failwith "concept_to_OWL: role conjunctions cannot be converted!"

let add_domain_range_concept_definitions roleNames ont =
  let module S = ClassExpressionAxiom.Set in
  let axiomList = StringSet.fold (fun l r -> let domainBinding = (RoleMapping.map_role_to_domain_binding r) in
                                             (definition_axiom ont (concept_name_class_expression ont domainBinding)
                                                                   (domain_class_expression ont (role_name_object_property_expression ont r)))::
                                             (definition_axiom ont (concept_name_class_expression ont (RoleMapping.map_role_to_range_binding r))
                                                                   (range_class_expression ont (role_name_object_property_expression ont r)))::l)
                                 []
                                 roleNames
  in
  List.iter (fun ax -> add_ClassExpressionAxiom ont ax;
                       ont.domainRangeAxioms <- S.add ax ont.domainRangeAxioms;)
            axiomList

let is_domain_range_axiom ont ax =
  let module S = ClassExpressionAxiom.Set in
  S.mem ax ont.domainRangeAxioms


let rec concept_ELHr_to_OWL ont c =
  match c with
    Top -> top_class_expression ont
  | Name cname -> concept_name_class_expression ont cname
  | Exists(r, c) -> exists_rC_class_expression ont (role_name_object_property_expression ont r) (concept_ELHr_to_OWL ont c)
  | And(l) -> conjunction_class_expression ont (List.rev_map (concept_ELHr_to_OWL ont) l)
  | _ -> failwith ("Non-ELHr constructor encountered in 'concept_ELHr_to_OWL'! (" ^ (concept_to_string c) ^ ")")

(*    | ExistsRoleConjunction(l, c) -> let (concepts, roles) = signature_of_concept c in
                                   (concepts, StringSet.union (StringSet.from_list l) roles) 
  | ExistsUniversalRole(c) -> signature_of_concept c
  | Domain(r)-> (StringSet.empty, StringSet.singleton r)
  | Range(r) -> (StringSet.empty, StringSet.singleton r)
*)

let rec concept_ELran_to_OWL ont c =
  match c with
    Top -> top_class_expression ont
  | Name cname -> concept_name_class_expression ont cname
  | Exists(r, c) -> exists_rC_class_expression ont (role_name_object_property_expression ont r) (concept_ELran_to_OWL ont c)
  | And(l) -> conjunction_class_expression ont (List.rev_map (concept_ELran_to_OWL ont) l)
  | Domain(r)-> domain_class_expression ont (role_name_object_property_expression ont r)
  | Range(r) -> range_class_expression ont (role_name_object_property_expression ont r)
(*  | Domain(r)-> concept_name_class_expression ont (RoleMapping.map_role_to_domain_binding r)
(*   domain_class_expression ont (role_name_object_property_expression ont r) *)
  | Range(r) -> concept_name_class_expression ont (RoleMapping.map_role_to_range_binding r)
(* range_class_expression ont (role_name_object_property_expression ont r) *)*)
  | _ -> failwith ("Non-ELran constructor encountered in 'concept_ELHr_to_OWL'! (" ^ (concept_to_string c) ^ ")")

(*    | ExistsRoleConjunction(l, c) -> let (concepts, roles) = signature_of_concept c in
                                   (concepts, StringSet.union (StringSet.from_list l) roles) 
  | ExistsUniversalRole(c) -> signature_of_concept c
*)

(*val domain_subsumption_axiom : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t -> ClassExpressionAxiom.Constructor.t
val range_subsumption_axiom : t -> Owl2.ObjectPropertyExpression.t -> ClassExpression.t -> ClassExpressionAxiom.Constructor.t
role_name_object_property_expression

val definition_axiom : t -> ClassExpression.t -> ClassExpression.t -> ClassExpressionAxiom.t

val subsumption_axiom : t -> ClassExpression.t -> ClassExpression.t -> ClassExpressionAxiom.t*)

let axioms_ELHr_to_Ontology l =
  let ont = create () in
  List.iter (fun ax -> match ax with
                         ConceptInclusion(Domain(r), c) -> let owlAx = domain_subsumption_axiom ont (role_name_object_property_expression ont r)
                                                                                                    (concept_ELHr_to_OWL ont c)
                                                           in
                                                           add_ClassExpressionAxiom ont (cons_ClassExpressionAxiom ont owlAx)
                         | ConceptInclusion(Range(r), c) -> let owlAx = range_subsumption_axiom ont (role_name_object_property_expression ont r)
                                                                                                    (concept_ELHr_to_OWL ont c)
                                                           in
                                                           add_ClassExpressionAxiom ont (cons_ClassExpressionAxiom ont owlAx)
                         | ConceptInclusion(c1, c2) -> let owlAx = subsumption_axiom ont (concept_ELHr_to_OWL ont c1)
                                                                                         (concept_ELHr_to_OWL ont c2)
                                                       in
                                                       add_ClassExpressionAxiom ont owlAx
                         | ConceptEquality(c1, c2) -> let owlAx = definition_axiom ont (concept_ELHr_to_OWL ont c1)
                                                                                       (concept_ELHr_to_OWL ont c2)
                                                      in
                                                      add_ClassExpressionAxiom ont owlAx
                         | RoleInclusion(r1, r2) -> let owlAx = ObjectPropertyAxiom.Constructor.SubObjectPropertyOf([role_name_object_property_expression ont r1],
                                                                                                                     role_name_object_property_expression ont r2)
                                                    in
                                                    add_ObjectPropertyAxiom ont (cons_ObjectPropertyAxiom ont owlAx))
            l;
  ont

(* kate: replace-tabs on; indent-width 2; *)
