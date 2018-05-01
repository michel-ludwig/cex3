%{
(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010 - 2014                              *)
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

open Types
open Utilities

(* abbreviations for commonly used modules *)

(*let parse_error s =            *)
(*  Printf.fprintf stderr "%s" s;*)
(*  raise Parsing.Parse_error    *)
(*;;                             *)

let checkIfConceptNameHashOccurredLeftAlready leftHandSideSet conceptName =
  if StringSet.mem !leftHandSideSet conceptName then
    failwith("Concept name " ^ conceptName ^ " already occurred on the left-hand side of a concept axiom!")
  else
    leftHandSideSet := StringSet.add !leftHandSideSet conceptName

let leftHandSideSet = ref (StringSet.empty)

%}

/* parentheses */
%token LeftParen RightParen

/* axioms keywords */
%token Implies Equivalent EquivalentRole ImpliesRole Inverse Functional
%token Transitive Composition Domain Range

/* properties */
%token DomainProperty RangeProperty RightIdentityProperty ParentsProperty ParentProperty

/* constructors */
%token And Or Some All Not Inv Top Bottom

/* identifiers */
%token <string> Identifier

/* comments */
%token <string> Comment

/* eof */
%token EOF

%start ontology
%type <Types.axiom list> ontology
%%

ontology : init axioms EOF
 { $2 }

init :
 { leftHandSideSet := StringSet.empty; }

axioms : /* empty */ { [] }
 | axioms axiom { List.rev_append $2 $1 }

axiom :
   concept_axiom { [$1] }
 | role_axiom    { [$1] }
 | ignored_role_axiom  { [] }
 | domain_axiom  { [$1] }
 | range_axiom   { [$1] }
 | domain_range_axiom   { $1 }

concept_axiom :
   LeftParen Implies concept_name RightParen
   { checkIfConceptNameHashOccurredLeftAlready leftHandSideSet $3;
     ConceptInclusion(Name $3, Name $3)
   }
 | LeftParen Implies concept_name concept RightParen
   { checkIfConceptNameHashOccurredLeftAlready leftHandSideSet $3;
     ConceptInclusion(Name $3, $4) }
 | LeftParen Equivalent concept_name concept RightParen
   { checkIfConceptNameHashOccurredLeftAlready leftHandSideSet $3;
     ConceptEquality(Name $3, $4) }

concept_name:
   Identifier
   { $1 }

concept :
   Identifier
   { Name $1 }
 | LeftParen And and_concepts RightParen
   { And($3) }
 | LeftParen Some role Top RightParen
   { Exists($3, Top) }
 | LeftParen Some role concept RightParen
   { Exists($3, $4) }

and_concepts :
   concept concept
   { [$1; $2]  }
 | and_concepts concept
   { $2::$1 }

role : 
   Identifier
   { $1 }

role_axiom : 
  LeftParen ImpliesRole role RightParen
    { RoleInclusion($3, $3) }
  | LeftParen ImpliesRole role role RightParen 
    { RoleInclusion($3, $4) }
  | LeftParen ImpliesRole role ParentProperty role RightParen 
    { RoleInclusion($3, $5) }
  | LeftParen ImpliesRole role ParentsProperty LeftParen role RightParen RightParen
    { RoleInclusion($3, $6) }

ignored_role_axiom : 
  | LeftParen ImpliesRole role RightIdentityProperty role RightParen
    { print_endline("Right-identity axiom (" ^ $3 ^ ", " ^ $3 ^ ") ignored."); }

domain_axiom :
    LeftParen Domain role concept RightParen
    { ConceptInclusion(Domain($3), $4) }
  | LeftParen ImpliesRole role DomainProperty concept RightParen
    { ConceptInclusion(Domain($3), $5) }

range_axiom :
    LeftParen Range role concept RightParen
    { ConceptInclusion(Range($3), $4) }
  | LeftParen ImpliesRole role RangeProperty concept RightParen
    { ConceptInclusion(Range($3), $5) }

domain_range_axiom :
    LeftParen ImpliesRole role DomainProperty concept RangeProperty concept RightParen
    { [ConceptInclusion(Domain($3), $5);
       ConceptInclusion(Range($3), $7)] }

%%

(* kate: replace-tabs on; indent-width 2; *)