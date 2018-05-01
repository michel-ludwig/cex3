%{
(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  Copyright (C) 2010                                     *)
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

%}

%token TOK_EOF
%token <string> TOK_IDENT
%token TOK_LPAR
%token TOK_RPAR
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_CONCEPTS
%token TOK_ROLES

%start start
%type <Types.StringSet.t * Types.StringSet.t> start

%%

start: concepts TOK_EOF { ($1, Types.StringSet.empty) }
     | concepts roles TOK_EOF { ($1, $2) }
     ;

concepts: TOK_CONCEPTS cnlist { $2 }
     ;

roles: TOK_ROLES rnlist { $2 }
     ;

cnlist: TOK_LBRACKET TOK_RBRACKET { Types.StringSet.empty }
      | TOK_LBRACKET necnlist TOK_RBRACKET { $2 }
      ;

necnlist: cname { Types.StringSet.add (Types.StringSet.empty) $1 }
        | cname necnlist { Types.StringSet.add $2 $1 }
        ;

rnlist: TOK_LBRACKET TOK_RBRACKET { Types.StringSet.empty }
      | TOK_LBRACKET nernlist TOK_RBRACKET { $2 }
      ;

nernlist: rname { Types.StringSet.add (Types.StringSet.empty) $1 }
        | rname nernlist { Types.StringSet.add $2 $1 }
        ;

cname: TOK_IDENT { $1 } ;
rname: TOK_IDENT { $1 } ;
