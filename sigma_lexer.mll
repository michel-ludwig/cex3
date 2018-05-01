(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
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

{
open Sigma_parser (* tokens *)
exception Eof

let currentLine = ref 1
let lineStartPos = ref 0
let posmin = ref 0 (*position*)
let posmax = ref 0 (*position de l'erreur*)


let update_line lexbuf = (*change line*)
	incr currentLine;
	lineStartPos := Lexing.lexeme_start lexbuf (*returns the position in the input stream of the first character of the matched string*)

let get = Lexing.lexeme

let update_pos str =
	posmin := (Lexing.lexeme_start str) - !lineStartPos + 1;
	posmax := (Lexing.lexeme_end str) - !lineStartPos + 1

let error buf callerID =
        print_endline callerID;
	update_pos buf;
	raise Parsing.Parse_error
}

let spaces = [ ' ' '\t' '\r']
let ident = [^ ';' '(' ')' '[' ']' ' ' '\n' '\r' ':']+

rule token = parse
    spaces          { update_pos lexbuf; token lexbuf } (* Eat up spaces/endlines *)
  | '\n'            { update_line lexbuf; token lexbuf }
  | "("             { update_pos lexbuf; TOK_LPAR }
  | ")"             { update_pos lexbuf; TOK_RPAR }
  | "["             { update_pos lexbuf; TOK_LBRACKET }
  | "]"             { update_pos lexbuf; TOK_RBRACKET }
  | "concepts"      { update_pos lexbuf; TOK_CONCEPTS }
  | "roles"         { update_pos lexbuf; TOK_ROLES }
  | ident           { update_pos lexbuf; TOK_IDENT (Lexing.lexeme lexbuf) }
  | eof             { TOK_EOF }
  | _               { error lexbuf "" }
