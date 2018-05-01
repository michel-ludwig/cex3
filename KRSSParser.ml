(***********************************************************)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
(*                                                         *)
(*  Copyright (C) 2010 - 2014                              *)
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

(**=========== input-output for ontologies in functional style syntax ============**)

open Owl2
open Consed.T
open Types
module O = Ontology
module PB = ProgressBar
module F = Format

(**=========== loading from the input channel ===========**)

let load_ontology input =
  let lexbuf = Lexing.from_channel input in
  let ont =
    try
      Krss_parser.ontology Krss_lexer.token lexbuf
    with Parsing.Parse_error ->
        let err_lexeme = Lexing.lexeme lexbuf in
        let err_pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
        Printf.fprintf stderr "\nLine %n, characters %n-%n:\nSyntax error: unexpected \"%s\"\n"
          lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
          (err_pos - String.length err_lexeme + 1)
          err_pos
          err_lexeme;
        raise Parsing.Parse_error
  in
  ont

let load_signature input =
  let lexbuf = Lexing.from_channel input in
  try
    Sigma_parser.start Sigma_lexer.token lexbuf
  with Parsing.Parse_error ->
      let err_lexeme = Lexing.lexeme lexbuf in
      let err_pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol in
      Printf.fprintf stderr "\nLine %n, characters %n-%n:\nSyntax error: unexpected \"%s\"\n"
        lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
        (err_pos - String.length err_lexeme + 1)
        err_pos
        err_lexeme;
      raise Parsing.Parse_error

let load_sigma input =
  Sigma.create_from_sets (load_signature input)