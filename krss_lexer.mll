(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
(*  Copyright (C) 2009                                     *)
(*  Yevgeny Kazakov <yevgeny.kazakov@comlab.ox.ac.uk>      *)
(*  University of Oxford                                   *)
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
open Krss_parser
  
let update_pos lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let p = pos.Lexing.pos_cnum in
  ProgressBar.set_state p;
  lexbuf.Lexing.lex_curr_p <- 
  { pos with
    Lexing.pos_lnum = succ pos.Lexing.pos_lnum;
    Lexing.pos_bol = pos.Lexing.pos_cnum
    }
 ;;  

let error buf callerID =
  print_endline callerID;
  update_pos buf;
  raise Parsing.Parse_error
 ;;

}

rule token = parse
    "\n"               { update_pos lexbuf; token lexbuf}
  | "\r\n"             { update_pos lexbuf; token lexbuf}
  | "("                { LeftParen    }
  | ")"                { RightParen   }
  | [' ' '\t' '\r']      { token lexbuf }

(* concept axioms *)

  | "implies"          { Implies    }
  | "defprimconcept"   { Implies    }
  | "define-primitive-concept"   { Implies    }
  | "DEFINE-PRIMITIVE-CONCEPT"   { Implies    }    
  | "equivalent"       { Equivalent }
  | "defconcept"       { Equivalent }
  | "define-concept"   { Equivalent }
  | "DEFINE-CONCEPT"   { Equivalent }

(* role axioms *)

  | "defprimrole"    { ImpliesRole }
  | "define-primitive-role"    { ImpliesRole }
  | "DEFINE-PRIMITIVE-ROLE"    { ImpliesRole }  
  | "implies-role"   { ImpliesRole }
  | "role-inclusion" { ImpliesRole }
(*   | "equivalent-role" { EquivalentRole } *)
(*   | "role-equivalent" { EquivalentRole } *)
(*   | "inverse"        { Inverse     } *)
(*   | "functional"     { Functional  } *)
(*   | "FUNCTIONAL"     { Functional  } *)
(*   | "transitive"     { Transitive  } *)
(*   | "TRANSITIVE"     { Transitive  } *)
(*   | "composition"    { Composition } *)
  | "domain"         { Domain  }
  | "DOMAIN"         { Domain  }
  | "range"          { Range  }
  | "RANGE"          { Range  }

(* concept constructors *)

  | "and"          { And    }
  | "AND"          { And    }
  | "intersection" { And    }
(*   | "or"           { Or     } *)
(*   | "OR"           { Or     } *)
(*   | "union"        { Or     }  *)
  | "some"         { Some   }
  | "SOME"         { Some   }
(*   | "all"          { All    } *)
(*   | "ALL"          { All    } *)
(*   | "not"          { Not    } *)
(*   | "complement"   { Not    } *)
  | "top"          { Top    }
  | "TOP"          { Top    }
(*  | "bottom"       { Bottom } *)
(*   | "BOTTOM"       { Bottom }  *)
(*   | "bot"          { Bottom } *)
(*   | "BOT"          { Bottom } *)

(* role constructors *)

(*   | "inv"          { Inv } *)

(* identifiers *)

  | [^ ';' '(' ')' ' ' '\n' '\r' ':']+ { Identifier (Lexing.lexeme lexbuf) }

(* comments annotations *)
  
  | ";" [^ '\n']*  { update_pos lexbuf; token lexbuf }

(* eof *)

  | eof            { update_pos lexbuf; EOF }

(* role properties *)

  | ":domain"               { DomainProperty }
  | "::range"                { RangeProperty }  
  | ":range"                { RangeProperty }
  | ":right-identity"       { RightIdentityProperty }
  | "::parents"              { ParentsProperty }
  | ":parents"              { ParentsProperty }
  | ":parent"               { ParentProperty }
  | _                       { error lexbuf ""}