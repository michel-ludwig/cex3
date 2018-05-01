(***********************************************************)
(*  Copyright (C) 2008                                     *)
(*  Boris Konev (konev@liverpool.ac.uk)                    *)
(*  University of Liverpool                                *)
(*                                                         *)
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

open NameMapping;;
open Ontology;;
open Settings;;
open Types;;
open Utilities;;

let start_time = Unix.times();;

(* First we change some settings of the GC in order to increase *)
(* the performance of the program.                              *)
let gc = Gc.get () in
  (* gc.verbose <- 1; *)
  (* The program is memory hungry *)
  gc.Gc.minor_heap_size <- 196608;
  gc.Gc.major_heap_increment <- 1048576;
  (* We do not care if it wastes some memory *)
  gc.Gc.space_overhead <- 150;
(*   gc.Gc.stack_limit <- 100000; *)
  Gc.set gc;;

(******************************************************************)
(*                    Parsing options                             *)
(******************************************************************)
let tbx1FileName = ref "";;
let tbx2FileName = ref "";;
let sigmaFileName = ref "";;
let outputFileName = ref "";;
let nameMappingFileNameList = ref [];;
let inx1 = ref stdin;;
let inx2 = ref stdin;;
let sigx = ref stdin;;
let versionString = "3.0";;

(******************************************************************************************)
(* CAUTION: For historical reasons, the argument given as TBox1 becomes TBox2 internally, *)
(*          and the argument given as TBox2 becomes TBox1 internally!                     *)
(******************************************************************************************)

let args_spec = ("-d", Arg.Set debug, " Print debug info")::
                ("-v", Arg.Set verbose, " Be verbose (print intermediate transformations)")::
                ("--tbx1", Arg.Set_string tbx1FileName, " Specify the first TBox")::
                ("--tbx2", Arg.Set_string tbx2FileName," Specify the second TBox")::
                ("--sig", Arg.Set_string sigmaFileName, " Specify the signature. If omitted, the signature of the first TBox is used")::
                ("--examples", Arg.Set constructCounterExamples, " Construct counter-examples")::
                ("--mode", Arg.String (fun s -> mode := if (String.compare (String.lowercase s) "instance") = 0 then
                                                         InstanceCase
                                                       else if ((String.compare (String.lowercase s) "conjunctive-query") = 0)
                                                               || ((String.compare (String.lowercase s) "cq") = 0) then
                                                         ConjunctiveQueryCase
                                                       else
                                                         ConceptCase), " Sets the comparison mode. Possible values are 'Instance', 'Concept' or 'Conjunctive-Query'," ^
                                                                       " and the default mode is 'Concept'")::
                ("--use-sig", Arg.String (fun s -> signature_mode := (match (String.lowercase s) with
                                                                      "tbx1" -> FromTBox2
                                                                    | "tbx2" -> FromTBox1
                                                                    | "shared" -> FromIntersection
                                                                    | _ -> FromTBox1)), " Derive the signature used for the difference computation from one of the TBoxes." ^
                                                                                        " Possible values are 'tbx1', 'tbx2', 'shared'.")::
                ("--simplify", Arg.Set simplifyCounterExamplesSemantically, " Try to simplify counter-examples")::
                ("--statistics", Arg.Set extendedStatistics, " Print extended statistics")::
(*                 ("--check", Arg.Set checkCounterExamples, " Verify the correctness of the constructed counter-examples"):: *)
                ("--name-mapping", Arg.String (fun s -> nameMappingFileNameList := s::(!nameMappingFileNameList)), " Specify the file containing a mapping from concept names to their descriptions")::
                ("--output", Arg.Set_string outputFileName, " Specify the output file for saving constructed counter-examples")::
                [];;

let program_info = "==================================================================================" ^
                   "\nCEX Version " ^ versionString ^ " (Prototype)" ^ 
                   "\n==================================================================================" ^
                   "\nCopyright (C) 2008-2014 by Boris Konev (konev@liverpool.ac.uk)," ^
                   "\n                           Michel Ludwig (michel@tcs.inf.tu-dresden.de)," ^
                   "\n                           University of Liverpool, and" ^
                   "\n                           TU Dresden" ^
                   "\n==================================================================================" ^
                   "\nThis program uses the reasoner CB internally, which is Copyright (C) by" ^
                   "\nYevgeny Kazakov (yevgeny.kazakov@comlab.ox.ac.uk) and University of Oxford (2009)." ^
                   "\n==================================================================================" ^
                   "\n"

let usage_spec = program_info ^
                 "\nUsage: cex3 [-d] [-v] [--mode <concept | instance | conjunctive-query | cq>] " ^
                 "[--examples] [--use-sig <tbx1 | tbx2 | shared>]\n" ^ 
(*                  "             [--simplify] [--statistics] [--check] [--name-mapping <file>]*\n" ^ *)
                 "             [--simplify] [--statistics] [--name-mapping <file>]*\n" ^
                 "             --tbx1 <file> --tbx2 <file> [--sig <file>]"

let printUsageAndExit () = Arg.usage (Arg.align args_spec) usage_spec; print_newline();
                           exit 0;;

if Array.length Sys.argv = 1 then
    printUsageAndExit ();

Arg.parse (Arg.align args_spec) (fun _ -> printUsageAndExit ()) usage_spec ;

let outputChannel = if string_is_empty !outputFileName then
                      stdout
                    else
                      open_out !outputFileName in

Debug.init !debug !verbose;

print_endline(program_info);

let ont1 = execute_measure_time ("Loading ontology 1 (" ^ !tbx1FileName ^ ")...")
                                (fun _ -> KRSSParser.load_ontology (open_in !tbx1FileName)) in
(* print_endline("1 number of axioms: " ^(string_of_int (List.length(ont1)))); *)
let (concepts1, roles1) = signature_of_axiom_list ont1 in
(* print_endline("1 number of concept names: " ^(string_of_int (StringSet.cardinal(concepts1)))); *)
(* print_endline("1 number of role names: " ^(string_of_int (StringSet.cardinal(roles1)))); *)

(*print_endline("Parsed terminology1:");
List.iter (fun ax -> print_endline(axiom_to_string ax)) ont1;*)

let ont2 = execute_measure_time ("Loading ontology 2 (" ^ !tbx2FileName ^ ")...")
                                (fun _ -> KRSSParser.load_ontology (open_in !tbx2FileName)) in
(* print_endline("2 number of axioms: " ^(string_of_int (List.length(ont2)))); *)
let (concepts2, roles2) = signature_of_axiom_list ont2 in
(* print_endline("2 number of concept names: " ^(string_of_int (StringSet.cardinal(concepts2)))); *)
(* print_endline("2 number of role names: " ^(string_of_int (StringSet.cardinal(roles2)))); *)

let allRoleNames = StringSet.union roles1 roles2 in

  (* We add all the role domain and role range bindings to both ontologies for *all* role names occurring *)
  (* in both ontologies. *)
let terminology1 = Terminology.create ont1 allRoleNames in
let terminology2 = Terminology.create ont2 allRoleNames in

(* if -sig is specified, the signature is read from the given file *)
(* otherwise, sigma consists of the signature of TBox 1            *)
print_endline("Loading sigma..." ^ !sigmaFileName);
let sigma = 
  if string_is_empty !sigmaFileName then
    let computedSigma = match !signature_mode with
                          FromTBox1 -> (concepts1, roles1)
                        | FromTBox2 -> (concepts2, roles2) 
                        | FromIntersection -> (StringSet.intersection concepts1 concepts2,
                                               StringSet.intersection roles1 roles2)
    in
    Sigma.create_from_sets computedSigma
  else
    KRSSParser.load_sigma (open_in !sigmaFileName)
  in
if Debug.verbose_output () || Debug.debug_output () then
begin
  print_string ("concept names "); StringSet.print_set (Sigma.get_concept_names sigma); print_newline ();
  print_string ("role names "); StringSet.print_set (Sigma.get_role_names sigma); print_newline ();
  print_endline("Done.");
end;

(* compact_heap (); *)

let (cexList, roleDifferencesCount, atomicRHSDiffCount, atomicLHSDiffCount, domainDiffCount, rangeDiffCount, nameMapping) =
  (fun _ -> 

(*  let (pre_concept_hash1, post_concept_hash1, ont1_index) = 
    (fun _ -> (* we use a function here to 'hide' 'ont1_sat', which consumes a lot of memory *)
          print_endline("Saturating ontology 1...");
          let (ont1_sat, ont1_index) = ReasonerTBox.saturate ont1 in
          let (pre_concept_hash1, post_concept_hash1) = execute_measure_time "Computing pre and post(concepts) for ontology 1..."
                                                                            (fun _ -> ReasonerTBox.compute_pre_post_for_concepts ont1_sat); in
          (pre_concept_hash1, post_concept_hash1, ont1_index)
    ) ()
  in

  compact_heap ();

  assert(OntologyLanguage.is_horn ont2);

  let (pre_concept_hash2, post_concept_hash2, ont2_index) = 
    (fun _ -> (* we use a function here to 'hide' 'ont2_sat', which consumes a lot of memory *)
          print_endline("Saturating ontology 2...");
          let (ont2_sat, ont2_index) = ReasonerTBox.saturate ont2 in
          let (pre_concept_hash2, post_concept_hash2) = execute_measure_time "Computing pre and post (concepts) for ontology 2..."
                                                                            (fun _ -> ReasonerTBox.compute_pre_post_for_concepts ont2_sat); in
          (pre_concept_hash2, post_concept_hash2, ont2_index)
    ) ()
  in

  compact_heap ();*)

(*  (* Ontology.print_statistics ont1 stdout; *)
  (* Ontology.print_statistics ont2 stdout; *)
  if Debug.verbose_output () || Debug.debug_output () then
  begin
    Owl2IO.print_ontology_ch ont1 stdout; print_newline();
    Owl2IO.print_ontology_ch ont2 stdout; print_newline(); 
  end;
  if Debug.verbose_output () || Debug.debug_output () then
  begin
    print_endline("For ontology 1");
    StringSet.iter (fun cname -> print_endline ("concept " ^ cname);
                            print_string("\tpre_concepts(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_concepts pre_concept_hash1 cname); print_newline();
                            print_string("\tpre_domain(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_domain ont1 pre_concept_hash1 cname); print_newline();
                            print_string("\tpre_range(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_range ont1 pre_concept_hash1 cname); print_newline();
                            print_string("\tpost_concepts(" ^ cname ^ "): "); StringSet.print_set (Ontology.post_concepts post_concept_hash1 cname); print_newline()
                  )
                  (Ontology.get_concept_names ont1);
    StringSet.iter (fun rname -> print_endline ("role " ^ rname);
                            print_string("\tpre_role(" ^ rname ^ "): "); StringSet.print_set (RoleDifference.preRole (Ontology.get_RoleDifferenceInformation ont1) rname); print_newline();
                            print_string("\tpost_role(" ^ rname ^ "): "); StringSet.print_set (RoleDifference.postRole (Ontology.get_RoleDifferenceInformation ont1) rname); print_newline();
                  )
                  (Ontology.get_role_names ont1);
    print_endline("For ontology 2");
    StringSet.iter (fun cname -> print_endline ("concept " ^ cname);
                            print_string("\tpre_concepts(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_concepts pre_concept_hash2 cname); print_newline();
                            print_string("\tpre_domain(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_domain ont2 pre_concept_hash2 cname); print_newline();
                            print_string("\tpre_range(" ^ cname ^ "): "); StringSet.print_set (Ontology.pre_range ont2 pre_concept_hash2 cname); print_newline();
                            print_string("\tpost_concepts(" ^ cname ^ "): "); StringSet.print_set (Ontology.post_concepts post_concept_hash2 cname); print_newline()
                  )
                  (Ontology.get_concept_names ont2);
    StringSet.iter (fun rname -> print_endline ("role " ^ rname);
                            print_string("\tpre_role(" ^ rname ^ "): "); StringSet.print_set (RoleDifference.preRole (Ontology.get_RoleDifferenceInformation ont2) rname); print_newline();
                            print_string("\tpost_role(" ^ rname ^ "): "); StringSet.print_set (RoleDifference.postRole (Ontology.get_RoleDifferenceInformation ont2) rname); print_newline();
                  )
                  (Ontology.get_role_names ont2);
  end;
  print_endline ("Renamings used: " ^ (string_of_int (Normalisation.new_symbols_introduced ())));

  compact_heap ();*)

  let nameMapping = match !nameMappingFileNameList with
                    []  -> Hashtbl.create 1
                    | _ -> execute_measure_time ("Loading name mapping files...")
                                                (fun _ -> NameMapping.readMappingFiles (!nameMappingFileNameList)) in

  print_string "Running in ";

  if is_concept_case() then
    print_endline "Concept Mode."
  else if is_instance_case() then
    print_endline "Instance Mode."
  else if is_conjunctive_query_case() then
    print_endline "Conjunctive Query Mode.";

  print_endline "Checking whether there exist differences w.r.t. roles...";
  let (roleDifferences, roleDifferencesCount) = RoleDifference.computeRoleDifferences sigma (Terminology.get_role_difference_information terminology1)
                                                                                            (Terminology.get_role_difference_information terminology2) in
  List.iter (fun (r, s) -> output_axiom_with_mapping outputChannel (RoleInclusion(r, s)) nameMapping; output_string outputChannel "\n") roleDifferences;
  print_endline("Number of differences w.r.t. roles: " ^ (string_of_int roleDifferencesCount));

  print_endline "Checking whether there exist counter-examples with atomic RHS...";
  let (atomicRHSCounterExamples, atomicRHSCounterExampleWitnesses, atomicRHSDiffCount) = AtomicRHS.checkCEx sigma nameMapping outputChannel terminology1 terminology2 in
  print_endline "...done";

(* let (atomicRHSCounterExamples, atomicRHSCounterExampleWitnesses, atomicRHSDiffCount) = ([], StringSet.empty, 0) in *)

(*  print_endline "Checking whether there exist counter-examples with atomic LHS...";
  let (atomicLHSCounterExamples, atomicLHSCounterExampleWitnesses, atomicLHSDiffCount) = AtomicLHS.checkCEx sigma nameMapping outputChannel terminology1 terminology2 in
  print_endline "...done";*)


(*   let (atomicLHSCounterExamples, atomicLHSCounterExampleWitnesses, atomicLHSDiffCount) = *)
  let (lhsDiff, domainDiff, rangeDiff) =
    execute_measure_time ("Checking whether there exist counter-examples with atomic LHS...")
                         (fun _ -> AtomicLHS.checkCEx sigma nameMapping outputChannel terminology1 terminology2)
   in

  let (atomicLHSCounterExamples, atomicLHSCounterExampleWitnesses, atomicLHSDiffCount) = lhsDiff in
  let (roleDomainCounterExamples, domainLHSCounterExampleWitnesses, domainDiffCount) = domainDiff in
  let (roleRangeCounterExamples, rangeLHSCounterExampleWitnesses, rangeDiffCount) = rangeDiff in
(*   let (roleDomainCounterExamples, domainLHSCounterExampleWitnesses, domainDiffCount) = ([], StringSet.empty, 0) in *)
  let (roleRangeCounterExamples, rangeLHSCounterExampleWitnesses, rangeDiffCount) = ([], StringSet.empty, 0) in
  
(*  print_endline "Checking whether there exist counter-examples w.r.t. the domain of roles...";
  let (roleDomainCounterExamples, domainLHSCounterExampleWitnesses, domainDiffCount) = AtomicLHS.checkRoleDomainCEx sigma nameMapping outputChannel ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 !leftHandSideSetWithRoleBindings1
                                                                                                                                                    ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 !leftHandSideSetWithRoleBindings2 in
  print_endline "...done";

  print_endline "Checking whether there exist counter-examples w.r.t. the range of roles...";
  let (roleRangeCounterExamples, rangeLHSCounterExampleWitnesses, rangeDiffCount) = AtomicLHS.checkRoleRangeCEx sigma nameMapping outputChannel ont1 ont1_index pre_concept_hash1 post_concept_hash1 occurrenceHash1 !leftHandSideSetWithRoleBindings1
                                                                                                                                                ont2 ont2_index pre_concept_hash2 post_concept_hash2 occurrenceHash2 !leftHandSideSetWithRoleBindings2 in*)

  print_endline "...done";

  if outputChannel <> stdout then
    close_out outputChannel;

  print_endline("Number of concept name witnesses: " ^ string_of_int(StringSet.cardinal (StringSet.union atomicLHSCounterExampleWitnesses atomicRHSCounterExampleWitnesses)));

  if !extendedStatistics then
    Statistics.print_statistics ();

  (List.rev_append roleRangeCounterExamples
                   (List.rev_append roleDomainCounterExamples
                                    (List.rev_append atomicRHSCounterExamples atomicLHSCounterExamples)),
   roleDifferencesCount, atomicRHSDiffCount, atomicLHSDiffCount, domainDiffCount, rangeDiffCount, nameMapping)
  (* end of function body *)
  ) ()
in

if roleDifferencesCount > 0 || atomicRHSDiffCount > 0 || atomicLHSDiffCount > 0 || domainDiffCount > 0 || rangeDiffCount > 0 then
  begin
  print_newline ();
  print_endline "    Overall: Difference Found.";
  print_newline ()
  end
else
  begin
  print_newline ();
  print_endline "    Overall: No Difference Found.";
  print_newline ()
  end;

if !checkCounterExamples && !constructCounterExamples then
  begin
  compact_heap ();
  print_endline ("Verifying the correctness of the counter-examples...");
  SanityCheck.checkConceptCounterExamples nameMapping ont1 ont2 cexList;
  end;

let end_time = Unix.times() in
print_string ("Maximum allocated MB: "); print_float (Gc.allocated_bytes() /. float(1024 * 1024)); print_newline ();
print_total_time_difference start_time end_time; print_newline();

print_maximum_memory_usage()

(* kate: replace-tabs on; indent-width 2; *)
