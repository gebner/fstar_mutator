module Mutate
open FStar.Compiler
open FStar.Compiler.Effect
open FStar.CheckedFiles
open FStar.Compiler.List
module Set = FStar.Compiler.FlatSet
module BU = FStar.Compiler.Util
module SMT = FStar.SMTEncoding.Solver
module TcEnv = FStar.TypeChecker.Env
module TcTerm = FStar.TypeChecker.TcTerm
module Rel = FStar.TypeChecker.Rel
module NBE = FStar.TypeChecker.NBE
module List = FStar.Compiler.List
open FStar.Syntax.Syntax
module U = FStar.Syntax.Util
module SS = FStar.Syntax.Subst
module P = FStar.Syntax.Print

let print_stderr f l = BU.fprint BU.stderr f l

let main () =
  FStar.Main.setup_hooks ();
  let usage () = print_stderr "Usage: fstar_mutator.exe\n" [] in
  usage ();
  exit 1

let _: unit = main ()
