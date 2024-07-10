module Mutate
open FStar.Compiler
open FStar.Compiler.Effect
open FStar.Compiler.List
module Set = FStar.Compiler.FlatSet
module BU = FStar.Compiler.Util
module List = FStar.Compiler.List
module A = FStar.Parser.AST
open FStar.Class.Show
module R = FStar.Compiler.Range

let mut_here_p = unit -> ML bool

let count_mut_pts (k : mut_here_p -> ML 'a) : nat =
  let n = BU.mk_ref 0 in
  let _ = k (fun _ -> n := !n + 1; false) in
  !n

let mut_at i (k : mut_here_p -> ML 'a) : ML 'a =
  let n = BU.mk_ref 0 in
  k (fun _ -> let vn = !n in n := vn + 1; vn = i)

let range (i: nat) : list nat =
  let rec go (i: nat) (acc: list nat) : list nat =
    if i = 0 then acc else go (i-1) ((i-1)::acc) in
  go i []

open FStar.Class.Setlike

let sample_k_rand_muts k (f: mut_here_p -> ML 'a) : ML (list 'a) =
  let n = count_mut_pts f in
  if n = 0 then [] else
  let is = List.map #nat #int (fun _ -> Rand.full_int n) (range k) in
  let is = elems (from_list is <: Set.flat_set int) in
  List.map (fun i -> mut_at i f) is

open FStar.Parser.AST

let filter_mut (mh: mut_here_p) =
  List.filter (fun _ -> not (mh ()))

let is_single_line (r: R.range) : bool =
  R.line_of_pos (R.start_of_range r) = R.line_of_pos (R.end_of_range r)

let rec mut_term' (mh: mut_here_p) (t: term') : ML term' =
  match t with
  | Op (id, ts) -> Op (id, List.map (mut_term mh) ts)
  | Construct (i, ts) -> Construct (i, List.map (fun (t,i) -> (mut_term mh t, i)) ts)
  | Abs (ps, t) -> Abs (List.map (mut_pattern mh) ps, mut_term mh t)
  | App (a, b, i) ->
    if mh () then a.tm else
    App (mut_term mh a, mut_term mh b, i)
  | Let (q, ps, t) ->
    Let (q, List.map (fun (a,(p,t)) -> (a,(mut_pattern mh p, mut_term mh t))) ps, mut_term mh t)
  | LetOperator (ps, t) ->
    LetOperator (List.map (fun (i,p,t) -> (i,mut_pattern mh p, mut_term mh t)) ps, mut_term mh t)
  | LetOpen (i, t) ->
    if mh () then t.tm else
    LetOpen (i, mut_term mh t)
  | LetOpenRecord (a, b, c) ->
    LetOpenRecord (mut_term mh a, mut_term mh b, mut_term mh c) // TODO
  | Seq (a, b) ->
    if mh () then b.tm else
    Seq (mut_term mh a, mut_term mh b)
  | Bind (i, a, b) ->
    Bind (i, mut_term mh a, mut_term mh b)
  | If (c, i,j, a, b) ->
    if mh () then a.tm else
    if mh () then b.tm else
    If (mut_term mh c, i,j, mut_term mh a, mut_term mh b)
  | Match (t, i,j, bs) ->
    Match (mut_term mh t, i,j, mut_branches mh (filter_mut mh bs))
  | TryWith (t, bs) ->
    if mh () then t.tm else
    TryWith (mut_term mh t, mut_branches mh (filter_mut mh bs))
  | Ascribed (a,t,s,b) ->
    if mh () then a.tm else
    Ascribed (mut_term mh a, mut_term mh t, BU.map_option (mut_term mh) s, b)
  | Record (src, flds) ->
    Record (BU.map_option (mut_term mh) src,
      List.map (fun (i, t) -> (i, mut_term mh t)) (filter_mut mh flds))
  | Project (t, fld) ->
    if mh () then t.tm else
    Project (mut_term mh t, fld)
  | Product (bs, t) ->
    Product (List.map (mut_binder mh) bs, mut_term mh t)
  | Sum (bs, t) ->
    let f = function | Inl b -> Inl (mut_binder mh b) | Inr t -> Inr (mut_term mh t) in
    Sum (List.map f bs, mut_term mh t)
  | QForall (bs, ps, t) ->
    if mh () then QExists (bs, ps, t) else
    QForall (List.map (mut_binder mh) bs, mut_patterns mh ps, mut_term mh t)
  | QExists (bs, ps, t) ->
    if mh () then QForall (bs, ps, t) else
    QExists (List.map (mut_binder mh) bs, mut_patterns mh ps, mut_term mh t)
  | QuantOp (i, bs, ps, t) ->
    QuantOp (i, List.map (mut_binder mh) bs, mut_patterns mh ps, mut_term mh t)
  | Refine (b, refine) ->
    Refine (mut_binder mh b, mut_term mh refine)
  | NamedTyp (i, t) ->
    NamedTyp (i, mut_term mh t)
  | Paren t ->
    if mh () then t.tm else
    Paren (mut_term mh t)
  | Requires (t, s) -> Requires (mut_term mh t, s)
  | Ensures (t, s) -> Ensures (mut_term mh t, s)
  | LexList ts -> LexList (List.map (mut_term mh) ts)
  | WFOrder (t, s) -> WFOrder (mut_term mh t, mut_term mh s)
  | Decreases (t, s) -> Decreases (mut_term mh t, s)
  | Labeled (t, s, b) -> Labeled (mut_term mh t, s, b)
  // TODO: quotes, calc, introduce, eliminate
  | _ -> t
and mut_branches (mh: mut_here_p) : list branch -> ML (list branch) =
  List.map (fun (p, t, s) -> (mut_pattern mh p, BU.map_option (mut_term mh) t, mut_term mh s))
and mut_patterns (mh: mut_here_p) : patterns -> ML patterns =
  fun (is, ts) -> (is, List.map (List.map (mut_term mh)) ts)
and mut_pattern' (mh: mut_here_p) (p: pattern') : ML pattern' =
  match p with
  | PatList ps -> PatList (List.map (mut_pattern mh) ps)
  | PatTuple (ps, d) -> PatTuple (List.map (mut_pattern mh) ps, d)
  | PatRecord ps ->
    PatRecord (List.map (fun (i, p) -> (i, mut_pattern mh p)) ps)
  | PatAscribed (p, (a, a')) ->
    if mh () then p.pat else
    PatAscribed (mut_pattern mh p, (mut_term mh a, BU.map_option (mut_term mh) a'))
  | PatOr ps -> PatOr (List.map (mut_pattern mh) ps)
  | _ -> p
and mut_binder' (mh: mut_here_p) (b: binder') : ML binder' =
  match b with
  | Annotated (i, t) ->
    if mh () then Variable i else
    Annotated (i, mut_term mh t)
  | TAnnotated (i, t) ->
    if mh () then TVariable i else
    TAnnotated (i, mut_term mh t)
  | NoName t -> NoName (mut_term mh t)
  | _ -> b
and mut_term (mh: mut_here_p) (t: term) : ML term =
  if is_single_line t.range && mh () then { t with tm = Wild } else
  { t with tm = mut_term' mh t.tm }
and mut_pattern (mh: mut_here_p) (p: pattern) : ML pattern =
  { p with pat = mut_pattern' mh p.pat }
and mut_binder (mh: mut_here_p) (b: binder) : ML binder =
  { b with b = mut_binder' mh b.b }

let mut_decl' (mh: mut_here_p) (d: decl') : ML decl' =
  match d with
  | TopLevelLet (q, ps) ->
    TopLevelLet (q, List.map (fun (p, t) -> (mut_pattern mh p, mut_term mh t)) ps)
  | _ -> d

let mut_decl (mh: mut_here_p) (d: decl) : ML decl =
  { d with d = mut_decl' mh d.d }

let mut_frag (mh: mut_here_p) (f: Parser.Driver.fragment) : ML Parser.Driver.fragment =
  match f with
  | Parser.Driver.Empty -> Parser.Driver.Empty
  | Parser.Driver.Decls decls -> Parser.Driver.Decls (List.map (mut_decl mh) decls)

let print_stderr f l = BU.fprint BU.stderr f l
let print_stdout f l = BU.fprint BU.stdout f l

let read_all_of_stdin () =
  let stdin = BU.open_stdin () in
  let rec go (acc: string) : string =
    match BU.read_line stdin with
    | None -> acc
    | Some line -> go (acc ^ line) in
  go ""

let print_decl (d: decl) : string =
  Pprint.render (Parser.ToDocument.decl_to_document d)

open FStar.Json

let main () =
try
  FStar.Main.setup_hooks ();
  Rand.self_init ();
  let usage () = print_stderr "Usage: fstar_mutator.exe\n" [] in
  let stdin = read_all_of_stdin () in
  let frag = Parser.Driver.parse_fragment { frag_fname = "<input>"; frag_text = stdin; frag_line = 1; frag_col = 0 } in
  match frag with
  | Parser.Driver.Decls [decl] -> begin
    print_stderr "%s\n" [Pprint.render (Parser.ToDocument.decl_to_document decl)];
    let n: int = 10 in
    let muts = sample_k_rand_muts 100 (fun mh -> mut_decl mh decl) in
    let j = JsonAssoc [
      "original", JsonStr (print_decl decl);
      "mutations", JsonList (List.map (fun mut -> JsonAssoc [
        "mutation", JsonStr (print_decl mut);
      ]) muts);
    ] in
    print_stdout (string_of_json j) [];
    ()
  end
  | _ ->
    usage ();
    exit 1;
    ()
with
| Errors.Error(e, msg, r, _ctx) ->
  BU.print1 "Error: %s\n" (Errors.rendermsg msg);
  exit 1
| e ->
  print_stderr "Exception: %s\n" [BU.print_exn e];
  exit 1

let _ = main ()
