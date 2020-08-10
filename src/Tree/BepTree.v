Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.
Open Scope string_scope.

Module Tree.
  Definition trace {t : Type} (x : string) (y : t) : t := y.
  Definition show {t : Type} (v : t) : string := "".

  Inductive RawIO (t : Set) : Set :=
  | RawReturn : t -> RawIO t.
  Arguments RawReturn {t} _.

  Definition IO (t : Set) : Set := RawIO t.
  Definition ret {t : Set} (x : t) : IO t := RawReturn x.

  Definition bind {t : Set} {r : Set} (x : IO t) (f : t -> IO r) : IO r :=
    match x with
    | RawReturn v => f v
    end.
  Notation "v >>= f" := (bind (v) (f)) (at level 90, left associativity).
  Notation "v >> f" := (bind (v) (fun _ => f)) (at level 90, left associativity).


  Definition NodeId : Set := nat.

  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

  Definition showKey (k : Key) : string := match k with
  | Meta s => "Meta " ++ s
  | Data s => "Data " ++ s
  end.

  Definition Children := list (Key * NodeId).

  Inductive JournalEntry (t : Set) : Set := 
  | Insert : Key -> t -> JournalEntry t
  | Delete : Key -> JournalEntry t.

  Arguments Insert {t} _ _.
  Arguments Insert {t} _.

  Definition Journal (t : Set) := list (JournalEntry t).

  Inductive Tree (t : Set) : Set :=
  | InternalNode : NodeId -> Journal t -> Children -> Tree t
  | Leaf : NodeId -> Journal t -> list (Key * t) -> Tree t.

  Arguments InternalNode {t} _ _.
  Arguments Leaf {t} _.

  Definition empty {t : Set} : Tree t := Leaf 0 [] [].


  Inductive WriteStatus (t : Set) : Set :=
  | WriteSuccess : Tree t -> list (Tree t) -> WriteStatus t
  | IWriteSuccess : list (Tree t) -> list (Tree t) -> nat -> WriteStatus t
  | WriteError : string -> WriteStatus t.

  Arguments WriteSuccess {t} _ _.
  Arguments IWriteSuccess {t} _ _ _.
  Arguments WriteError {t} _.

  Inductive ReadStatus (res : Set) : Set :=
  | ReadSuccess : res -> ReadStatus res
  | ReadDoesNotExist : ReadStatus res
  | ReadError : string -> ReadStatus res.

  Arguments ReadSuccess {res} _.
  Arguments ReadDoesNotExist {res}.
  Arguments ReadError {res} _.

  Definition max_node_size := 3.

  Definition invalidKey str := Meta ("__DOESNOTEXIST__" ++ str).

  Definition key_beq (k1 : Key) (k2 : Key) : bool :=
    match (k1, k2) with
      | (Meta s, Meta s') => if string_dec s s' then true else false
      | (Data s, Data s') => if string_dec s s' then true else false
      | _ => false
    end.

  Definition getNodeId {t : Set} (node : Tree t) : NodeId :=
    match node with
    | Leaf id _ _ => id
    | InternalNode id _ _ => id
    end.

  Definition getNodeFirstKey {t : Set} (node : Tree t) : Key :=
    let firstKey {v : Set} (kvs : list (Key * v)) : Key := match kvs with
      | ((k,v)::kvs) => k
      | _ => invalidKey "firstkey" (* Impossible? *)
    end in
    match node with
    | Leaf _ _ kvs => (firstKey kvs)
    | InternalNode _ _ kvs => firstKey kvs
    end.

  Definition option_key_beq (k1 : Key) (k2 : option Key) : bool :=
    match k2 with
    | None => false
    | Some k2' => key_beq k1 k2'
    end.

  Definition keyPath (k : Key) : string :=
    match k with
    | Meta s => s
    | Data s => s
  end.

  Definition onKeyPath (op : string -> string) (k : Key) : Key :=
    match k with
    | Meta s => Meta (op s)
    | Data s => Data (op s)
    end.

  Fixpoint countSlashes (path : string) : nat :=
    match path with
    | EmptyString => 0
    | String c path' => (if Ascii.eqb c "/" then 1 else 0) + countSlashes path'
    end.

  Definition leb_key (a : Key) (b : Key) : bool :=
    match a, b with
    | Meta s1, Meta s2 => match Nat.compare (countSlashes s1) (countSlashes s2) with
                          | Lt => true
                          | Gt => false
                          | Eq => string_leb s1 s2
                          end
    | Data s1, Data s2 => if string_leb s1 s2 then true else false
    | Meta _, Data _ => true
    | Data _, Meta _ => false
    end.

  Definition leb_option_key (a : Key) (b : option Key) : bool :=
    match b with
    | None => true
    | Some b' => leb_key a b'
    end.


  Definition leb_key_pair {t : Set} (a : Key * t) (b : Key * t) : bool :=
    leb_key (fst a) (fst b).

  Fixpoint insertSorted {t : Type} (lt : t -> t -> bool) (l : list t) (v : t) : list t :=
    match l with
    | [] => [v]
    | (h::r) =>
        if lt v h then
          v::h::r
        else
          h::insertSorted lt r v
    end.

  Fixpoint lookup {t : Set} (gas : nat) (fetchNode : NodeId -> IO (Tree t)) (tr : Tree t) (k : Key) : IO (ReadStatus t) :=
    ret ReadDoesNotExist.

  (*Fixpoint findChildId (children : list (Key * NodeId)) (k : Key) : (list (Key * NodeId)) * (Key * NodeId) * (list (Key * NodeId)) :=
    match children with
    | (candidate::children') =>
        match children' with
        | [] => ([], candidate, [])
        | ((k', id)::children'') =>
            if leb_key k' k then
              match findChildId children' k with
              | (pre, (minKey, id'), post) => (candidate::pre, (minKey, id'), post)
              end
            else
              ([], (candidate), ((k', id)::children''))
        end
    | _ => ([], (invalidKey "childId", 0), []) (* Impossible *)
    end.

  Definition splitList {t : Set} (nodes : list (Key * t)) : (list (Key * t) * list (Key * t)):=
    let splitPoint := Nat.div (List.length nodes) 2 in
    let front := List.firstn splitPoint nodes in
    let back := List.skipn splitPoint nodes in
    (front, back).*)

  Fixpoint childEntries {t : Set} (tr : Tree t) (s : string) : (ReadStatus (list (Key * t))) :=
    (ReadError "Unimplemented").

  Fixpoint insertNode {t : Set} (gas : nat) (fetchNode : NodeId -> IO (Tree t)) (tr : Tree t) (k : Key) (v : t) (nextId : NodeId) : IO (WriteStatus t) :=
    ret (WriteError "Unimplemented").

  Definition insert {t : Set} (fetchNode : NodeId -> IO (Tree t)) (tr : Tree t) (k : Key) (v : t) (nextId : NodeId) : IO (WriteStatus t) :=
    ret (WriteError "Unimplemented").

  Fixpoint removeNode {t : Set} (gas : nat) (fetchNode : NodeId -> IO (Tree t)) (tr : Tree t) (k : Key) (nextId : NodeId) : IO (WriteStatus t) :=
    ret (WriteError "Unimplemented").


  Definition remove {t : Set} (fetchNode : NodeId -> IO (Tree t)) (tr : Tree t) (k : Key) (nextId : NodeId) : IO (WriteStatus t) :=
    ret (WriteError "Unimplemented").


End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Require Import Coq.extraction.ExtrHaskellNatInt.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extract Inlined Constant Tree.trace => "trace".
Extract Inlined Constant Tree.show => "Prelude.show".
Extract Inlined Constant Tree.NodeId => "Prelude.Int".
(*Extract Inductive Tree.RawIO => "IOSHOULDNTBEUSED" [ "RETURNSHOULDNTBEUSED" ].*)
Extract Inlined Constant Tree.ret => "Prelude.return".
Extract Constant Tree.IO "t" => "Prelude.IO t".
Extract Inlined Constant Tree.bind => "( Prelude.>>= )".
(*Extract Inlined Constant Tree.trace => "(\ x y -> y)".*)
(*Extract Inlined Constant Tree.show => "(\ _ -> """")".*)
Extraction "BepTreeCoq.hs" Tree.
