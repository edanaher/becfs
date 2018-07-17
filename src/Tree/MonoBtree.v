Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.

Module Tree.
  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

  Definition max_node_size := 3.

  Definition key_beq (k1 : Key) (k2 : Key) : bool :=
    match (k1, k2) with
      | (Meta s, Meta s') => if string_dec s s' then true else false
      | (Data s, Data s') => if string_dec s s' then true else false
      | _ => false
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

  Definition leb_key (a : Key) (b : Key) : bool :=
    match a, b with
    | Meta s1, Meta s2 => if string_leb s1 s2 then true else false
    | Data s1, Data s2 => if string_leb s1 s2 then true else false
    | Meta _, Data _ => true
    | Data _, Meta _ => false
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

  Inductive Tree (t : Set) : Set :=
  | InternalNode : list (option Key * (Tree t)) -> Tree t
  | Leaf : list (Key * t) -> Tree t.

  Arguments InternalNode {t} _.
  Arguments Leaf {t} _.

  Definition empty {t : Set} : Tree t := Leaf [].

  Definition lastKey {t : Set} (kvs : list (Key * t)) : Key :=
    let justs := map (fun kv => Some kv) kvs in
    match last justs None with
    | None => Meta "__DOESNOTEXIST__"
    | Some kv => fst kv
    end.

  Fixpoint insertNode {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    match tr with
    | Leaf vs =>
        let newList := (insertSorted leb_key_pair vs (k, v)) in
        if Nat.eqb (List.length vs) max_node_size then
          let splitPoint := Nat.div (List.length newList) 2 in
          let front := List.firstn splitPoint newList in
          let back := List.skipn splitPoint newList in
          InternalNode [(Some (lastKey front), Leaf front); (None, Leaf back)]
        else
          Leaf newList
    | InternalNode ks =>
        InternalNode
        ((fix insertList (ks : list (option Key * Tree t)) : list (option Key * Tree t) :=
          match ks with
          | [] => [] (* Impossibile *)
          | ((Some k', tr')::ns) =>
              if leb_key k k' then
                (Some k, insertNode tr' k v)::ns
              else
                (Some k, tr')::insertList ns
          | ((None, tr')::ns) => (None, insertNode tr' k v)::ns
          end
        ) ks)
    end.

  Definition insert {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    insertNode tr k v.

  Definition remove {t : Set} (tr : Tree t) (k : Key) : Tree t :=
    match tr with
    | Leaf vs => Leaf (filter (fun kv' => match kv' with pair k' _ => negb (key_beq k k') end) vs)
    | InternalNode _ => tr
    end.

  Definition update {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    insert (remove tr k) k v.

  Definition lookup {t : Set} (tr : Tree t) (k : Key) : option t :=
    match tr with
    | Leaf vs =>
        match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) vs) with
          | None => None
          | Some (k, v) => Some v
        end
    | InternalNode _ => None
    end.

  Definition children {t : Set} (tr : Tree t) (k : Key) :=
    let prefixSlash := add_final_slash (keyPath k) in
    let getSuffix := fun mpath => match mpath with
          | Meta path => match strip_prefix prefixSlash path with
              | None => None
              | Some s => Some (Meta s)
              end
          | Data path => match strip_prefix prefixSlash path with
              | None => None
              | Some s => Some (Data s)
              end
          end
    in
      match tr with
      | Leaf vs => filter (compose no_slashes keyPath) (get_justs (map (compose getSuffix fst) vs))
      | InternalNode _ => []
      end.

  Definition sampleTree :=
    let t := insert empty (Meta "a") "av"%string in
    let t := insert t (Meta "c") "cv"%string in
    let t := insert t (Meta "b") "bv"%string in
    let t := insert t (Meta "d") "dv"%string in
    let t := insert t (Meta "ab") "abv"%string in
    let t := insert t (Meta "z") "zv"%string in
    let t := insert t (Meta "q") "q"%string in
    t.
End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extraction "MonoBtreeCoq.hs" Tree.
