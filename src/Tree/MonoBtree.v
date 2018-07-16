Require Extraction.
Require Import Util.
Require Import List.
Require Import Coq.Lists.List.
Require Import Coq.Program.Basics.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.

Module Tree.
  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

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

  (* TODO: Use lt/gt instead of string_dec. *)
  Definition lt_key (a : Key) (b : Key) : bool :=
    match a, b with
    | Meta s1, Meta s2 => if string_dec s1 s2 then true else false
    | Data s1, Data s2 => if string_dec s1 s2 then true else false
    | Meta _, Data _ => true
    | Data _, Meta _ => false
    end.


  Definition lt_key_pair {t : Set} (a : Key * t) (b : Key * t) : bool :=
    lt_key (fst a) (fst b).

  Fixpoint insertSorted {t : Type} (lt : t -> t -> bool) (l : list t) (v : t) : list t :=
    match l with
    | nil => v::nil
    | (h::r) =>
        if lt v h then
          v::h::r
        else
          h::insertSorted lt r v
    end.

  Inductive Tree (t : Set) : Set := 
  | InternalNode : list (option Key * t) -> Tree t
  | Leaf : list (Key * t) -> Tree t.

  Arguments InternalNode {t} _.
  Arguments Leaf {t} _.

  Definition empty {t : Set} : Tree t := Leaf nil.

  Definition add {t : Set} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    match tr with
    | Leaf vs => Leaf (insertSorted lt_key_pair vs (k, v))
    | InternalNode ks => tr
    end.

  (*
  Definition remove {t : Type} (tr : Tree t) (k : Key) : Tree t :=
    filter (fun kv' => match kv' with pair k' _ => negb (key_beq k k') end) tr.
    
  Definition update {t : Type} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    add (remove tr k) k v.

  Definition lookup {t : Type} (tr : Tree t) (k : Key) : option t :=
    match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) tr) with
      | None => None
      | Some (k, v) => Some v
    end.

  Definition children {t : Type} (tr : Tree t) (k : Key) :=
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
      filter (compose no_slashes keyPath) (get_justs (map (compose getSuffix fst) tr)).*)
End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant map => "Prelude.map".
Extraction "MonoBtreeCoq.hs" Tree.
