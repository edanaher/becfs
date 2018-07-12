Require Extraction.
Require Import Coq.Lists.List.
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

  Definition Tree t := list (Key * t).

  Definition empty {t : Type} : (Tree t) := nil.

  Definition add {t : Type} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    (k, v) :: tr.

  Definition remove {t : Type} (tr : Tree t) (k : Key) : Tree t :=
    filter (fun kv' => match kv' with pair k' _ => negb (key_beq k k') end) tr.
    
  Definition update {t : Type} (tr : Tree t) (k : Key) (v : t) : Tree t :=
    add (remove tr k) k v.

  Definition lookup {t : Type} (tr : Tree t) (k : Key) : option t :=
    match (find (fun kv' => match kv' with (k', v) => key_beq k k' end) tr) with
      | None => None
      | Some (k, v) => Some v
    end.

End Tree.

Extraction Language Haskell.
Require Import Coq.extraction.ExtrHaskellBasic.
Require Import Coq.extraction.ExtrHaskellString.
Extraction "ListCoq.hs" Tree.
