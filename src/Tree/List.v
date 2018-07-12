Require Extraction.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

Module Tree.
  Inductive Key : Set :=
  | Meta : string -> Key
  | Data : string -> Key.

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
    

End Tree.

Extraction Language Haskell.
Extract Inductive string => "Prelude.String" [ "[]" "(:)" ] "".
Extract Inductive list => "([])" [ "[]" "(:)" ] "".
Extract Inductive prod => "(,)" [ "(,)" ] "".
(*Extract Inductive Prod => "Prelude.String" [ "[]" "(:)" ] "".*)
Extraction "List.hs" Tree.
