Require Import Coq.Lists.List.
Require Import Coq.Strings.Ascii.
Require Import Coq.Strings.String.

Fixpoint add_final_slash (s : string) : string :=
  match s with
    | EmptyString => "/"%string
    | String "/"%char EmptyString => "/"%string
    | String c EmptyString => String c "/"%string
    | String c s => String c (add_final_slash s)
  end.

Fixpoint strip_prefix (p : string) (s : string) : option string :=
  match p, s with
    | EmptyString, _ => Some s
    | String _ _, EmptyString => None
    | String p1 p', String s1 s' =>
        if ascii_dec p1 s1 then
          strip_prefix p' s'
        else
          None
  end.

Fixpoint get_justs {t : Type} (l : list (option t)) : list t :=
  match l with
  | nil => nil
  | (None::r) => get_justs r
  | (Some h::r) => h::get_justs r
  end.

Fixpoint no_slashes (s : string) : bool :=
  match s with
  | EmptyString => true
  | String "/"%char _ => false
  | String _ s' => no_slashes s'
  end.

Fixpoint ascii_leb (c1 : ascii) (c2 : ascii) : bool :=
  let a1 := nat_of_ascii c1 in
  let a2 := nat_of_ascii c2 in
  Nat.leb a1 a2.

Fixpoint string_leb (s1 : string) (s2 : string) : bool :=
  match s1, s2 with
  | EmptyString, EmptyString => true
  | EmptyString, String _ _ => true
  | String _ _, EmptyString => false
  | String c1 s1', String c2 s2' =>
      if ascii_leb c1 c2 then
        if ascii_leb c2 c1 then
          string_leb s1' s2'
        else
          true
      else
        false
  end.
