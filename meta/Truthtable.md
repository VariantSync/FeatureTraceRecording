## Truth Tables for the Ternary Logic With Null by Sobociński [71, p.70][79]

Internally, we represent nullable propositional formula as a `Maybe` of a propositional formula (in [`src/propositions/NullPropositions.hs`](src/propositions/NullPropositions.hs)):
```haskell
type NullableFormula a = Maybe (PropositionalFormula a)
```
So, a nullable formula is either null (represented by the value `Nothing`) or a propositional formula `p` (represented by the value `Just p`).
We write `⊤` for `true` and `⊥` for `false`.

    ¬
    _________________
    Nothing | Nothing
    Just ⊥  | Just ⊤
    Just ⊤  | Just ⊥


    ∧
    _________________________
    Nothing Nothing | Nothing
    Nothing Just ⊥  | Just ⊥
    Nothing Just ⊤  | Just ⊤
    Just ⊥  Nothing | Just ⊥
    Just ⊥  Just ⊥  | Just ⊥
    Just ⊥  Just ⊤  | Just ⊥
    Just ⊤  Nothing | Just ⊤
    Just ⊤  Just ⊥  | Just ⊥
    Just ⊤  Just ⊤  | Just ⊤


    ∨
    _________________________
    Nothing Nothing | Nothing
    Nothing Just ⊥  | Just ⊥
    Nothing Just ⊤  | Just ⊤
    Just ⊥  Nothing | Just ⊥
    Just ⊥  Just ⊥  | Just ⊥
    Just ⊥  Just ⊤  | Just ⊤
    Just ⊤  Nothing | Just ⊤
    Just ⊤  Just ⊥  | Just ⊤
    Just ⊤  Just ⊤  | Just ⊤


    ⇒
    _________________________
    Nothing Nothing | Nothing
    Nothing Just ⊥  | Just ⊥
    Nothing Just ⊤  | Just ⊤
    Just ⊥  Nothing | Just ⊤
    Just ⊥  Just ⊥  | Just ⊤
    Just ⊥  Just ⊤  | Just ⊤
    Just ⊤  Nothing | Just ⊥
    Just ⊤  Just ⊥  | Just ⊥
    Just ⊤  Just ⊤  | Just ⊤


    ⇔
    _________________________
    Nothing Nothing | Nothing
    Nothing Just ⊥  | Just ⊥
    Nothing Just ⊤  | Just ⊥
    Just ⊥  Nothing | Just ⊥
    Just ⊥  Just ⊥  | Just ⊤
    Just ⊥  Just ⊤  | Just ⊥
    Just ⊤  Nothing | Just ⊥
    Just ⊤  Just ⊥  | Just ⊥
    Just ⊤  Just ⊤  | Just ⊤
