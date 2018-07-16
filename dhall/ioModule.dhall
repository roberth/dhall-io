  λ(IO : Type → Type)
→ { pure :
      ∀(a : Type) → a → IO a
  , bind :
      ∀(b : Type) → ∀(a : Type) → IO b → (b → IO a) → IO a
  , writeLn :
      Text → IO {}
  , readLn :
      IO Text
  }
