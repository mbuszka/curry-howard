# Putting Curry-Howard to Work

This repository contains slides `slides.pdf` and code for my seminar talk,
based on paper *Putting Curry-Howard to Work* by Tim Sheard.

Code can built using `stack build`.

# Homework
1. implement `length    :: Seq n a -> SNat n`
2. implement `replicate :: SNat n -> a -> Seq n a`
3. implement `tail      :: Seq (S n) a -> Seq n`
4. 
   This signature (`transpose :: Seq n (Seq m a) -> Seq m (Seq n a)`) is too restrictive, as we cannot match on the length of inner list when outer list is Nil.
   Instead we could implement:
   -  `transpose :: SNat m -> Seq n (Seq m a) -> Seq m (Seq n a)` which requires length of inner sequence to be given
   - `transpose :: Seq (S n) (Seq m a) -> Seq m (Seq (S n) a)` and require outer sequence to be non-empty
   - pass the length of inner sequence via typeclass
     ```
     class SSNat (n :: Nat) where
       witness :: SNat n
     
     instance SSNat Z where
       witness = SZ 
     
     instance (SSNat n) => SSNat (S n) where
       witness = SS witness

     transpose :: SSNat m => Seq n (Seq m a) -> Seq m (Seq n a)
     ```
