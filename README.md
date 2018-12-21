# Putting Curry-Howard to Work

This repository contains slides `slides.pdf` and code for my seminar talk,
based on paper *Putting Curry-Howard to Work* by Tim Sheard.

# Homework
1. implement `length    :: Seq n a -> SNat n`
2. implement `replicate :: SNat n -> a -> Seq n a`
3. implement `tail      :: Seq (S n) a -> Seq n`
4. implement `transpose :: Seq n (Seq m a) -> Seq m (Seq n a)`
