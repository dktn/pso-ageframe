
### EKG

http://hackage.haskell.org/package/ekg-0.4.0.9/docs/System-Remote-Monitoring.html


### Modules

- repa https://ocharles.org.uk/blog/posts/2013-12-16-24-days-of-hackage-repa.html
- acid-state
- data-memocombinators
- linear
- configurator ?
- docopt
- criterion

MTL like:
- monads-tf (https://wiki.haskell.org/Monad_Transformer_Library, http://stackoverflow.com/questions/2769487/mtl-transformers-monads-fd-monadlib-and-the-paradox-of-choice)
mtl == transformers ++ monads-fd, mtl-tf == transformers ++ monads-tf

- monad-classes (ro-che) !
- transformers-eff (ocharles) !
- extensible-effects
- effect-handlers
- effects
- monadLib


### Paralelism
- lvish
- monad-par

### Extensions http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html
- https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns
  https://ocharles.org.uk/blog/posts/2014-12-02-view-patterns.html (Seq)
- DeriveGeneric, DeriveAnyClass http://hackage.haskell.org/package/deepseq-1.4.2.0/docs/Control-DeepSeq.html#t:NFData
- ExistentialQuantification  (data Foo = forall a. MkFoo a)
- RecordWildcards
- RankNTypes (Random https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html)
- TypeFamilies (equality constraints: X ~ Y)
- KindSignatures (id :: forall (a :: *). a -> a)
- GADTs

data Some :: * -> * where
    SomeInt  :: Int -> Some Int
    SomeChar :: Char -> Some Char
    Anything :: a -> Some a

- NoMonomorphismRestriction
- MultiParamTypeClasses
