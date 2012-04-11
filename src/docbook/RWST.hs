newtype ReaderWriterStateT r w s f a =
  ReaderWriterStateT {
    run :: (r, s) -> f (w, a, s)
  }

instance Functor f =>
  Functor (ReaderWriterStateT r w s f) where
  fmap f (ReaderWriterStateT x) =
    ReaderWriterStateT $
      fmap (\(w, a, s) -> (w, f a, s)) . x

instance (FlatMap f, Semigroup w) =>
  FlatMap (ReaderWriterStateT r w s f) where
  flatMap f (ReaderWriterStateT x) =
    ReaderWriterStateT $ \(r, s) ->
      flatMap (\(w1, a, s1) ->
        fmap (\(w2, b, s2) ->
          (op w1 w2, b, s2))
          (run (f a) (r, s1))) (x (r, s))

type ReaderWriterState r w s a =
  ReaderWriterStateT r w s Id a

newtype ReaderT a f b =
  ReaderT { rd :: a -> f b }

rwsR (ReaderT f) =
  ReaderWriterStateT $ \(r, s) ->
    fmap (\z -> (identity, z, s)) (f r)

type Reader a b =
  ReaderT a Id b

newtype WriterT w f a =
  WriterT { wx :: f (w, a) }

rwsW (WriterT x) =
  ReaderWriterStateT $ \(r, s) ->
    fmap (\(w, a) -> (w, a, s)) x

type Writer w a =
  WriterT w Id a

newtype StateT s f a =
  StateT { st :: s -> f (a, s) }

rwsS (StateT f) =
  ReaderWriterStateT $ \(r, s) ->
    fmap (\(a, s) -> (identity, a, s)) (f s)

type State s a =
  StateT s Id a

newtype Id a = Id a

instance Functor Id where
  fmap f (Id a) = Id (f a)

instance FlatMap Id where
  flatMap f (Id a) = f a

class Functor f => FlatMap f where
  flatMap :: (a -> f b) -> f a -> f b

class Semigroup a where
  op :: a -> a -> a

class Semigroup a => Monoid a where
  identity :: a