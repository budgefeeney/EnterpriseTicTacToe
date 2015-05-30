module FSharpisms where

  (|>) :: b -> (b -> c) -> c
  (|>) = flip ($)

  (<|) :: (b -> c) -> b -> c
  (<|) = ($)

  (.>), andThen :: (a -> b) -> (b -> c) -> a -> c
  (.>) = flip (.)
  andThen = flip(.)

  (<.) :: (b -> c) -> (a -> b) -> a -> c
  (<.) = (.)
