-- | Irc is a monadic DSL and library for writing Irc bots.
--   It conveniently utilizes 'do' notation in the style of
--   <https://hackage.haskell.org/package/shake Shake> and
--   <https://hackage.haskell.org/package/clay clay>
--

module Irc
    ( Behavior
    , Config(..)
    , mainWithConfigAndBehavior
    , (|!))
    where
import Irc.Internal( Behavior
    , Config(..)
    , mainWithConfigAndBehavior
    , (|!))
