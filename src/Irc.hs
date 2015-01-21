-- | Irc is a monadic DSL and library for writing Irc bots.
--   It conveniently utilizes 'do' notation in the style of
--   <https://hackage.haskell.org/package/shake Shake>
--
module Irc
    ( Behavior
    , BehaviorM
    , Config(..)
    , mainWithConfigAndBehavior
    , (|!))
    where
import Irc.Internal( Behavior
                   , BehaviorM
                   , Config(..)
                   , mainWithConfigAndBehavior
                   , (|!))
