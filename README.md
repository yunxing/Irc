Irc is a monadic DSL and library for writing Irc bots.

It conveniently utilizes 'do' notation in the style similar to [Shake](https://hackage.haskell.org/package/shake)

You can write something like:
```haskell
    main :: IO ()
    main = mainWithConfigAndBehavior (Config
                                   "irc.freenode.org"
                                   6667
                                   "#yunbot-testing"
                                   "yunbot") $ do
          "!echo " |! return . drop 6
          "!reverse " |! return . reverse . drop 9
```
