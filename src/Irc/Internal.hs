{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Irc.Internal where
import Data.String ( IsString(..) )
import Data.Default ( Default(..) )
import Control.Applicative (Applicative)
import Control.Monad.Trans.State as State
        ( State
        , modify
        , execState )
import Control.Exception
import Control.Monad.Reader
import Data.List ( find
                 , isPrefixOf)
import Network ( PortID(PortNumber)
               , connectTo)
import System.IO ( Handle
                 , hClose
                 , hSetBuffering
                 , BufferMode(..)
                 , hFlush
                 , stdout
                 , hGetLine
                 )
import Text.Printf( hPrintf
                  , printf)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>>

--
-- | The Irc monad is an IO monad wrapped by a readerT, which
--   contains the bot's immutable state (Rule, socket connection,
--   and config)
--
type Irc = ReaderT Bot IO
data Bot = Bot { rules :: [Rule]
               , config :: Config
               , socket :: Handle}

--
-- | The Config struct represents the Irc configuration
--
data Config = Config { server :: String
                     , port :: Integer
                     , chan :: String
                     , nick :: String}

--
-- | Set up actions to run on start and end, and run the main loop.
--   as an example:
--
--   > main :: IO ()
--   > main = mainWithConfigAndBehavior (Config
--   >                                  "irc.freenode.org"
--   >                                  6667
--   >                                  "#yunbot-testing"
--   >                                  "yunbot") $ do
--   >         "!echo " |! return . drop 6
--   >         "!reverse " |! return . reverse . drop 9
--
mainWithConfigAndBehavior :: Config -> Behavior -> IO ()
mainWithConfigAndBehavior conf bev = bracket (connect conf bev) disconnect loop
  where
    disconnect = hClose . socket
    loop       = runReaderT run

--
-- | Connect to the server and return the initial bot state
--
connect :: Config -> Behavior -> IO Bot
connect conf bev = notify $ do
    h <- connectTo (server conf) (PortNumber (fromIntegral (port conf)))
    hSetBuffering h NoBuffering
    return (Bot (runBevhavior bev) conf h)
  where
    notify = bracket_
        (printf "Connecting to %s ... " (server conf) >> hFlush stdout)
        (putStrLn "done.")

--
-- | We're in the Irc monad now, so we've connected successfully
--   Join a channel, and start processing commands
--
run :: Irc ()
run = do
    conf <- asks config
    write "NICK" $ nick conf
    write "USER" $ nick conf ++ " 0 * :bot"
    write "JOIN" $ chan conf
    asks socket >>= listen

--
-- | Process each line from the server
--
listen :: Handle -> Irc ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

--
-- | Dispatch a command
--
eval :: String -> Irc ()
eval s = do
    r <- asks rules
    liftAction (findAction s r) s

findAction :: String -> [Rule] -> Action
findAction s l = maybe doNothing action $ find (\x -> pattern x `isPrefixOf` s) l
                 where doNothing _ = return ""

--
-- | Send a privmsg to the current chan + server
--
privmsg :: String -> Irc ()
privmsg s = do
  conf <- asks config
  write "PRIVMSG" (chan conf ++ " :" ++ s)

--
-- | Send a message out to the server we're currently connected to
--
write :: String -> String -> Irc ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- | Convenience.
--
io :: IO a -> Irc a
io = liftIO

type Pattern = String

type Action = String -> IO String

data Rule = Rule {
      pattern :: Pattern
    , action :: Action
}

instance Default Rule where
  def = Rule
          { pattern = ""
          , action  = def
          }

instance IsString Rule where
  fromString x = def { pattern = x}

liftAction :: Action -> String -> Irc ()
liftAction a s = do
    h <- asks socket
    conf <- asks config

    r <- io (a s)
    p r h (chan conf)
        where
          p [] _ _ = return ()
          p r h c = io $ hPrintf h "PRIVMSG %s\r\n" (c ++ " :" ++ r)


newtype BehaviorM a = BehaviorM {unBehaviorM :: State [Rule] a}
    deriving (Functor
             , Applicative
             , Monad)

type Behavior = BehaviorM ()

instance IsString Behavior where
  fromString = addRule . fromString

instance Show Behavior where
    show = show . map pattern . runBevhavior

runBevhavior :: Behavior -> [Rule]
runBevhavior bev = execState (unBehaviorM bev) []

addRule :: Rule -> Behavior
addRule r = BehaviorM $ modify (r :)
-- | modHeadRule modifies the first rule of a behavior
--
-- >>> "pattern" `modHeadRule` (\x -> x {pattern = (reverse.pattern) x})
-- ["nrettap"]
--
modHeadRule :: Behavior -> (Rule -> Rule) -> Behavior
modHeadRule bev f = do
  let rs = runBevhavior bev
  BehaviorM $ case rs of
                x:_ -> modify (reverse.(f x:).reverse)
                []  -> modify id

ruleAddAction :: Action -> Rule -> Rule
ruleAddAction f r = r {action = f}

infixl 8 |!

-- | (|!) is a infix API to add a rule to a Behavior monad
--
-- >>> "pattern1" |! return >> "pattern2" |! return
-- ["pattern1","pattern2"]
--
-- >>> "pattern1" |! return >> "pattern2" |! return >> "pattern3" |! return
-- ["pattern1","pattern2","pattern3"]
(|!) :: Behavior -> (String -> IO String) -> Behavior
bev |! f = modHeadRule bev $ ruleAddAction f
