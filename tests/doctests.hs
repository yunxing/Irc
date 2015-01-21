import Test.DocTest
main :: IO()
main = doctest ["-isrc", "src/Irc/Internal.hs"]
