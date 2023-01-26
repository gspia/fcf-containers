import Test.DocTest

exts :: [String]
exts =
  [ "-XDataKinds"
  , "-XKindSignatures"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  ]

main :: IO ()
main = doctest $ exts ++ ["src"]
