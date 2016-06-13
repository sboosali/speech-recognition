{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Recognition.Example where
import Recognition()

{-|
@
stack build && stack exec -- example-speech-recognition
@
-}
main :: IO ()
main = do
 putStrLn "(Recognition.Example...)"

