{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

module Recognition.Example where
import Recognition()

import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB

{-|
@
stack build && stack exec -- example-speech-recognition
@

https://hackage.haskell.org/package/pipes-http-1.0.2/docs/Pipes-HTTP.html

https://hackage.haskell.org/package/http-client-0.4.28/docs/Network-HTTP-Client.html


-}
main = do
    putStrLn "(Recognition.Example...)"

    _req <- parseUrl "https://www.example.com"
    let req = post _req

    m <- newManager tlsManagerSettings
    withHTTP req m $ \resp ->
            runEffect $ responseBody resp >-> PB.stdout

post r = r
 { method = "POST"
 , requestBody = stream PB.stdin
 }
