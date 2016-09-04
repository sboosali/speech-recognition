{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Google Speech.

WARNING: rate-limited. public usage is unofficial.


see:

* <http://codeabitwiser.com/2014/09/python-google-speech-api/>
* <http://blog.travispayton.com/wp-content/uploads/2014/03/Google-Speech-API.pdf>

what it does:

* Connect the Download stream
* Connect the Upload stream
* Upon successful connection, Google will send back an empty result: {“result”:[]}
* Keep the Download & Upload stream open until Google Finishes responding
* Google will signal the transcription is finished with a final:true tag in the JSON object
* Files with long silences in them will have mutliple final:true sections returned!
* Google will also send a response to the Upload stream connection to signal there is nothing else to process
* Close the Upload Stream
* Close the Download Stream

-}
module Recognition.Google where
import Recognition.Extra
import Recognition.Google.Types


import Network.Wreq
import Control.Lens
-- import Data.Aeson
-- import Data.Aeson.Lens
import Formatting
import System.Random
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy.Char8 as BL8
-- import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

import Data.Word
--import Control.Concurrent
import Control.Concurrent.Async

--------------------------------------------------------------------------------

{-

https://hackage.haskell.org/package/wreq-0.4.1.0/docs/Network-Wreq.html


("X-Request-URL","POST https://www.google.com:443/speech-api/fullduplex/v1/up?client=chrome&pFilter=0&interim=True&continuous=True&lang=en-US&pair=703e4035de67d5a7&key=AIzaSyBOti4mM-6x9WDnZIjIeyEU21OpBXqWBgw")


get "https://www.google.com/speech-api/full-duplex/v1/down?pair=25904992c8187b9"


http://docs.python-requests.org/en/master/user/advanced/

requests.Session().get("https://www.google.com/speech-api/full-duplex/v1/down?pair=25904992c8187b9",stream=True)

What does this do? requests.Session().get(_,stream=True)

requests stream=True


-}
example_Recognition_Google :: IO()
example_Recognition_Google = do
 (downstream, upstream) <- getDefaultDuplexSpeech

 d'thread <- async $ do
     let d'request = downstreamRequest downstream
     d'response <- getWith d'request downstreamUrl
     print d'response

 -- wait d'thread

 u'thread <- async $ do
     let flacFile = "python/test.flac"
     let u'request = upstreamRequest 16000 upstream
     u'response <- postWith u'request upstreamUrl (partFileSource "" flacFile) -- nullAudio --(""::BL.ByteString)
     print u'response

 traverse_ wait [d'thread, u'thread]

-- requestUploadSpeech :: SampleRate -> UploadSpeech -> (String,Options)
-- requestUploadSpeech sampleRate UploadSpeech{..} = (url,options)
--   where
--   url = upstreamUrl
--   options = headers <> parameters
--   headers = upstreamOptions sampleRate
--   parameters = defaultAPIKey
--  _UploadSpeech_key        = defaultAPIKey
--  _UploadSpeech_pair      =
--  _UploadSpeech_lang       = "en-US"
--  _UploadSpeech_continuous = True
--  _UploadSpeech_interim    = True
--  _UploadSpeech_pFilter    = 0
--  _UploadSpeech_client     = "chrome"

{-|

-}
downstreamRequest :: DownloadSpeech -> Options
downstreamRequest DownloadSpeech{..} = defaults
  & header "Transfer-Encoding" .~ ["chunked"]
  & param "pair"               .~ [T.pack _DownloadSpeech_pair]

{-|

'SampleRate' depends on the audio, the 'UploadSpeech' is independent of the audio.

https://www.google.com/speech-api/full-duplex/v1/up?key=%(key)s&pair=%(pair)s&lang=en-US&client=chromium&continuous&interim&pFilter=0

-}
upstreamRequest :: SampleRate -> UploadSpeech -> Options
upstreamRequest sampleRate UploadSpeech{..} = defaults
--   & header "Content-Type"      .~ [formatToString ("audio/x-flac; rate="%int) sampleRate]
   & header "Content-Type"      .~ ["audio/x-flac; rate=1600"] --TODO
   & header "Transfer-Encoding" .~ ["chunked"]

   & param "key"                .~ [T.pack _UploadSpeech_key]
   & param "pair"               .~ [T.pack _UploadSpeech_pair]
   & param "lang"               .~ [T.pack _UploadSpeech_lang]
   & param "continuous"         .~ [tshow _UploadSpeech_continuous]
   & param "interim"            .~ [tshow _UploadSpeech_interim]
   & param "pFilter"            .~ [tshow _UploadSpeech_pFilter]
   & param "client"             .~ [T.pack _UploadSpeech_client]
-- & param ""                   .~ [_UploadSpeech_]

-- paramBool :: String -> Lens' Options Bool
-- paramBool k v = 

--------------------------------------------------------------------------------

{-|

Generates a random number, 64-bit, in hexadecimal.

-}
getDefaultDuplexSpeech :: IO DuplexSpeech
getDefaultDuplexSpeech = do
  i :: Word64 <- randomIO -- non-negative
  let s = formatToString hex i
  return $ defaultDuplexSpeech s

-- {-|

-- @
-- {'content-type': 'audio/x-flac; rate=' + str(self.flac.sampleRate)}
-- @

-- -}
-- uploadHeaders :: SampleRate -> [(String,String)]
-- uploadHeaders rate =
--   [ "content-type"-: (formatToString ("audio/x-flac; rate="%int) rate)
--   ]

{-| A few zero-bytes.

To keep-alive the upstream.

-}
nullAudio :: BL.ByteString
nullAudio = BL.pack [0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0]

--------------------------------------------------------------------------------

upstreamUrl :: String
upstreamUrl = "https://www.google.com/speech-api/full-duplex/v1/up"

-- upstreamHeaders :: SampleRate -> Headers
-- upstreamHeaders sampleRate = defaults
--   & header "Content-Type"      .~ [formatToString ("audio/x-flac; rate="%int) sampleRate]
--   & header "Transfer-Encoding" .~ ["chunked"]

downstreamUrl :: String
downstreamUrl = "https://www.google.com/speech-api/full-duplex/v1/down"

-- downstreamHeaders :: Headers
-- downstreamHeaders = defaults

-- {-|

-- e.g.

-- @
-- {
--   "initialRequest": {
--     "encoding":"FLAC",
--     "sampleRate":16000
--   },
--   "audioRequest": {
--     "content": "$(cat audio.base64)"
--   }
-- }
-- @

-- -}
-- gJSON :: GoogleSpeechRequest -> Value
-- gJSON GoogleSpeechRequest{..} = [dict|
-- {
--   "initialRequest": {
--     "encoding":"LINEAR16",
--     "sampleRate":16000
--   },
--   "audioRequest": {
--     "content": #{gAudio}
--   }
-- }
-- |]

--------------------------------------------------------------------------------
