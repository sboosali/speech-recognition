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


import Formatting
import System.Random
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy.Char8 as BL8
-- import qualified Data.ByteString.Char8 as BS8

import Data.Int (Int64)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

{-|

Generates a random number, 64-bit, in hexadecimal.

-}
getDefaultDuplexSpeech :: IO DuplexSpeech
getDefaultDuplexSpeech = do
  i :: Int64 <- randomIO -- hex(random.getrandbits(64))[2:-1]
  let s = formatToString hex i
  return $ defaultDuplexSpeech s

{-|

@
{'content-type': 'audio/x-flac; rate=' + str(self.flac.sampleRate)}
@

-}
uploadHeaders :: SampleRate -> [(String,String)]
uploadHeaders rate =
  [ "content-type"-: (formatToString ("audio/x-flac; rate="%int) rate)
  ]

{-| A few zero-bytes.

To keep-alive the upstream.

-}
nullAudio :: BL.ByteString
nullAudio = BL.pack [0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0]

--------------------------------------------------------------------------------
