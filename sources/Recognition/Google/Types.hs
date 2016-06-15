{-# LANGUAGE RecordWildCards #-}
module Recognition.Google.Types where
-- import Recognition.Extra
-- import Recognition.Types

--------------------------------------------------------------------------------

{-|

"simultaneously and asynchronously"

-}
type DuplexSpeech = (DownloadSpeech,UploadSpeech)

{-|

either:

* use 'defaultAPIKey' (unlimited (?), but unofficial)
* get an API Key (limited to 50 transcriptions per day, but officially supported):

      * join the Chromium Development Group: visit <https://groups.google.com/a/chromium.org/forum/#!forum/chromium-dev>, click @Join Group@.
      * enable : visit <https://console.developers.google.com/apis/api/speech/overview>, click @Enable@, copy the key.
      * (it only takes a minute)

see <http://blog.travispayton.com/wp-content/uploads/2014/03/Google-Speech-API.pdf>

-}
type APIKey = String

{-|

key: API Key
pair: A random string of Letters and Numbers used to join to the Upload stream
lang: What language i.e. “en-US”
continuous: keep the connections open
interim: send back information as it become available (before final:true)
pFilter*: Profanity filter (0: none, 1:some, 2:strict)

-}
data UploadSpeech = UploadSpeech
 { _UploadSpeech_key        :: APIKey
 , _UploadSpeech_pair       :: DuplexPair
 , _UploadSpeech_lang       :: Language
 , _UploadSpeech_continuous :: IsContinuous
 , _UploadSpeech_interim    :: IsInterim
 , _UploadSpeech_pFilter    :: ProfanityFilter
 , _UploadSpeech_client     :: String
 } deriving (Show)

{-|

-}
data DownloadSpeech = DownloadSpeech
 { _DownloadSpeech_pair :: DuplexPair
 } deriving (Show)

{-|

-}
type DuplexPair = String

{-|

-}
type IsContinuous = Bool

{-|

-}
type IsInterim = Bool

{-|

-}
type Language = String

{-|

-}
type ProfanityFilter = Integer

{-|

-}
type SampleRate = Integer

--------------------------------------------------------------------------------

{-|

'_UploadSpeech_pair' and '_DownloadSpeech_pair' must coincide.

-}
defaultDuplexSpeech ::DuplexPair -> DuplexSpeech
defaultDuplexSpeech = (,) <$> DownloadSpeech <*> defaultUploadSpeech
  -- DownloadSpeech _pair, defaultUploadSpeech _pair)

{-|

-}
defaultUploadSpeech :: DuplexPair -> UploadSpeech
defaultUploadSpeech _UploadSpeech_pair = UploadSpeech{..}
 where
 _UploadSpeech_key        = defaultAPIKey
-- _UploadSpeech_pair      =
 _UploadSpeech_lang       = "en-US"
 _UploadSpeech_continuous = True
 _UploadSpeech_interim    = True
 _UploadSpeech_pFilter    = 0
 _UploadSpeech_client     = "chrome"

{-| The API Key hardcoded into Chrome to implementation the Web Speech API.

-}
defaultAPIKey :: APIKey
defaultAPIKey = "AIzaSyBOti4mM-6x9WDnZIjIeyEU21OpBXqWBgw"

--------------------------------------------------------------------------------

{-

see <http://codeabitwiser.com/2014/09/python-google-speech-api/>
<http://blog.travispayton.com/wp-content/uploads/2014/03/Google-Speech-API.pdf>

POST https://www.google.com/speech-api/fullduplex/v1/up

key = API KEY
pair = a random string used to connect the down stream to this string.
output = the type of response you want, if none is specified, JSON is used.

GET https://www.google.com/speech-api/fullduplex/v1/down
key
pair

This stream must be connected first.

A GET
request to “down” without a matching POST to
“up” will fail.

Also, if the upstream terminates
before the full transcription is finished
downloading, then the down stream will be
terminated prematurely.

?
‘Transfer-Encoding: chunked’,
‘Content-Type: audio/l16; rate=16000’


-}

--------------------------------------------------------------------------------
