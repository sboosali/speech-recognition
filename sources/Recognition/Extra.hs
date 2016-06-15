module Recognition.Extra
 ( module Prelude.Spiros    -- My custom Prélude
 , module Recognition.Extra -- Extra definitions
-- , module X                 -- Extra modules
 ) where

--------------------------------------------------------------------------------
-- re-exported
import Prelude.Spiros

--------------------------------------------------------------------------------
-- non-re-exported
import Language.Haskell.TH.Quote (QuasiQuoter)
import Data.Aeson.QQ

--------------------------------------------------------------------------------

{-| JSON literals.

e.g.

@
>>> :set -XQuasiQuotes
>>> let example key value = [json| {$key: #{value}, "falsy": [0, False, null, {}]} |]
@

@
= 'aesonQQ'
@

-}
json :: QuasiQuoter
json = aesonQQ

--------------------------------------------------------------------------------
