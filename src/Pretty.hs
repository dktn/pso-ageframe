module Pretty where

import           Protolude
import           Text.Show.Pretty (ppDoc)
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))

ppShow :: Show a => a -> Text
ppShow = toS . renderStyle wideStyle . ppDoc
  where
    wideStyle = Style PageMode 238 1.2
