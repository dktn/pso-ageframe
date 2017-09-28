module Pretty where

import           Text.Show.Pretty (ppDoc)
import           Text.PrettyPrint (renderStyle, Style(..), Mode(..))

ppShow :: Show a => a -> String
ppShow = renderStyle wideStyle . ppDoc
  where
    wideStyle = Style PageMode 238 1.2
