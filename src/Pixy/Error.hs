module Pixy.Error
    ( ErrorMessage(..)
    , Error(..)
    , renderError
    ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

import Data.Text (Text)

data ErrorMessage = ErrorMessage
    { errorDescription :: Doc AnsiStyle
    }

class Error e where
    toError :: e -> ErrorMessage

instance Error String where
    toError s = ErrorMessage { errorDescription = pretty s }


renderError :: ErrorMessage -> Text
renderError err = 
    let errorStyle = color Red <> bold
        errDoc = (annotate errorStyle $ pretty "Error:") <> nest 4 line <>  (errorDescription err)
    in renderStrict $ layoutPretty defaultLayoutOptions errDoc
