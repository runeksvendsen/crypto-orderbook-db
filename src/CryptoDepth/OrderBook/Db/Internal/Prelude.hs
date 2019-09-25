module CryptoDepth.OrderBook.Db.Internal.Prelude
( T.Text
, Word32
, Generic
, logErrorS
, module ProtoString
, module TypeLits
, module Proxy
, module Maybe
, module Safe
, module IO
, module Monad
)
where

import Data.Word (Word32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Protolude.Conv as ProtoString (toS, StringConv)
import GHC.TypeLits as TypeLits
import Data.Proxy as Proxy
import Data.Maybe as Maybe
import Protolude.Safe as Safe
import Protolude as IO (MonadIO, liftIO)
import Control.Monad as Monad
import qualified Control.Logging                    as Log


logErrorS :: T.Text -> T.Text -> IO ()
logErrorS = Log.loggingLogger Log.LevelError
