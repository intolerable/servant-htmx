module Servant.HTMX where

import Data.Maybe
import Data.Proxy
import Servant.API
import Servant.Server
import Servant.Server.Internal
import Data.Text (Text)
import Network.Wai

data IsHXRequest

instance
  ( HasContextEntry (MkContextWithErrorFormatter ctx) ErrorFormatters
  , HasServer api ctx
  ) => HasServer (IsHXRequest :> api) ctx where
  type ServerT (IsHXRequest :> api) m = ServerT api m

  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx $
      subserver `addHeaderCheck_` withRequest \req -> do
        case lookup "HX-Request" (requestHeaders req) of
          Nothing -> delayedFail err400
          Just _ -> pure ()

  hoistServerWithContext Proxy ctx nt server =
    hoistServerWithContext (Proxy :: Proxy api) ctx nt server

addHeaderCheck_ :: Delayed env a -> DelayedIO () -> Delayed env a
addHeaderCheck_ Delayed{..} check =
  Delayed
    { headersD = check *> headersD
    , ..
    }

instance HasLink api => HasLink (IsHXRequest :> api) where
  type MkLink (IsHXRequest :> api) r = MkLink api r

  toLink toA Proxy = toLink toA (Proxy :: Proxy api)

data HXHeaders =
  HXHeaders { hxRequest :: Bool
            , hxBoosted :: Bool
            , hxTrigger :: Maybe Text
            , hxTriggerName :: Maybe Text
            , hxTarget :: Maybe Text
            , hxPrompt :: Maybe Text
            }
  deriving (Show, Eq, Ord)

emptyHXHeaders :: HXHeaders
emptyHXHeaders = HXHeaders False False Nothing Nothing Nothing Nothing

type HXRequestHeader = Header' '[Optional, Strict] "HX-Request" Bool
type HXBoostedHeader = Header' '[Optional, Strict] "HX-Boosted" Bool
type HXTriggerHeader = Header' '[Optional, Strict] "HX-Trigger" Text
type HXTriggerNameHeader = Header' '[Optional, Strict] "HX-Trigger-Name" Text
type HXTargetHeader = Header' '[Optional, Strict] "HX-Target" Text
type HXPromptHeader = Header' '[Optional, Strict] "HX-Prompt" Text

type HXHeaders' api =
  HXRequestHeader :>
  HXBoostedHeader :>
  HXTriggerHeader :>
  HXTriggerNameHeader :>
  HXTargetHeader :>
  HXPromptHeader :>
  api

instance
  ( HasContextEntry (MkContextWithErrorFormatter ctx) ErrorFormatters
  , HasServer api ctx )
  => HasServer (HXHeaders :> api) ctx where
  type ServerT (HXHeaders :> api) m = HXHeaders -> ServerT api m

  route (Proxy :: Proxy (HXHeaders :> api)) ctx subserver =
    route (Proxy :: Proxy (HXHeaders' api)) ctx (fmap convert subserver)
    where convert f maybeRequest maybeBoosted trigger triggerName target prompt =
            f $
              HXHeaders (fromMaybe False maybeRequest)
                        (fromMaybe False maybeBoosted)
                        trigger
                        triggerName
                        target
                        prompt

  hoistServerWithContext (Proxy :: Proxy (HXHeaders :> api)) ctx nt server =
    hoistServerWithContext (Proxy :: Proxy api) ctx nt . server

instance HasLink api => HasLink (HXHeaders :> api) where
  type MkLink (HXHeaders :> api) r = MkLink api r

  toLink toA Proxy = toLink toA (Proxy :: Proxy api)
