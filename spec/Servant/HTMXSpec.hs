module Servant.HTMXSpec where

import Servant.HTMX

import Servant.Server
import Data.Text (Text)
import Servant.API
import Test.Hspec
import Test.Hspec.Wai
import Data.Proxy
import GHC.Exts
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = with testApplication do

  describe "IsHXRequest" do

    it "returns 200 when a HX-Request header is provided" do
      request "GET" "/" [("HX-Request", "true")] mempty
        `shouldRespondWith` 200 { matchBody = "IsHXRequest-gated example" }

    it "skips the IsHXRequest route when no HX-Request header is provided" do
      get "/" `shouldRespondWith` 200 { matchBody = "Example test" }

  describe "HXHeaders" do

    it "parses the headers correctly" do
      request "GET" "/headers" [("HX-Request", "true")] mempty
        `shouldRespondWith` 200
          { matchBody = fromString (show emptyHXHeaders { hxRequest = True }) }

      request "GET" "/headers" [("HX-Boosted", "true")] mempty
        `shouldRespondWith` 200 { matchBody = fromString (show emptyHXHeaders { hxBoosted = True }) }

type TestAPI =
  IsHXRequest :> Get '[PlainText] Text :<|>
  Get '[PlainText] Text :<|>
  "headers" :> HXHeaders :> Get '[PlainText] Text

testServer :: Server TestAPI
testServer = hxRequestRootHandler :<|> rootHandler :<|> headersHandler
  where
    hxRequestRootHandler = pure "IsHXRequest-gated example"
    rootHandler = pure "Example test"
    headersHandler hx =
      pure $ Text.pack $ show hx

testApplication :: IO Application
testApplication = pure $ serve (Proxy :: Proxy TestAPI) testServer
