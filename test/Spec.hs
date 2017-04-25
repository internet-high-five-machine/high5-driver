import           ClassyPrelude
import           Test.Hspec
import           Control.High5.App             hiding (go)
import           Control.High5.Core
import           Pipes
import qualified Pipes.Prelude                 as P


-- Command to run tests with ghcid:
-- ghcid --command="stack ghci :test" --test=":main" --restart=package.yaml
main :: IO ()
main = hspec protocol


protocol :: Spec
protocol = do
    describe "High 5 Protocol" $ do
        it "responds to 'ping'" $ do
            -- TODO: Implement
            True `shouldBe` True

