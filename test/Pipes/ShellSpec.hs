module Pipes.ShellSpec where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char
import           Data.Monoid
import           Pipes
import qualified Pipes.Prelude         as P
import           Pipes.Safe
import           Pipes.Shell
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "features" $ do
    it "works with tr" $ do
      (err, out) <- collectOutput trTest
      out `shouldBe` ["AAA"]
      err `shouldBe` []

    it "handles stdout *and* stderr" $ do
      (err, out) <- collectOutput catEchoTest
      out `shouldBe` ["out", "put"]
      err `shouldBe` ["err", "or"]

    it "handles env variables" $ do
      (err, out) <- collectOutput envTest
      out `shouldBe` ["value"]
      err `shouldBe` []

  describe "robustness" $ do
    it "can handle /usr/share/dict/words" $ do
      wrds <- wordsTest
      wrdsRef <- wordsRef
      wrds `shouldBe` wrdsRef

collectOutput ::
  Producer (Either BS.ByteString BS.ByteString) (SafeT IO) () ->
  IO ([String], [String])
collectOutput = runSafeT . P.fold combine ([],[]) fix
  where
  combine (err, out) (Left x)  = (x:err, out)
  combine (err, out) (Right x) = (err  , x:out)

  fix (err, out) = (concatMap (lines . BSC.unpack) err,
                    concatMap (lines . BSC.unpack) out)

trTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
trTest = yield (BSC.pack "aaa") >?> cmd "tr 'a' 'A'"

catEchoTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
catEchoTest = yield (BSC.pack "out\nput") >?>
              cmd ("cat > /dev/stdout;" <>
                   "echo 'err\nor' > /dev/stderr")

envTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
envTest = cmdEnv env "echo $VARIABLE"
  where
    env = Just [("VARIABLE","value")]

wordsRef :: IO Int
wordsRef = do
  (lines':_) <- runSafeT $ P.toListM $ cmd' "cat /usr/share/dict/words | wc -l"
  return $ read $ BSC.unpack lines'

wordsTest :: IO Int
wordsTest = runShell $ countNewlines $ cmd' "cat /usr/share/dict/words"

countNewlines :: Monad m => Producer BS.ByteString m () -> m Int
countNewlines = P.fold countChunk 0 id
  where
  countChunk acc x = acc + BS.foldl' countBS 0 x
  countBS cnt wrd
    | wrd == fromIntegral (ord '\n') = cnt + 1
    | otherwise                      = cnt
