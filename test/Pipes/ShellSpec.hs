module Pipes.ShellSpec where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char
import           Pipes
import qualified Pipes.Prelude         as P
import qualified Pipes.ByteString      as PBS
import           Pipes.Safe
import           Pipes.Shell
import           Test.Hspec
import           System.IO


import qualified Control.Concurrent as Conc
import System.Directory

spec :: Spec
spec = parallel $ do
    describe "meta tests" $ do
        it "collectOutput works as expected" $ do
            (err, out) <- collectOutput boring
            out `shouldBe` "1234"
            err `shouldBe` "abcd"

    describe "features" $ do
        it "works with tr" $ do
            (err, out) <- collectOutput trTest
            out `shouldBe` "AAA"
            err `shouldBe` ""

        it "handles stdout *and* stderr" $ do
            (err, out) <- collectOutput catEchoTest
            out `shouldBe` "out\nput\n"
            err `shouldBe` "err\nor\n"

        it "handles env variables" $ do
            (err, out) <- collectOutput envTest
            out `shouldBe` "value\n"
            err `shouldBe` ""

    describe "robustness" $ do
        it "can handle /usr/share/dict/*" $ do
            wrds <- wordsTest
            wrdsRef <- wordsRef
            wrds `shouldBe` wrdsRef

        it "shouldn't hang on gzip" $ do
            archivePath <- fmap (++ "/test/numbers.gz") getCurrentDirectory
            withFile archivePath ReadMode (\h ->
                runShell $ PBS.fromHandle h >?> pipeCmd' "gzip -d -c" >-> slowConsumer 10000)

  where
    slowConsumer waitDuration = do
       _ <- await
       liftIO $ Conc.threadDelay waitDuration
       slowConsumer waitDuration


boring :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
boring = each [Left $ BSC.pack "ab",
               Left $ BSC.pack "cd",
               Right $ BSC.pack "12",
               Right $ BSC.pack "34"]

-- collect the output of the pruducer as a (String,String)
-- these are not [String], because chunking is not really deterministic
collectOutput ::
  Producer (Either BS.ByteString BS.ByteString) (SafeT IO) () ->
  IO (String, String)
collectOutput = runSafeT . P.fold combine ([],[]) fixUp
  where
    combine (err, out) (Left x)  = (x:err, out)
    combine (err, out) (Right x) = (err  , x:out)

    fixUp (err, out) = (concat $ reverse $ map BSC.unpack err,
                        concat $ reverse $ map BSC.unpack out)

trTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
trTest = yield (BSC.pack "aaa") >?> cmd "tr 'a' 'A'"

catEchoTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
catEchoTest = yield (BSC.pack "out\nput\n") >?>
               cmd ("cat; " <>
                    "echo 'err\nor' > /dev/stderr; sync")

envTest :: Producer (Either BS.ByteString BS.ByteString) (SafeT IO) ()
envTest = cmdEnv env "echo $VARIABLE"
  where
    env = Just [("VARIABLE","value")]

wordsRef :: IO Int
wordsRef = do
  (lines':_) <- runSafeT $ P.toListM $ cmd' "cat /usr/share/dict/* | wc -l"
  return $ read $ BSC.unpack lines'

wordsTest :: IO Int
wordsTest = runShell $ countNewlines $ cmd' "cat /usr/share/dict/*"
  where
    -- yay, double fold
    countNewlines = P.fold countInChunk 0 id
    countInChunk soFar chunk = soFar + BS.foldl' countInBS 0 chunk
    countInBS soFar wrd
      | wrd == fromIntegral (ord '\n') = soFar + 1
      | otherwise                      = soFar
