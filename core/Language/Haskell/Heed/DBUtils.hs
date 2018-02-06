module Language.Haskell.Heed.DBUtils where

import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.List.Split
import Control.Monad.Trans
import Control.Monad
import Control.Exception (throwIO)
import Control.Monad.Catch
import Data.Text (pack)

import GHC (gcatch)
import GhcMonad

import Database.Selda
import qualified Database.Selda.Backend as Backend -- just for disabling foreign keys to drop tables

instance MonadThrow Ghc where
    throwM  = liftIO . throwIO

instance MonadCatch Ghc where
    catch   = GHC.gcatch

instance MonadMask Ghc where
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
     where
        wrap   = GhcMonad.Ghc
        unwrap = GhcMonad.unGhc
    uninterruptibleMask = mask

liftGhc :: Ghc a -> SeldaT Ghc a
liftGhc = lift

withForeignCheckTurnedOff :: (MonadMask m, MonadIO m, MonadThrow m) => SeldaT m a -> SeldaT m a
withForeignCheckTurnedOff act = do
   backend <- Backend.seldaBackend
   (resCode, _) <- liftIO $ Backend.runStmt backend (pack "PRAGMA foreign_keys = OFF") [] -- TODO: something different for postgre
   when (resCode /= 0) (throwM $ Backend.SqlError "withForeignCheckTurnedOff: Switching foreign keys off was not successful.")
   res <- act
   (resCode, _) <- liftIO $ Backend.runStmt backend (pack "PRAGMA foreign_keys = ON") []
   when (resCode /= 0) (throwM $ Backend.SqlError "withForeignCheckTurnedOff: Switching foreign keys back on was not successful.")
   return res

tabulate :: Monad m => Int -> ([a] -> [a] -> [a]) -> [b] -> ([b] -> m [a]) -> m [a]
tabulate n comb inp op = foldl1 comb <$> mapM op chunks
  where chunks = chunksOf n inp

bsToText :: BS.ByteString -> Text
bsToText = T.decodeUtf8 -- X.toText . X.fromBinary

textToBS :: Text -> BS.ByteString
textToBS = T.encodeUtf8 -- X.toBinary . hexString . T.encodeUtf8

