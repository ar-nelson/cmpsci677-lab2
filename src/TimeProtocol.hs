{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module TimeProtocol( Timestamp
                   , Lamport
                   , ClockTime
                   , ClockOffset
                   , clockTime
                   , lamportTime
                   , offsetBy
                   , timeDiff
                   , TimeState
                   , initTimeState
                   , Timed
                   , runTimed
                   , updateClockOffset
                   , newTimestamp
                   , Timestamped
                   , timestampOf
                   , stamp
                   , unstamp
                   , stm
) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Time.Clock
import           Data.Time.Format

--------------------------------------------------------------------------------
-- Time datatypes.

data Timestamp = Timestamp ClockTime Lamport deriving (Show, Read)

clockTime (Timestamp c _) = c
lamportTime (Timestamp _ l) = l

newtype Lamport = Lamport Integer deriving (Eq, Ord, Show, Read, Num)

newtype ClockTime = ClockTime UTCTime deriving (Eq, Ord, FormatTime, ParseTime)

newtype ClockOffset = ClockOffset Double
                    deriving (Eq, Ord, Num, Fractional, Show, Read)

instance Show ClockTime where
  show = formatTime defaultTimeLocale "%s%Q"

instance Read ClockTime where
  readsPrec _ = readSTime True defaultTimeLocale "%s%Q"

offsetBy :: ClockTime -> ClockOffset -> ClockTime
ClockTime utc `offsetBy` ClockOffset ms = ClockTime (conv ms `addUTCTime` utc)
  where conv = fromRational . toRational

timeDiff :: ClockTime -> ClockTime -> ClockOffset
timeDiff (ClockTime utc1) (ClockTime utc2) =
  (ClockOffset . fromRational . toRational) (diffUTCTime utc1 utc2)

--------------------------------------------------------------------------------
-- The Timed monad contains I/O computations with a logical ordering, according
-- to Lamport clocks. It deals with Timestamped data: a Timestamped value can
-- only be read by passing it through `unstamp`, which updates the Timed monad's
-- Lamport clock if necessary to be greater than the Timestamped value's time.

data TimeState = TimeState { clockOffsetV :: TVar ClockOffset
                           , lamportTimeV :: TVar Lamport
                           }

initTimeState :: IO TimeState
initTimeState = atomically $ TimeState <$> newTVar (ClockOffset 0)
                                       <*> newTVar (Lamport 0)

newtype Timed a = Timed { unTimed :: ReaderT TimeState IO a }
                deriving (Functor, Applicative, Monad, MonadReader TimeState,
                          MonadIO, MonadBase IO)

-- http://stackoverflow.com/a/28141148/548027
instance MonadBaseControl IO Timed where
  type StM Timed a = a
  liftBaseWith f = Timed $ liftBaseWith $ \q -> f (q . unTimed)
  restoreM = Timed . restoreM

runTimed :: Timed a -> TimeState -> IO a
runTimed (Timed m) = runReaderT m

updateClockOffset :: (MonadReader TimeState m, MonadIO m) => ClockOffset -> m ()
updateClockOffset o = asks clockOffsetV >>= stm . flip modifyTVar (+ o)

newTimestamp :: (MonadReader TimeState m, MonadIO m) => m Timestamp
newTimestamp = do stLamport  <- asks lamportTimeV
                  stOffset   <- asks clockOffsetV
                  (lmp, off) <- stm $ do off <- readTVar stOffset
                                         lmp <- readTVar stLamport
                                         writeTVar stLamport (lmp + 1)
                                         return (lmp, off)
                  now        <- liftM ClockTime (liftIO getCurrentTime)
                  return $ Timestamp (now `offsetBy` off) lmp

data Timestamped a = Timestamped a Timestamp deriving (Read, Show)

timestampOf :: Timestamped a -> Timestamp
timestampOf (Timestamped _ t) = t

stamp :: (MonadReader TimeState m, MonadIO m) => a -> m (Timestamped a)
stamp x = liftM (Timestamped x) newTimestamp

unstamp :: (MonadReader TimeState m, MonadIO m) => Timestamped a -> m a
unstamp (Timestamped x (Timestamp _ recvLamport)) =
  do stLamport <- asks lamportTimeV
     stm . modifyTVar stLamport $ \oldLamport ->
       if recvLamport >= oldLamport then recvLamport + 1 else oldLamport
     return x

stm :: (MonadIO m) => STM a -> m a
stm = liftIO . atomically

