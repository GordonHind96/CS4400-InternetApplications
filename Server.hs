{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-}

module Server where
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Hashable
import           System.IO
import           Text.Printf              (hPrintf, printf)
import           Network


import Channel
import Client
import Types

type Server = TVar (Map Int Room)

