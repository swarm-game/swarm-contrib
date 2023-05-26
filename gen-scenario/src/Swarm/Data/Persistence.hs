-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Data.Persistence where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (eitherDecodeFileStrict)
import Data.SortedList (SortedList, toSortedList)
import Data.Yaml (encodeFile)
import Swarm.Data.GenEntities

loadSpecs :: FilePath -> ExceptT String IO (SortedList ColoredEnt)
loadSpecs = fmap toSortedList . ExceptT . eitherDecodeFileStrict

writeScenario :: FilePath -> SortedList ColoredEnt -> ExceptT String IO ()
writeScenario outPath specs = do
  let s = mkSkeletonScenario specs
  liftIO $ encodeFile outPath s
