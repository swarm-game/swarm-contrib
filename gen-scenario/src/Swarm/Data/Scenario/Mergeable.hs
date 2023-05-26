{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Data.Scenario.Mergeable where

import Data.Text (Text)
import Data.Yaml (ToJSON)
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Scenario.Style

data MergeableScenario = MergeableScenario
  { attrs :: [CustomAttr]
  , entities :: [Entity]
  , recipes :: [Recipe Text]
  }
  deriving (Show, Generic, ToJSON)
