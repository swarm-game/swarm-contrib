{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Data.GenEntities where

import Control.Lens (view, (&), (.~))
import Control.Monad.Random.Lazy
import Data.Aeson (FromJSON, defaultOptions, genericParseJSON, omitNothingFields, parseJSON)
import Data.Colour.Palette.RandomColor
import Data.Colour.Palette.Types
import Data.Colour.SRGB (sRGB24show)
import Data.Function (on)
import Data.SortedList (SortedList, fromSortedList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Data.Scenario.Mergeable
import Swarm.Game.Display
import Swarm.Game.Entity
import Swarm.Game.Recipe
import Swarm.Game.Scenario.Style (CustomAttr (CustomAttr), HexColor (HexColor))

data ColoredEnt = ColoredEnt
  { name :: Text
  , description :: Maybe Text
  , color :: Maybe HexColor
  }
  deriving (Show, Generic)

instance FromJSON ColoredEnt where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

instance Eq ColoredEnt where
  (==) = (==) `on` name

instance Ord ColoredEnt where
  compare = compare `on` name

mkSkeletonScenario :: SortedList ColoredEnt -> MergeableScenario
mkSkeletonScenario xs =
  MergeableScenario
    atts
    ents
    rcps
 where
  flowerEntities = mkFlowerEntities xs
  ents = pouchEntity : map snd flowerEntities
  atts = mkColorAttrs xs
  rcps = createRecipes flowerEntities

toAttrName :: ColoredEnt -> Text
toAttrName (ColoredEnt entName _ _) = entName <> "ColorAttr"

mkColorAttrs :: SortedList ColoredEnt -> [CustomAttr]
mkColorAttrs = genAll . fromSortedList
 where
  genAll xs = evalRand (mapM f xs) (mkStdGen 0)

  f e@(ColoredEnt _ _ maybecolor) = do
    clr <- case maybecolor of
      Nothing -> do
        c <- randomColor HueRandom LumLight
        return $ HexColor $ T.pack $ sRGB24show c
      Just x -> pure x

    return $
      CustomAttr
        (T.unpack $ toAttrName e)
        (Just clr)
        (Just $ HexColor "#000000")
        Nothing

mkFlowerEntities :: SortedList ColoredEnt -> [(ColoredEnt, Entity)]
mkFlowerEntities = map f . fromSortedList
 where
  a = AWorld . toAttrName
  f e@(ColoredEnt entName maybeDesc _) =
    (e, ent)
   where
    ent =
      mkEntity
        (defaultEntityDisplay '*' & displayAttr .~ a e)
        entName
        (maybe [] pure maybeDesc)
        [Known, Portable, Growable]
        []

pouchEntity :: Entity
pouchEntity =
  mkEntity
    (defaultEntityDisplay 'p')
    "flower pouch"
    ["A pouch that holds every type of flower. Allows converting one planted flower to the next flower in order."]
    [Portable]
    []

oneItem :: e -> (Count, e)
oneItem x = (1, x)

createPouchRecipe :: [(ColoredEnt, Entity)] -> Recipe Text
createPouchRecipe ents =
  mkRecipe
    (map (oneItem . view entityName . snd) ents) -- inputs
    [oneItem . view entityName $ pouchEntity] -- outputs
    mempty

createSuccessorRecipes :: [Entity] -> [Recipe Text]
createSuccessorRecipes ents = zipWith f ents $ tail $ cycle ents
 where
  f e1 e2 =
    mkRecipe
      [oneItem $ view entityName e1] -- inputs
      [oneItem $ view entityName e2] -- outputs
      (map oneItem [view entityName pouchEntity, "drill"])

mkRecipe ::
  IngredientList a ->
  IngredientList a ->
  IngredientList a ->
  Recipe a
mkRecipe inputs outputs requirements = Recipe inputs outputs requirements 1 1

createRecipes :: [(ColoredEnt, Entity)] -> [Recipe Text]
createRecipes ents = createPouchRecipe ents : createSuccessorRecipes (map snd ents)
