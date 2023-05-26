{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Data.GenCode where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.SortedList (SortedList, fromSortedList)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Data.GenEntities qualified as GE
import Swarm.Util (quote)
import Text.Ginger

mkContext :: Text -> Map Text Text
mkContext indexContent =
  M.fromList
    [ ("indices", indexContent)
    ]

basicResolver :: IncludeResolver IO
basicResolver sourceName = do
  content <- readFile sourceName
  return $ Just content

mkTemplate :: FilePath -> IO (Either ParserError (Template SourcePos))
mkTemplate codePath = do
  content <- readFile codePath
  parseGinger basicResolver (Just codePath) content

renderIfElseCases :: Int -> [(Int, Text)] -> [Text]
renderIfElseCases _ [] = []
renderIfElseCases firstIndex stuff =
  reverse $ lastLine (snd x) : tailLines
 where
  lastLine b = T.unwords ["$", "else", cbraces $ quote b]
  tailLines = map (renderLine (\x -> if x == firstIndex then "if" else "$ elif")) xs
  (x : xs) = reverse stuff
  renderLine prefixFunc (a, b) =
    T.unwords
      [ prefixFunc a
      , parens $
          T.unwords
            [ "i"
            , "=="
            , T.pack $ show a
            ]
      , cbraces $ quote b
      ]

  parens x = "(" <> x <> ")"
  cbraces x = "{" <> x <> "}"

renderIndexCode :: SortedList GE.ColoredEnt -> [Text]
renderIndexCode =
  renderIfElseCases firstIndex . zipWith (curry $ fmap GE.name) [firstIndex ..] . fromSortedList
 where
  firstIndex = 1

writeCode :: FilePath -> SortedList GE.ColoredEnt -> FilePath -> ExceptT String IO ()
writeCode templatePath xs outPath = do
  template <- withExceptT show $ ExceptT $ mkTemplate templatePath
  let indexCode = T.unlines $ renderIndexCode xs
  let outContent = easyRender (mkContext indexCode) template
  liftIO $ writeFile outPath $ T.unpack outContent
