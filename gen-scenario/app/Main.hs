-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Monad.Trans.Except
import Options.Applicative
import Swarm.Data.GenCode
import Swarm.Data.Persistence

data Options = Options
  { dataPath :: FilePath
  , outputPath :: FilePath
  , templatePath :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "data"
          <> metavar "DATA_FILEPATH"
          <> help "data input filepath"
      )
    <*> strOption
      ( long "out"
          <> metavar "OUTPUT"
          <> help "output filepath"
      )
    <*> strOption
      ( long "codegen-template"
          <> metavar "TEMPLATE_DIR"
          <> value ""
          <> help "Generate code from a template"
      )

main :: IO ()
main = run =<< execParser opts
 where
  opts =
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Generate greenhouse scenario"
          <> header "Greenhouse generator"
      )

run :: Options -> IO ()
run opts = do
  out <- runExceptT $ do
    specs <- loadSpecs $ dataPath opts
    if not $ null templPath
      then writeCode templPath specs (outputPath opts)
      else writeScenario (outputPath opts) specs
  print out
 where
  templPath = templatePath opts
