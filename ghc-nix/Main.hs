{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Main ( main ) where

import Digraph
import Data.Foldable
import Control.Monad.IO.Class ( liftIO )
import GHC.Paths ( libdir )
import Finder
import GHC
import DynFlags
import System.Environment ( getArgs )

main :: IO ()
main = runGhc ( Just libdir ) do
  commandLineArguments <-
    liftIO getArgs

  ( dynFlags, files ) <- do
    initialDynFlags <-
      getSessionDynFlags

    ( newDynFlags, leftOver, _ ) <-
      parseDynamicFlagsCmdLine initialDynFlags ( map noLoc commandLineArguments )

    return ( newDynFlags, leftOver )

  setSessionDynFlags dynFlags

  setTargets do
    L _ filePath <-
      files

    return
      Target
        { targetId = TargetFile filePath Nothing
        , targetAllowObjCode = True
        , targetContents = Nothing
        }

  moduleGraph <-
    depanal [] True

  hsc_env <-
    getSession

  targets <-
    getTargets

  liftIO ( putStrLn ( "length targets = " ++ show ( length targets ) ) )

  let
    stronglyConnectedComponents =
      topSortModuleGraph False moduleGraph Nothing

  liftIO ( putStrLn ( "length sccs = " ++ show ( length stronglyConnectedComponents ) ) )

  for_ stronglyConnectedComponents \case
    AcyclicSCC ms@ModSummary{ ms_location = ModLocation{ ml_hs_file }, ms_srcimps } -> do
      liftIO ( print ml_hs_file )
