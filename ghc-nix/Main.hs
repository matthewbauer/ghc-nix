{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Main ( main ) where

import Control.Monad.IO.Class ( liftIO )
import Data.Foldable
import Digraph
import DynFlags
import Finder
import GHC
import GHC.Paths ( libdir )
import HscTypes
import System.Environment ( getArgs )

main :: IO ()
main = print libdir >>
  runGhc ( Just libdir ) do
  commandLineArguments <-
    liftIO getArgs

  ( dynFlags, files ) <- do
    initialDynFlags <-
      getSessionDynFlags

    ( newDynFlags, leftOver, _ ) <-
      parseDynamicFlagsCmdLine initialDynFlags ( map noLoc commandLineArguments )

    return ( newDynFlags, leftOver )

  installed <-
    setSessionDynFlags dynFlags

  liftIO ( print ( length installed ) )

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
      liftIO ( print ( map fst ms_srcimps ) )
 
      for_ ( ms_imps ms ) \( package, L _ moduleName ) -> liftIO do
        findImportedModule hsc_env moduleName package >>= \case
          Found ModLocation{ ml_hs_file = Just hsFile } _ -> do
            putStrLn ( moduleNameString moduleName ++ " was found. Compile " ++ hsFile ++ " first" )

          Found _ _ ->
            putStrLn ( moduleNameString moduleName ++ " was found, but I don't know the .hs file" )

          FoundMultiple options ->
            putStrLn ( moduleNameString moduleName ++ " has " ++ show ( length options ) ++ " options." )

          NoPackage _ ->
            putStrLn "NoPackage"

          NotFound{} ->
            putStrLn ( "I dunno where " ++ moduleNameString moduleName ++ " is" )
