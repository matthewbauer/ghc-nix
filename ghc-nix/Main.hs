{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Paths_ghc_nix ( getDataFileName )

import System.Directory ( canonicalizePath )
import Data.List ( (\\) )
import Control.Applicative ( empty )
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception.Safe ( tryAny, throwIO, bracket_ )
import qualified Control.Foldl
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String ( fromString )
import qualified Data.Text
import Data.Text ( Text )
import qualified Data.Text.Encoding
import Data.Traversable ( for )
import Digraph
import DynFlags
import Finder
import GHC
import GHC.Paths ( libdir )
import HscTypes
import System.Environment ( getArgs )
import System.FilePath ( takeExtension )
import qualified Turtle


main :: IO ()
main = do
  commandLineArguments <-
    getArgs

  case commandLineArguments of
    "--numeric-version" : _ ->
      proxyToGHC

    "--supported-languages" : _ ->
      proxyToGHC

    "--info" : _ ->
      proxyToGHC

    "--print-global-package-db" : _ ->
      proxyToGHC

    "--print-libdir" : _ ->
      proxyToGHC

    _ ->
      main2


main2 :: IO ()
main2 = runGhc ( Just libdir ) do
  files <-
    interpretCommandLine

  if ".c" `elem` map takeExtension files || ".o" `elem` map takeExtension files || ".dyn_o" `elem` map takeExtension files
    then proxyToGHC
    else main3 ( filter ( `notElem` [ "--make" ] ) files )


main3 :: [ FilePath ] -> Ghc ()
main3 files = do
  ghcOptions <- do
    commandLineArguments <-
      liftIO getArgs

    return ( relayedArguments commandLineArguments \\ files )

  hsBuilder <-
    liftIO ( getDataFileName "compile-hs.nix" )

  targets <-
    traverse ( \filePath -> guessTarget filePath Nothing ) files

  setTargets targets

  moduleGraph <-
    depanal [] True

  let
    modSummaryMap = Map.fromList do
      ms@ModSummary{ ms_location = ModLocation{ ml_hs_file = Just srcFile } } <-
        mgModSummaries moduleGraph

      return ( srcFile, ms )

  hsc_env <-
    getSession

  let
    stronglyConnectedComponents =
      topSortModuleGraph False moduleGraph Nothing

  dependencyGraph <-
    fmap ( Map.unionsWith mappend ) do
      for stronglyConnectedComponents \case
        AcyclicSCC ms@ModSummary{ ms_location = ModLocation{ ml_hs_file = Just srcFile } } -> do
          dependencies <-
            for ( ms_imps ms ) \( package, L _ moduleName ) -> liftIO do
              findImportedModule hsc_env moduleName package >>= \case
                Found ModLocation{ ml_hs_file = Just hsFile } _ ->
                  return [ hsFile ]

                _ ->
                  return []

          return ( Map.singleton srcFile ( concat dependencies ) )

        AcyclicSCC ModSummary{ ms_location = ModLocation{ ml_hs_file = Nothing } } ->
          return mempty

        CyclicSCC{} ->
          return mempty

  ghcPath <- liftIO do
    Just ( Turtle.lineToText -> ghcPath ) <-
      Turtle.fold
        ( Turtle.inproc "which" [ "ghc" ] empty )
        Control.Foldl.head

    return ghcPath

  outputs <- liftIO do
    buildResults <-
      for dependencyGraph \_ -> ( newEmptyMVar :: IO ( MVar Data.Text.Text )  )

    builders <-
      newQSem 1

    forConcurrently_ ( Map.keys dependencyGraph ) \srcFile -> do
      dependencies <-
        transitiveDependencies dependencyGraph buildResults srcFile

      bracket_ ( waitQSem builders ) ( signalQSem builders ) do
        putStrLn ( "Checking " <> srcFile )

        buildResult <-
          tryAny ( nixBuild ghcPath ghcOptions hsBuilder srcFile dependencies modSummaryMap )

        case buildResult of
          Left e -> do
            putStrLn ( "Build for " ++ srcFile ++ " failed" )

            throwIO e

          Right out -> do
            contentAddressableBuildResult <-
              nixMakeContentAddressable out

            putMVar
              ( buildResults Map.! srcFile )
              contentAddressableBuildResult

    traverse readMVar buildResults

  DynFlags{ objectDir, hiDir } <-
    getSessionDynFlags

  for_ objectDir \dir ->
    rsyncFiles [ ".o", ".dyn_o", ".p_o" ] outputs dir

  for_ hiDir \dir ->
    rsyncFiles [ ".hi", ".dyn_hi" ] outputs dir


transitiveDependencies
  :: ( Traversable f, Ord a, Monoid ( f k ), Ord k )
  => Map.Map k ( f k ) -> Map.Map k ( MVar a ) -> k -> IO ( Set.Set a )
transitiveDependencies dependencyGraph buildResults srcFile = do
  let
    sourceDependencies =
      fold ( Map.lookup srcFile dependencyGraph )

  immediateDependencies <-
    fmap concat do
      for sourceDependencies \dep ->
        case Map.lookup dep buildResults of
          Nothing ->
            return []

          Just mvar ->
            return <$> readMVar mvar

  Set.union ( Set.fromList immediateDependencies ) . Set.unions
    <$> traverse
          ( transitiveDependencies dependencyGraph buildResults )
          sourceDependencies


interpretCommandLine :: Ghc [ FilePath ]
interpretCommandLine = do
  commandLineArguments <-
    fmap ( filter ( `notElem` [ "-c" ] ) ) ( liftIO getArgs )

  ( dynFlags, files ) <- do
    initialDynFlags <-
      getSessionDynFlags

    ( newDynFlags, leftOver, _ ) <-
      parseDynamicFlagsCmdLine initialDynFlags ( map noLoc commandLineArguments )

    return ( newDynFlags, leftOver )

  _ <-
    setSessionDynFlags dynFlags

  return ( map unLoc files )


nixBuild
  :: MonadIO m
  => Text
  -> [ String ]
  -> String
  -> String
  -> Set.Set Turtle.Text
  -> Map.Map String ModSummary
  -> m Turtle.Text
nixBuild ghcPath ghcOptions hsBuilder srcFile dependencies modSummaryMap = liftIO do
  canonicalSrcPath <-
    canonicalizePath srcFile

  Just packageDb <-
    Turtle.need "GHC_NIX_PACKAGE_DB"

  Just ( Turtle.lineToText -> out ) <-
    Turtle.fold
      ( Turtle.inproc
          "nix-build"
          [ fromString hsBuilder
          , "--argstr", "ghc", ghcPath
          , "--arg", "hs-path", fromString canonicalSrcPath
          , "--arg", "dependencies", "[" <> Data.Text.intercalate " " ( Set.toList dependencies ) <> "]"
          , "--argstr", "moduleName", fromString ( moduleNameString ( moduleName ( ms_mod ( modSummaryMap Map.! srcFile ) ) ) )
          , "--argstr", "args", Data.Text.intercalate " " ( map fromString ghcOptions )
          , "--argstr", "package-db", packageDb
          ]
          empty
      )
      Control.Foldl.head

  return out


nixMakeContentAddressable :: MonadIO io => Text -> io Text
nixMakeContentAddressable out = liftIO do
  Just ( Turtle.lineToText -> contentAddressableJSON ) <-
    Turtle.fold
      ( Turtle.inproc
          "nix"
          [ "--no-net"
          , "--experimental-features"
          , "nix-command"
          , "make-content-addressable"
          , "--json"
          , out
          ]
          empty
      )
      Control.Foldl.head

  case JSON.decodeStrict ( Data.Text.Encoding.encodeUtf8 contentAddressableJSON ) of
    Just ( JSON.Object keys ) ->
      case KeyMap.lookup "rewrites" keys of
        Just ( JSON.Object outputs ) ->
          case KeyMap.lookup (Key.fromText out) outputs of
            Just ( JSON.String path ) ->
              return path

            _ -> do
              print contentAddressableJSON

              fail "Could not find path in CA result"

        _ ->
          fail "Could not find `rewrites`"

    _ ->
      fail "Unexpected JSON structure"


rsyncFiles
  :: ( MonadIO io, Foldable f, Foldable g )
  => f Text -> g Text -> String -> io Turtle.ExitCode
rsyncFiles suffixes outputs dir = do
  Turtle.proc
    "rsync"
    ( concat
        [ [ "--recursive"
          , "--include=*/"
          ]
        , foldMap ( \suffix -> [ "--include=*" <> suffix ] ) suffixes
        , [ "--exclude=*"
          , "--chmod=u+w"
          ]
        , foldMap ( \output -> [ output <> "/" ] ) outputs
        , [ fromString dir ]
        ]
    )
    empty


proxyToGHC :: MonadIO io => io ()
proxyToGHC = do
  arguments <-
    liftIO getArgs

  void ( Turtle.proc "ghc" ( map fromString arguments ) empty )


relayedArguments :: [ String ] -> [ String ]
relayedArguments ( "--make" : args ) = relayedArguments args
relayedArguments ( "-outputdir" : _ : args ) = relayedArguments args
relayedArguments ( "-odir" : _ : args ) = relayedArguments args
relayedArguments ( "-hidir" : _ : args ) = relayedArguments args
relayedArguments ( "-stubdir" : _ : args ) = relayedArguments args
relayedArguments ( "-package-db" : _ : args ) = relayedArguments args -- TODO We do want to relay this!
relayedArguments ( ( '-' : 'o' : 'p' : 't' : 'P' : _ ) : args ) = relayedArguments args -- TODO We do want to relay this!
relayedArguments ( ( '-' : 'i' : _ ) : args ) = relayedArguments args
relayedArguments ( ( '-' : 'I' : _ ) : args ) = relayedArguments args
relayedArguments ( x : args ) = x : relayedArguments args
relayedArguments [] = []
