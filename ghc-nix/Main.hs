{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Paths_ghc_nix ( getDataFileName )

import System.Directory ( canonicalizePath )
import System.Posix.Directory ( getWorkingDirectory )
import Data.List ( (\\) )
import Control.Applicative ( empty )
import Control.Concurrent.MVar
import Control.Exception.Safe ( tryAny, throwIO )
import qualified Control.Foldl
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String ( fromString )
import qualified Data.Text
import Data.Text ( Text )
import qualified Data.Text.Encoding
import Data.Traversable ( for )
import Digraph
import DynFlags
import Finder
import qualified GHC as GHC
import GHC ( Ghc )
import GHC.Paths ( libdir )
import HscTypes
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.FilePath ( takeExtension )
import qualified Turtle
import UnliftIO.Async ( pooledForConcurrently_ )


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

    _ -> GHC.runGhc ( Just libdir ) do
      ( files, verbosity ) <-
        interpretCommandLine

      if ".c" `elem` map takeExtension files || ".o" `elem` map takeExtension files || ".dyn_o" `elem` map takeExtension files
        then proxyToGHC
        else compileHaskell ( filter ( `notElem` [ "--make" ] ) files ) verbosity

compileHaskell :: [ FilePath ] -> Int -> Ghc ()
compileHaskell files verbosity = do
  ghcOptions <- do
    commandLineArguments <-
      liftIO getArgs

    return ( relayedArguments commandLineArguments \\ files )

  hsBuilder <-
    liftIO ( getDataFileName "compile-hs.nix" )

  targets <-
    traverse ( \filePath -> GHC.guessTarget filePath Nothing ) files

  GHC.setTargets targets

  moduleGraph <-
    GHC.depanal [] True

  let
    modSummaryMap = Map.fromList do
      ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } <-
        mgModSummaries moduleGraph

      return ( srcFile, ms )

  hsc_env <-
    GHC.getSession

  let
    stronglyConnectedComponents =
      GHC.topSortModuleGraph False moduleGraph Nothing

    topoSortedSrcFiles = flip Maybe.mapMaybe stronglyConnectedComponents \case
      AcyclicSCC GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = srcFile } } -> srcFile
      CyclicSCC{} -> Nothing

  dependencyGraph <-
    fmap ( Map.unionsWith mappend ) do
      for stronglyConnectedComponents \case
        AcyclicSCC ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } -> do
          dependencies <-
            for ( ms_imps ms ) \( package, GHC.L _ moduleName ) -> liftIO do
              findImportedModule hsc_env moduleName package >>= \case
                Found GHC.ModLocation{ GHC.ml_hs_file = Just hsFile } _ ->
                  return [ hsFile ]

                _ ->
                  return []

          return ( Map.singleton srcFile ( concat dependencies ) )

        AcyclicSCC GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Nothing } } ->
          return mempty

        CyclicSCC{} ->
          return mempty

  outputs <- liftIO do
    Just ghcPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc" )

    buildResults <-
      for dependencyGraph \_ -> ( newEmptyMVar :: IO ( MVar Data.Text.Text )  )

    let totalModules = length topoSortedSrcFiles

    numCompiled <- newMVar ( 0 :: Int )

    pooledForConcurrently_ topoSortedSrcFiles \srcFile -> do
      dependencies <-
        transitiveDependencies dependencyGraph buildResults srcFile

      let moduleName = GHC.moduleNameString ( GHC.moduleName ( GHC.ms_mod ( modSummaryMap Map.! srcFile ) ) )
      modifyMVar_ numCompiled \n -> do
        let n' = n + 1
        putStrLn ( "[ " <> show n' <> " of " <> show totalModules <> "]  Compiling " <> moduleName <> " ( " <> srcFile <> " )" )
        return n'

      buildResult <-
        tryAny ( nixBuild ghcPath ghcOptions hsBuilder srcFile dependencies moduleName verbosity )

      case buildResult of
        Left e -> do
          putStrLn ( "Build for " <> srcFile <> " failed" )

          throwIO e

        Right out -> do
          contentAddressableBuildResult <-
            nixMakeContentAddressable out

          putMVar
            ( buildResults Map.! srcFile )
            contentAddressableBuildResult

    traverse readMVar buildResults

  DynFlags{ objectDir, hiDir } <-
    GHC.getSessionDynFlags

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


interpretCommandLine :: Ghc ( [ FilePath ], Int )
interpretCommandLine = do
  args <- liftIO getArgs

  Turtle.when ( null args ) do
    liftIO ( putStrLn "Provide Haskell files as arguments." )
    liftIO exitFailure

  let commandLineArguments = filter ( `notElem` [ "-c" ] ) args

  ( dynFlags, files ) <- do
    initialDynFlags <-
      GHC.getSessionDynFlags

    ( newDynFlags, leftOver, _ ) <-
      parseDynamicFlagsCmdLine initialDynFlags ( map GHC.noLoc commandLineArguments )

    return ( newDynFlags, leftOver )

  _ <-
    GHC.setSessionDynFlags dynFlags

  return ( map GHC.unLoc files, verbosity dynFlags )


nixBuild
  :: MonadIO m
  => Text
  -> [ String ]
  -> String
  -> String
  -> Set.Set Turtle.Text
  -> String
  -> Int
  -> m Turtle.Text
nixBuild ghcPath ghcOptions hsBuilder srcFile dependencies moduleName verbosity = liftIO do
  canonicalSrcPath <-
    canonicalizePath srcFile

  workingDirectory <- getWorkingDirectory

  Just ghcLibDir <-
    Turtle.need "NIX_GHC_LIBDIR"

  Just dataFiles <- fmap ( fmap ( Data.Text.splitOn " " ) ) ( Turtle.need "NIX_GHC_DATA_FILES" )

  Right packageDb <- pure ( Turtle.toText ( Turtle.fromText ghcLibDir Turtle.</> "package.conf.d" ) )

  Just ( Turtle.lineToText -> out ) <-
    Turtle.fold
      ( Turtle.inproc
          "nix-build"
          ( [ fromString hsBuilder
            , "--argstr", "ghc", ghcPath
            , "--arg", "hs-path", fromString canonicalSrcPath
            , "--arg", "dependencies", "[" <> Data.Text.intercalate " " ( Set.toList dependencies ) <> "]"
            , "--argstr", "moduleName", fromString moduleName
            , "--argstr", "args", Data.Text.intercalate " " ( map fromString ghcOptions )
            , "--argstr", "package-db", packageDb
            , "--arg", "dataFiles", "[" <> Data.Text.intercalate " " ( map ( \dataFile -> "\"" <> dataFile <> "\"" ) dataFiles ) <> "]"
            , "--argstr", "workingDirectory", fromString workingDirectory
            , "--no-out-link"
            ] ++ if verbosity < 2 then [ "--quiet" ] else [] )
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
          , "store"
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
          case KeyMap.lookup ( Key.fromText out ) outputs of
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
