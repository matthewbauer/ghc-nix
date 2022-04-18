{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Paths_ghc_nix ( getDataFileName )

import System.Posix.Directory ( getWorkingDirectory )
import System.Info ( os, arch)
import Data.Graph ( SCC (..) )
import Data.List ( (\\) )
import Control.Applicative ( empty )
import Control.Exception.Safe ( tryAny, throwIO )
import qualified Control.Foldl
import Control.Monad ( void , forM, when )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Set ( Set )
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text ( Text )
import qualified Data.Text.Encoding as TE
import Data.Traversable ( for )
import qualified GHC as GHC
import GHC ( Ghc )
import qualified GHC.Driver.Session as GHC
import qualified GHC.Paths as GHC
import qualified GHC.Unit.Finder as GHC
import qualified GHC.Unit.Module.Graph as GHC
import qualified GHC.Unit.Module.ModSummary as GHC
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.FilePath ( takeExtension )
import qualified System.IO as IO
import System.IO.Temp ( getCanonicalTemporaryDirectory, withTempFile )
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

    _ -> GHC.runGhc ( Just GHC.libdir ) do
      ( files, verbosity ) <-
        interpretCommandLine

      if ".c" `elem` map takeExtension files || ".o" `elem` map takeExtension files || ".dyn_o" `elem` map takeExtension files
        then proxyToGHC
        else compileHaskell ( filter ( `notElem` [ "--make" ] ) files ) verbosity


data NixBuildJSON = NixBuildJSON { nixBuildJSONDrvPath :: Text, nixBuildJSONOutputs :: Map Text Text }

instance JSON.FromJSON NixBuildJSON where
  parseJSON = JSON.withObject "NixBuildJSON" \o -> do
    nixBuildJSONDrvPath <- o .: "drvPath"
    nixBuildJSONOutputs <- o .: "outputs"
    return NixBuildJSON { nixBuildJSONDrvPath, nixBuildJSONOutputs }

nixBuildTool :: (MonadIO io, MonadFail io) => String -> Text -> Text -> io Text
nixBuildTool system name output = do
  Just ( Turtle.lineToText -> bashJSON ) <-
    Turtle.fold
      ( Turtle.inproc
          "nix"
          ( [ "--extra-experimental-features", "nix-command flakes"
            , "build"
            , "nixpkgs#legacyPackages." <> fromString system <> "." <> name <> "." <> output
            , "--no-link"
            , "--quiet"
            , "--json"
            ] )
          empty
      )
      Control.Foldl.head

  Just results <- return ( JSON.decodeStrict ( TE.encodeUtf8 bashJSON ) )

  Just result <- return ( Maybe.listToMaybe results )

  return ( nixBuildJSONOutputs result Map.! output )


compileHaskell :: [ FilePath ] -> Int -> Ghc ()
compileHaskell files verbosity = do
  when (verbosity > 1) do
    liftIO ( putStrLn "Starting ghc-nix..." )

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

  hsc_env <-
    GHC.getSession

  let
    stronglyConnectedComponents =
      GHC.topSortModuleGraph False moduleGraph Nothing

  when (verbosity > 1) do
    liftIO ( putStrLn "Finding dependency graph..." )

  dependencyGraph <-
    fmap ( Map.unionsWith mappend ) do
      for stronglyConnectedComponents \case
        AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just _ } } } ) ) -> do
          dependencies <-
            for ( GHC.ms_imps ms ) \( package, GHC.L _ moduleName ) -> liftIO do
              GHC.findImportedModule hsc_env moduleName package >>= \case
                GHC.Found GHC.ModLocation{ GHC.ml_hs_file = Just _ } _ ->
                  return [ ( GHC.moduleNameString moduleName ) ]

                _ ->
                  return []

          return ( Map.singleton ( GHC.moduleNameString ( GHC.ms_mod_name ms ) ) ( concat dependencies ) )

        AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Nothing } } } ) ) ->
          return mempty

        AcyclicSCC ( GHC.InstantiationNode _ ) ->
          return mempty

        CyclicSCC{} ->
          return mempty

  when (verbosity > 1) do
    liftIO ( putStrLn "Finished finding dependency graph..." )

  let srcFiles =
         Map.unions do
           flip fmap stronglyConnectedComponents \case
             AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } } ) ) ->
               Map.singleton ( GHC.moduleNameString ( GHC.ms_mod_name ms ) ) srcFile

             AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Nothing } } } ) ) -> mempty

             AcyclicSCC ( GHC.InstantiationNode _ ) -> mempty

             CyclicSCC{} -> mempty

  outputs <- liftIO do
    Just ghcPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc" )

    let system = arch <> "-" <> os

    bash <- nixBuildTool system "bash" "out"
    coreutils <- nixBuildTool system "coreutils" "out"
    jq <- nixBuildTool system "jq" "bin"

    buildResult <- tryAny ( nixBuildHaskell ghcPath ghcOptions hsBuilder ( transitiveDependencies dependencyGraph ) srcFiles verbosity bash coreutils jq system )

    case buildResult of
      Left e -> do
        putStrLn "Build failed"

        throwIO e

      Right out -> do
        return out

  GHC.DynFlags{ GHC.objectDir, GHC.hiDir, GHC.hieDir } <-
    GHC.getSessionDynFlags

  for_ objectDir \dir ->
    rsyncFiles [ ".o", ".dyn_o", ".p_o" ] outputs dir

  for_ hiDir \dir ->
    rsyncFiles [ ".hi", ".dyn_hi" ] outputs dir

  for_ hieDir \dir ->
    rsyncFiles [ ".hie" ] outputs dir


transitiveDependencies
  :: Map.Map String [ String ] -> Map.Map String (Set String)
transitiveDependencies dependencyGraph =
  let dependencyGraph' = flip fmap dependencyGraph \dependencies ->
        Set.union ( Set.fromList dependencies ) ( Set.unions ( flip fmap dependencies \dependency -> dependencyGraph' Map.! dependency ) )
  in dependencyGraph'

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
      GHC.parseDynamicFlagsCmdLine initialDynFlags ( map GHC.noLoc commandLineArguments )

    return ( newDynFlags, leftOver )

  _ <-
    GHC.setSessionDynFlags dynFlags

  return ( map GHC.unLoc files, GHC.verbosity dynFlags )


nixBuildHaskell
  :: MonadIO m
  => Text
  -> [ String ]
  -> String
  -> Map String (Set String)
  -> Map String FilePath
  -> Int
  -> Text
  -> Text
  -> Text
  -> String
  -> m [Text]
nixBuildHaskell ghcPath ghcOptions hsBuilder dependencyGraph srcFiles verbosity bash coreutils jq system = liftIO do
  workingDirectory <- getWorkingDirectory

  dataFiles <- fmap ( Maybe.fromMaybe [] . fmap ( T.splitOn " " ) ) ( Turtle.need "NIX_GHC_DATA_FILES" )

  nativeBuildInputs' <- fmap ( Maybe.fromMaybe [] . fmap ( T.splitOn " " ) ) ( Turtle.need "NIX_GHC_NATIVE_BUILD_INPUTS" )
  let nativeBuildInputs = [ coreutils, jq ] ++ nativeBuildInputs'

  mGhcLibDir <- Turtle.need "NIX_GHC_LIBDIR"
  mPackageDb <- forM mGhcLibDir \ghcLibDir -> do
    Right packageDb <- return ( Turtle.toText ( Turtle.fromText ghcLibDir Turtle.</> "package.conf.d" ) )
    return packageDb

  when (verbosity > 1) do
    putStrLn "Building..."

  let jsonArgs = JSON.object
                   [ "ghcPath" .= ghcPath
                   , "dependencyGraph" .= dependencyGraph
                   , "srcFiles" .= srcFiles
                   , "nativeBuildInputs" .= nativeBuildInputs
                   , "ghcOptions" .= ghcOptions
                   , "packageDb" .= mPackageDb
                   , "dataFiles" .= dataFiles
                   , "workingDirectory" .= workingDirectory
                   , "bash" .= bash
                   , "system" .= system ]

  tmpdir <- getCanonicalTemporaryDirectory
  json <- withTempFile tmpdir "ghc-nix-args.json" \tmpFile handle -> do
    IO.hClose handle
    when (verbosity > 1) do
      putStrLn "Writing json..."
    JSON.encodeFile tmpFile jsonArgs
    when (verbosity > 1) do
      putStrLn "Running nix build..."
    Just ( Turtle.lineToText -> json ) <-
      Turtle.fold
        ( Turtle.inproc
            "nix"
            ( [ "--extra-experimental-features", "nix-command"
              , "build"
              , "-f", fromString hsBuilder
              , "--argstr", "jsonArgsFile", T.pack tmpFile
              , "--no-link"
              , "--json"
              , "--offline"
              , "--builders", ""
              , "-L"
              ] ++ if verbosity < 2 then [ "--quiet" ] else [] )
            empty
        )
        Control.Foldl.head
    return json

  when ( verbosity > 1 ) do
    putStrLn "Decoding json..."

  Just results <- return ( JSON.decodeStrict ( TE.encodeUtf8 json ) )

  when ( verbosity > 1 ) do
    putStrLn ( "Finished building " <> show ( length ( Map.keys dependencyGraph ) ) <> " modules" )

  return ( flip fmap results ( \result -> nixBuildJSONOutputs result Map.! "out" ) )


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
relayedArguments ( "-hiedir" : _ : args ) = relayedArguments args
relayedArguments ( "-stubdir" : _ : args ) = relayedArguments args
relayedArguments ( "-package-db" : _ : args ) = relayedArguments args -- TODO We do want to relay this!
relayedArguments ( ( '-' : 'o' : 'p' : 't' : 'P' : _ ) : args ) = relayedArguments args -- TODO We do want to relay this!
relayedArguments ( ( '-' : 'i' : _ ) : args ) = relayedArguments args
relayedArguments ( ( '-' : 'I' : _ ) : args ) = relayedArguments args
relayedArguments ( x : args ) = x : relayedArguments args
relayedArguments [] = []
