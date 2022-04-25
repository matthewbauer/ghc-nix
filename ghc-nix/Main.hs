{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language CPP #-}

module Main ( main ) where

import Paths_ghc_nix ( getDataFileName )

import System.Posix.Directory ( getWorkingDirectory )
import System.Info ( os, arch)
import Data.Graph ( SCC (..) )
import Data.List ( (\\) , isSuffixOf , nub, intercalate )
import Control.Applicative ( empty )
import Control.Exception.Safe ( tryAny, throwIO )
import qualified Control.Foldl
import Control.Monad ( void , when , forM , filterM )
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
import qualified GHC.Paths as GHC
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.FilePath ( takeExtension )
import qualified System.IO as IO
import System.IO.Temp ( getCanonicalTemporaryDirectory, withTempFile )
import qualified Turtle
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Pretty as Cabal
import System.Directory ( listDirectory )
import qualified Data.ByteString as BS
import System.Posix.Files (fileExist)

#if MIN_VERSION_ghc(9, 0, 0)
import qualified GHC.Driver.Session as GHC
import qualified GHC.Unit.Finder as GHC
import qualified GHC.Unit.Module.Graph as GHC
import qualified GHC.Unit.Module.ModSummary as GHC
import qualified GHC.Unit.Types as GHC
#else
import qualified DynFlags as GHC
import qualified Finder as GHC
import qualified HscTypes as GHC
import qualified Module as GHC
#endif

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

  ( ghcOptions, packageDbs, mOfile ) <- do
    commandLineArguments <-
      liftIO getArgs

    when (verbosity > 1) do
      liftIO ( putStrLn ( "Got args: " <> intercalate " " commandLineArguments ) )

    return ( relayedArguments ( commandLineArguments \\ files ) )

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

  dependencyGraph' <-
    fmap concat do
      for stronglyConnectedComponents \case
#if MIN_VERSION_ghc(9, 0, 0)
        AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just _ } } } ) ) -> do
#else
        AcyclicSCC ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just _ } } -> do
#endif
          dependencies <-
            for ( GHC.ms_imps ms ) \( package, GHC.L _ moduleName ) -> liftIO do
              GHC.findImportedModule hsc_env moduleName package >>= \case
                GHC.Found GHC.ModLocation{ GHC.ml_hs_file = Just _ } _ ->
                  return [ ( GHC.moduleNameString moduleName ) ]

                _ ->
                  return []

          return [ ( GHC.moduleNameString ( GHC.ms_mod_name ms ) , concat dependencies ) ]

        _ ->
          return mempty

  let dependencyGraph = Map.fromListWith mappend dependencyGraph'

  let mExeMainModule = -- TODO: this should do the same thing GHC does in this situation
        if Maybe.isJust mOfile
        then Just ( if "Main" `Map.member` dependencyGraph then "Main" else fst ( last dependencyGraph' ) )
        else Nothing

  when (verbosity > 1) do
    liftIO ( putStrLn "Finished finding dependency graph..." )

  let srcFiles =
         Map.unions do
           flip fmap stronglyConnectedComponents \case
#if MIN_VERSION_ghc(9, 0, 0)
             AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } } ) ) ->
#else
             AcyclicSCC ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } ->
#endif
               Map.singleton ( GHC.moduleNameString ( GHC.ms_mod_name ms ) ) srcFile

             _ -> mempty

  pkgNames <- fmap Set.unions ( for stronglyConnectedComponents \case
#if MIN_VERSION_ghc(9, 0, 0)
    AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms } ) ) -> do
#else
    AcyclicSCC ms ->
#endif
      fmap Set.unions ( for ( GHC.ms_imps ms ) \( package, GHC.L _ moduleName ) -> liftIO do
        GHC.findImportedModule hsc_env moduleName package >>= \case
          GHC.Found _ ( GHC.Module pkgName _ ) ->
#if MIN_VERSION_ghc(9, 0, 0)
            return ( Set.singleton ( GHC.unitString pkgName ) )
#else
            return ( Set.singleton ( GHC.unitIdString pkgName ) )
#endif

          _ -> return Set.empty )

    _ -> return Set.empty )

  pkgConfFiles <- fmap concat ( forM packageDbs \pkgConfDir -> liftIO do
    pkgConfFiles <- listDirectory pkgConfDir
    fmap concat ( forM pkgConfFiles \pkgConfFile -> do
      if ( ".conf" `isSuffixOf` pkgConfFile ) then do
        pkgConfText <- BS.readFile ( Turtle.encodeString ( Turtle.decodeString pkgConfDir Turtle.</> Turtle.decodeString pkgConfFile ) )
        case Cabal.parseInstalledPackageInfo pkgConfText of
          Right ( _, pkgConf ) -> do
            let pkgName = Cabal.prettyShow ( Cabal.sourcePackageId pkgConf ) <> "-" <> Cabal.prettyShow ( Cabal.abiHash pkgConf )
            if ( pkgName `Set.member` pkgNames ) then do
              let importDirs = nub ( Cabal.importDirs pkgConf ++ Cabal.libraryDirs pkgConf ++ Cabal.libraryDynDirs pkgConf )
              importDirs' <- filterM fileExist importDirs
              return ( fmap ( \importDir -> ( Map.fromList [ ( "pkgConfDir", pkgConfDir ) , ( "pkgConfFile" , pkgConfFile ) , ( "importDir" , importDir ) ] ) ) importDirs' )
            else return []
          Left _ -> return []
      else return [] ) )

  outputs <- liftIO do
    Just ghcPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc" )
    Just ghcPkgPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc-pkg" )

    let system = arch <> "-" <> os

    bash <- nixBuildTool system "bash" "out"
    coreutils <- nixBuildTool system "coreutils" "out"
    jq <- nixBuildTool system "jq" "bin"
    gnused <- nixBuildTool system "gnused" "out"

    buildResult <- tryAny ( nixBuildHaskell ghcPath ghcOptions hsBuilder ( transitiveDependencies dependencyGraph ) srcFiles verbosity bash coreutils jq system pkgConfFiles ghcPkgPath gnused mExeMainModule )

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

  for_ mOfile \ofile -> do
    for_ outputs \dir -> do
      let exePath = Turtle.fromText dir Turtle.</> "exe"
      exists <- Turtle.testpath exePath
      when exists do
        _ <- Turtle.proc
          "cp"
          [ "-f"
          , fromString ( Turtle.encodeString exePath )
          , fromString ofile
          ]
          empty
        pure ()


transitiveDependencies
  :: Map String [ String ] -> Map String (Set String)
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
  -> [ (Map String String) ]
  -> Text
  -> Text
  -> Maybe String
  -> m [Text]
nixBuildHaskell ghcPath ghcOptions hsBuilder dependencyGraph srcFiles verbosity bash coreutils jq system pkgConfFiles ghcPkgPath gnused exeModuleName = liftIO do
  workingDirectory <- getWorkingDirectory

  dataFiles <- fmap ( Maybe.fromMaybe [] . fmap ( T.splitOn " " ) ) ( Turtle.need "NIX_GHC_DATA_FILES" )

  nativeBuildInputs' <- fmap ( Maybe.fromMaybe [] . fmap ( T.splitOn " " ) ) ( Turtle.need "NIX_GHC_NATIVE_BUILD_INPUTS" )
  let nativeBuildInputs = [ coreutils, jq, gnused ] ++ nativeBuildInputs'

  when (verbosity > 1) do
    putStrLn "Building..."

  let jsonArgs = JSON.object
                   [ "ghcPath" .= ghcPath
                   , "ghcPkgPath" .= ghcPkgPath
                   , "dependencyGraph" .= dependencyGraph
                   , "srcFiles" .= srcFiles
                   , "nativeBuildInputs" .= nativeBuildInputs
                   , "ghcOptions" .= ghcOptions
                   , "pkgConfFiles" .= pkgConfFiles
                   , "dataFiles" .= dataFiles
                   , "workingDirectory" .= workingDirectory
                   , "bash" .= bash
                   , "system" .= system
                   , "exeModuleName" .= exeModuleName ]

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
              , "--argstr", "jsonArgsFile", fromString tmpFile
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


relayedArguments :: [ String ] -> ( [ String ], [ String ] , Maybe String )
relayedArguments ( "--make" : args ) = relayedArguments args
relayedArguments ( "-outputdir" : _ : args ) = relayedArguments args
relayedArguments ( "-odir" : _ : args ) = relayedArguments args
relayedArguments ( "-hidir" : _ : args ) = relayedArguments args
relayedArguments ( "-hiedir" : _ : args ) = relayedArguments args
relayedArguments ( "-stubdir" : _ : args ) = relayedArguments args
relayedArguments ( "-o" : oFile : args ) =
  (\( args' , packageDbs , _ ) -> ( args' , packageDbs , Just oFile ) ) ( relayedArguments args )
relayedArguments ( ( '-' : 'o' : 'p' : 't' : 'P' : _ ) : args ) = relayedArguments args -- TODO We do want to relay this!
relayedArguments ( ( '-' : 'i' : _ ) : args ) = relayedArguments args
relayedArguments ( ( '-' : 'I' : _ ) : args ) = relayedArguments args
relayedArguments ( "-package-db" : packageDb : args ) =
  (\( args' , packageDbs , output ) -> ( args' , packageDb : packageDbs , output ) ) ( relayedArguments args )
relayedArguments ( x : args ) =
  (\( args' , packageDbs , output ) -> ( x : args' , packageDbs , output ) ) ( relayedArguments args )
relayedArguments [] = ( [] , [] , Nothing )
