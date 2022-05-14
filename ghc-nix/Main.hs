{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language CPP #-}

module Main ( main ) where

import Paths_ghc_nix ( getDataFileName )

import System.Posix.Directory ( getWorkingDirectory )
import System.Info ( os, arch )
import Data.Graph ( SCC (..) )
import Data.List ( (\\) , isSuffixOf , nub, intercalate )
import Control.Applicative ( empty )
import Control.Exception.Safe ( tryAny, throwIO )
import qualified Control.Foldl
import Control.Monad ( void , when , unless , forM , filterM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map )
import qualified Data.Maybe as Maybe
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text ( Text )
import qualified Data.Text.Encoding as TE
import Data.Traversable ( for )
import qualified GHC
import GHC ( Ghc )
import qualified GHC.Paths as GHC
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.FilePath ( takeExtension )
import System.IO.Temp ( withSystemTempDirectory )
import qualified Turtle
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Types.UnitId as Cabal
import System.Directory ( listDirectory )
import System.FilePath ( takeDirectory )
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

nixBuildTool :: (MonadIO io, MonadFail io) => String -> Text -> Text -> String -> io Turtle.FilePath
nixBuildTool system name output command = do
  mPath <- Turtle.which ( Turtle.decodeString command )
  case mPath of
    Just path -> do
      realdir <- Turtle.realpath ( Turtle.decodeString ( takeDirectory ( Turtle.encodeString path ) ) )
      return realdir
    Nothing -> do
      Just ( Turtle.lineToText -> bashJSON ) <-
        Turtle.fold
          ( Turtle.inproc
              "nix"
              [ "--extra-experimental-features", "nix-command flakes"
              , "build"
              , "nixpkgs#legacyPackages." <> fromString system <> "." <> name <> "." <> output
              , "--no-link"
              , "--quiet"
              , "--json"
              ]
              empty
          )
          Control.Foldl.head

      Just results <- return ( JSON.decodeStrict ( TE.encodeUtf8 bashJSON ) )

      Just result <- return ( Maybe.listToMaybe results )

      return ( Turtle.fromText ( nixBuildJSONOutputs result Map.! output ) Turtle.</> "bin" )


compileHaskell :: [ FilePath ] -> Int -> Ghc ()
compileHaskell files verbosity = do
  when ( verbosity > 1 ) do
    liftIO ( putStrLn "Starting ghc-nix..." )

  ( ghcOptions, packageDbs, mOfile ) <- do
    commandLineArguments <-
      liftIO getArgs

    when (verbosity > 1) do
      liftIO ( putStrLn ( "Got args: " <> intercalate " " commandLineArguments ) )

    return ( relayedArguments ( commandLineArguments \\ files ) )

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

  when ( verbosity > 1 ) do
    liftIO ( putStrLn "Finding dependency graph..." )

  dependencyGraph' <-
    fmap concat do
      for stronglyConnectedComponents \case
#if MIN_VERSION_ghc(9, 0, 0)
        AcyclicSCC ( GHC.ModuleNode ( GHC.ExtendedModSummary{ GHC.emsModSummary = ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } } ) ) -> do
#else
        AcyclicSCC ms@GHC.ModSummary{ GHC.ms_location = GHC.ModLocation{ GHC.ml_hs_file = Just srcFile } } -> do
#endif
          dependencies <-
            for ( GHC.ms_imps ms ) \( package, GHC.L _ moduleName ) -> liftIO do
              GHC.findImportedModule hsc_env moduleName package >>= \case
                GHC.Found GHC.ModLocation{ GHC.ml_hs_file = Just _ } _ ->
                  return [ GHC.moduleNameString moduleName ]

                _ ->
                  return []

          return [ ( GHC.moduleNameString ( GHC.ms_mod_name ms ) , JSON.object [ "hsPath" .= srcFile, "dependencies" .= concat dependencies ] ) ]

        _ ->
          return mempty

  let dependencyGraph = Map.fromList dependencyGraph'

  let mExeMainModule = -- TODO: this should do the same thing GHC does in this situation
        if Maybe.isJust mOfile
        then Just ( if "Main" `Map.member` dependencyGraph then "Main" else fst ( last dependencyGraph' ) )
        else Nothing

  when ( verbosity > 1 ) do
    liftIO ( putStrLn "Finished finding dependency graph..." )

#if MIN_VERSION_ghc(9, 0, 0)
  GHC.DynFlags{ GHC.homeUnitId_ } <- GHC.getSessionDynFlags
  let homeUnitId = GHC.unitString homeUnitId_
#else
  GHC.DynFlags{ GHC.thisInstalledUnitId } <- GHC.getSessionDynFlags
  let homeUnitId = GHC.installedUnitIdString thisInstalledUnitId
#endif

  outputs <- liftIO do
    buildResult <- tryAny ( nixBuildHaskell ghcOptions dependencyGraph verbosity packageDbs mExeMainModule homeUnitId )

    case buildResult of
      Left e -> do
        putStrLn "Build failed"

        throwIO e

      Right out -> do
        return out

  GHC.DynFlags{ GHC.objectDir, GHC.hiDir, GHC.hieDir } <-
    GHC.getSessionDynFlags

  cacheDir <- case objectDir of
    Just dir -> return ( Turtle.decodeString dir Turtle.</> ".ghc-nix" )
    Nothing -> do
      workingDirectory <- liftIO getWorkingDirectory
      return ( Turtle.decodeString workingDirectory Turtle.</> ".ghc-nix" )
  outputsAndRootsToReplace <- fmap Maybe.catMaybes ( forM outputs \output -> liftIO do
    fileName <- Turtle.readTextFile ( Turtle.fromText output Turtle.</> "nix-support" Turtle.</> "module-path" )
    let root = cacheDir Turtle.</> Turtle.fromText fileName
    exists <- Turtle.testfile root
    if exists then do
      oldOutput <- Turtle.readlink root
      if ( oldOutput /= Turtle.fromText output ) then do
        Turtle.rm root
        return ( Just ( output, root) )
      else return Nothing
    else return Nothing )
  unless ( null outputsAndRootsToReplace ) do
    _ <- liftIO ( Turtle.procs "nix"
      ([ "--extra-experimental-features", "nix-command"
      , "store"
      , "delete"
      ] ++ fmap fst outputsAndRootsToReplace) empty )
    return ()
  forM_ outputsAndRootsToReplace \( output, root ) -> liftIO do
    Turtle.mktree ( Turtle.decodeString ( takeDirectory ( Turtle.encodeString root ) ) )
    _ <- Turtle.procs "nix"
      [ "--extra-experimental-features", "nix-command"
      , "build"
      , output
      , "-o", fromString ( Turtle.encodeString root )
      ] empty
    return ()

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
        _ <- Turtle.procs
          "cp"
          [ "-f"
          , fromString ( Turtle.encodeString exePath )
          , fromString ofile
          ]
          empty
        return ()


interpretCommandLine :: Ghc ( [ FilePath ], Int )
interpretCommandLine = do
  args <- liftIO getArgs

  when ( null args ) do
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
  => [ String ]
  -> Map String JSON.Value
  -> Int
  -> [ String ]
  -> Maybe String
  -> String
  -> m [Text]
nixBuildHaskell ghcOptions dependencyGraph verbosity packageDbs exeModuleName homeUnitId = liftIO do
  hsBuilder <-
    liftIO ( getDataFileName "compile-hs.nix" )

  ( ghcPath, ghcPkgPath, packageDbs' ) <- liftIO do
    ghcPackagePaths <- fmap ( filter (not . null)
                              . fmap T.unpack
                              . Maybe.fromMaybe []
                              . fmap ( T.splitOn ":" ) ) ( Turtle.need "GHC_PACKAGE_PATH" )
    mNixGhc <- Turtle.need "NIX_GHC"
    case mNixGhc of
      Just ghcPath -> do
        Just ghcPkgPath <- Turtle.need "NIX_GHCPKG"
        Just ghcLibDirPath <- Turtle.need "NIX_GHC_LIBDIR"
        let basePackageDb = fromString ( Turtle.encodeString ( Turtle.decodeString ( T.unpack ghcLibDirPath ) Turtle.</> "package.conf.d" ) )
        return ( ghcPath , ghcPkgPath , basePackageDb : ( packageDbs ++ ghcPackagePaths ) )
      Nothing -> do
        Just ghcPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc" )
        Just ghcPkgPath <- fmap ( fmap ( fromString . Turtle.encodeString ) ) ( Turtle.which "ghc-pkg" )
        return ( ghcPath , ghcPkgPath , packageDbs ++ ghcPackagePaths )

  let system = arch <> "-" <> os

  when ( verbosity > 1 ) do
    liftIO ( putStrLn ( "Finding build tools..." ) )

  bash <- fmap ( \path -> path Turtle.</> "bash" ) ( nixBuildTool system "bash" "out" "bash" )
  coreutils <- nixBuildTool system "coreutils" "out" "coreutils"
  jq <- nixBuildTool system "jq" "bin" "jq"
  gnused <- nixBuildTool system "gnused" "out" "sed"

  when ( verbosity > 1 ) do
    liftIO ( putStrLn ( "Finished finding build tools." ) )

  workingDirectory <- getWorkingDirectory

  dataFiles <- fmap ( Maybe.maybe [] ( T.splitOn " " ) ) ( Turtle.need "NIX_GHC_DATA_FILES" )
  dataFilesIgnore <- Turtle.need "NIX_GHC_DATA_FILES_IGNORE"

  path' <- fmap ( Maybe.maybe [] ( T.splitOn ":" ) ) ( Turtle.need "NIX_GHC_PATH" )
  let path = ( fmap ( fromString . Turtle.encodeString ) [ coreutils, jq, gnused ] ) ++ path'

  when (verbosity > 1) do
    putStrLn "Building..."

  json <- withSystemTempDirectory "ghc-nix" \tmpdir -> do
    pkgConfFiles <- fmap concat ( forM packageDbs' \pkgConfDir -> do
      pkgConfFiles <- listDirectory pkgConfDir
      fmap concat ( forM pkgConfFiles \pkgConfFile -> do
        if ".conf" `isSuffixOf` pkgConfFile then do
          pkgConfText <- BS.readFile ( Turtle.encodeString ( Turtle.decodeString pkgConfDir Turtle.</> Turtle.decodeString pkgConfFile ) )
          case Cabal.parseInstalledPackageInfo pkgConfText of
            Right ( _, pkgConf ) -> do
              let pkgName = Cabal.unUnitId ( Cabal.installedUnitId pkgConf )
              if pkgName /= homeUnitId then do
                let importDirs = nub ( Cabal.importDirs pkgConf ++ Cabal.libraryDirs pkgConf )
                importDirs' <- flip filterM importDirs \importDir -> do
                  exists' <- fileExist importDir
                  unless exists' do
                    putStrLn ( "Directory " <> importDir <> " doesn't exist, skipping " <> pkgName )
                  pure exists'
                return ( fmap ( \importDir -> Map.fromList [ ( "pkgConfDir", pkgConfDir ) , ( "pkgConfFile" , pkgConfFile ) , ( "importDir" , importDir ) ] ) importDirs' )
              else return []
            Left _ -> return []
        else return [] ) )

    let jsonArgs = JSON.object
                     [ "ghcPath" .= ghcPath
                     , "ghcPkgPath" .= ghcPkgPath
                     , "dependencyGraph" .= dependencyGraph
                     , "PATH" .= path
                     , "ghcOptions" .= ghcOptions
                     , "pkgConfFiles" .= ( pkgConfFiles :: [ Map String String ] )
                     , "dataFiles" .= dataFiles
                     , "dataFilesIgnore" .= dataFilesIgnore
                     , "workingDirectory" .= workingDirectory
                     , "bash" .= Turtle.encodeString bash
                     , "system" .= system
                     , "exeModuleName" .= exeModuleName ]

    let jsonFile = Turtle.decodeString tmpdir Turtle.</> "ghc-nix-args.json"

    when (verbosity > 1) do
      putStrLn "Writing json..."
    JSON.encodeFile ( Turtle.encodeString jsonFile ) jsonArgs
    when (verbosity > 1) do
      putStrLn "Running nix build..."
    Just ( Turtle.lineToText -> json ) <-
      Turtle.fold
        ( Turtle.inproc
            "nix"
            ( [ "--extra-experimental-features", "nix-command"
              , "build"
              , "-f", fromString hsBuilder
              , "--argstr", "jsonArgsFile", fromString ( Turtle.encodeString jsonFile )
              , "--no-link"
              , "--json"
              , "-L"
              ] ++ [ "-vvvvv" | verbosity >= 5 ]
                ++ [ "--quiet" | verbosity < 2 ] )
            empty
        )
        Control.Foldl.head
    return json

  when ( verbosity > 1 ) do
    putStrLn "Decoding json..."

  Just results <- return ( JSON.decodeStrict ( TE.encodeUtf8 json ) )

  when ( verbosity > 1 ) do
    putStrLn ( "Finished building " <> show ( length ( Map.keys dependencyGraph ) ) <> " modules" )
    putStrLn ( "Got " <> show ( length results ) <> " results" )

  return ( fmap ( \result -> nixBuildJSONOutputs result Map.! "out" ) results )


rsyncFiles
  :: ( MonadIO io, Foldable f, Foldable g )
  => f Text -> g Text -> String -> io ()
rsyncFiles suffixes outputs dir = do
  Turtle.procs
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

  void ( Turtle.procs "ghc" ( map fromString arguments ) empty )


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
