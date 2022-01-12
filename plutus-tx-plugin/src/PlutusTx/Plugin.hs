{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module PlutusTx.Plugin
    ( plugin
    , plc
    -- the following symbols *must* be exported
    -- for plugins to work under cross compilation.
    , mkCompiledCode
    )
    where

import Data.Bifunctor
import PlutusTx.Code
import PlutusTx.Compiler.Builtins
import PlutusTx.Compiler.Error
import PlutusTx.Compiler.Expr
import PlutusTx.Compiler.Types
import PlutusTx.Compiler.Utils
import PlutusTx.Coverage
import PlutusTx.PIRTypes
import PlutusTx.PLCTypes
import PlutusTx.Plugin.Utils
import PlutusTx.Trace

import Finder qualified as GHC
import GhcPlugins qualified as GHC
import LoadIface qualified as GHC
import OccName qualified as GHCO
import Panic qualified as GHC
import TcRnMonad qualified as GHC

import PlutusCore qualified as PLC
import PlutusCore.Pretty as PLC
import PlutusCore.Quote

import UntypedPlutusCore qualified as UPLC

import PlutusIR qualified as PIR
import PlutusIR.Compiler qualified as PIR
import PlutusIR.Compiler.Definitions qualified as PIR

import Language.Haskell.TH.Syntax as TH hiding (lift)

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer hiding (All)
import Flat (Flat, flat, unflat)

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSUnsafe
import Data.Char (isDigit)
import Data.Foldable (fold)
import Data.List (intercalate, isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Traversable (for)
import ErrorCode
import FamInstEnv qualified as GHC
import Prettyprinter qualified as PP
import Text.Read (readMaybe)

import System.IO (openTempFile)
import System.IO.Unsafe (unsafePerformIO)

data PluginOptions = PluginOptions {
    poDoTypecheck                      :: Bool
    , poDeferErrors                    :: Bool
    , poContextLevel                   :: Int
    , poDumpPir                        :: Bool
    , poDumpPlc                        :: Bool
    , poDumpUPlc                       :: Bool
    , poOptimize                       :: Bool
    , poPedantic                       :: Bool
    , poVerbose                        :: Bool
    , poDebug                          :: Bool
    , poMaxSimplifierIterations        :: Int
    , poDoSimplifierUnwrapCancel       :: Bool
    , poDoSimplifierBeta               :: Bool
    , poDoSimplifierInline             :: Bool
    , poDoSimplifierRemoveDeadBindings :: Bool
    , poProfile                        :: ProfileOpts
    , poCoverage                       :: CoverageOpts

    -- Setting to `True` defines `trace` as `\_ a -> a` instead of the builtin version.
    -- Which effectively ignores the trace text.
    , poRemoveTrace                    :: Bool
    }

data PluginCtx = PluginCtx
    { pcOpts            :: PluginOptions
    , pcFamEnvs         :: GHC.FamInstEnvs
    , pcMarkerName      :: GHC.Name
    , pcModuleName      :: GHC.ModuleName
    , pcModuleModBreaks :: Maybe GHC.ModBreaks
    }

{- Note [Making sure unfoldings are present]
Our plugin runs at the start of the Core pipeline. If we look around us, we will find
that as expected, we have unfoldings for some bindings from other modules or packages
depending on whether GHC thinks they're good to inline/are marked INLINEABLE.

But there will be no unfoldings for local bindings!

It turns out that these are added by the simplifier, of all things. To avoid relying too
much on the shape of the subsequent passes, we add a single, very gentle, simplifier
pass before we run, turning off everything that we can and running only once.

This means that we need to be robust to the transformations that the simplifier performs
unconditionally which we pretty much are.

See https://gitlab.haskell.org/ghc/ghc/issues/16615 for upstream discussion.
-}

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.pluginRecompile = GHC.flagRecompile
                           , GHC.installCoreToDos = install
                           }
    where
      install :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
      install args rest = do
          -- create simplifier pass to be placed at the front
          simplPass <- mkSimplPass <$> GHC.getDynFlags
          -- instantiate our plugin pass
          pluginPass <- mkPluginPass <$> parsePluginArgs args
          -- return the pipeline
          pure $
             simplPass
             : pluginPass
             : rest

-- | A simplifier pass, implemented by GHC
mkSimplPass :: GHC.DynFlags -> GHC.CoreToDo
mkSimplPass flags =
  -- See Note [Making sure unfoldings are present]
  GHC.CoreDoSimplify 1 $ GHC.SimplMode {
              GHC.sm_names = ["Ensure unfoldings are present"]
            , GHC.sm_phase = GHC.InitialPhase
            , GHC.sm_dflags = flags
            , GHC.sm_rules = False
            -- You might think you would need this, but apparently not
            , GHC.sm_inline = False
            , GHC.sm_case_case = False
            , GHC.sm_eta_expand = False
            }

-- | Parses the arguments that were given to ghc at commandline as "-fplugin-opt PlutusTx.Plugin:arg1"
parsePluginArgs :: [GHC.CommandLineOption] -> GHC.CoreM PluginOptions
parsePluginArgs args = do
    let opts = PluginOptions {
            poDoTypecheck = notElem' "no-typecheck"
            , poDeferErrors = elem' "defer-errors"
            , poContextLevel = if elem' "no-context" then 0 else if elem "debug-context" args then 3 else 1
            , poDumpPir = elem' "dump-pir"
            , poDumpPlc = elem' "dump-plc"
            , poDumpUPlc = elem' "dump-uplc"
            , poOptimize = notElem' "no-optimize"
            , poPedantic = elem' "pedantic"
            , poVerbose = elem' "verbose"
            , poDebug = elem' "debug"
            , poMaxSimplifierIterations = maxIterations
            -- Simplifier Passes
            , poDoSimplifierUnwrapCancel = notElem' "no-simplifier-unwrap-cancel"
            , poDoSimplifierBeta = notElem' "no-simplifier-beta"
            , poDoSimplifierInline = notElem' "no-simplifier-inline"
            , poDoSimplifierRemoveDeadBindings = notElem' "no-simplifier-remove-dead-bindings"
            -- profiling: @profile-all@ turns on profiling for everything
            , poProfile =
                if elem' "profile-all"
                then All
                else None
            , poCoverage = CoverageOpts . Set.fromList $
                   [ l | l <- [minBound .. maxBound], elem' "coverage-all" ]
                ++ [ LocationCoverage  | elem' "coverage-location"  ]
                ++ [ BooleanCoverage  | elem' "coverage-boolean"  ]
            , poRemoveTrace = elem' "remove-trace"
            }
    -- TODO: better parsing with failures
    pure opts
    where
        elem' :: String -> Bool
        elem' = flip elem args
        notElem' :: String -> Bool
        notElem' = flip notElem args
        prefix :: String
        prefix = "max-simplifier-iterations="
        defaultIterations :: Int
        defaultIterations = view PIR.coMaxSimplifierIterations PIR.defaultCompilationOpts
        maxIterations :: Int
        maxIterations = case filter (isPrefixOf prefix) args of
            match : _ ->
                let val = drop (length prefix) match in
                    fromMaybe defaultIterations (readMaybe val)
            _ -> defaultIterations


{- Note [Marker resolution]
We use TH's 'foo exact syntax for resolving the 'plc marker's ghc name, as
explained in: <http://hackage.haskell.org/package/ghc-8.10.1/docs/GhcPlugins.html#v:thNameToGhcName>

The GHC haddock suggests that the "exact syntax" will always succeed because it is statically resolved here (inside this Plugin module);

If this is the case, then it means that our plugin will always traverse each module's binds searching for plc markers
even in the case that the `plc` name is not in scope locally in the module under compilation.

The alternative is to use the "dynamic syntax" (`TH.mkName "plc"`), which implies that
the "plc" name will be resolved dynamically during module's compilation. In case "plc" is not locally in scope,
the plugin would finish faster by completely skipping the module under compilation. This dynamic approach
comes with its own downsides however, because the user may have imported "plc" qualified or aliased it, which will fail to resolve.
-}

loadName_maybe :: String -> GHC.ModuleName -> GHC.NameSpace -> String -> GHC.CoreM (Maybe GHC.Name)
loadName_maybe pkg mod_name namespace occ = do
  hsc_env <- GHC.getHscEnv
  fmap (fmap fst) (lookupRdrNameInModule hsc_env
                                           (GHC.mkFastString pkg)
                                           mod_name
                                           (GHC.mkUnqual namespace $ GHC.mkFastString occ))

lookupRdrNameInModule :: (GHC.HasDynFlags m, MonadIO m) => GHC.HscEnv -> GHC.FastString -> GHC.ModuleName
                      -> GHC.RdrName -> m (Maybe (GHC.Name, GHC.ModIface))
lookupRdrNameInModule hsc_env pkg mod_name rdr_name' = do
    -- First find the package the module resides in by searching exposed packages and home modules
    -- Fixme: package name completely removed from query, since name mangling on macOS breaks the lookup
    found_module0 <- liftIO $ GHC.findExposedPackageModule hsc_env mod_name Nothing {-(Just pkg)-}
    let found_module = case found_module0 of
                        (GHC.NotFound _paths _missing_hi [mod_hidden_unit] _pkgs_hidden _unusables _suggs) ->
                           GHC.Found undefined (GHC.mkModule mod_hidden_unit mod_name)
                        (GHC.NotFound _paths _missing_hi _mod_hidden_unit [pkg_hidden_unit] _unusables _suggs) ->
                           GHC.Found undefined (GHC.mkModule pkg_hidden_unit mod_name)
                        x -> x
        rdr_name = GHC.Unqual (GHC.mkOccName (GHC.occNameSpace (GHC.rdrNameOcc rdr_name')) (GHC.unpackFS (GHC.occNameFS (GHC.rdrNameOcc rdr_name'))))
    case found_module of
        GHC.Found _ mod -> do
            -- Find the exports of the module
            (_, mb_iface) <- liftIO $ GHC.initTcInteractive hsc_env $
                             GHC.initIfaceTcRn $
                             GHC.loadPluginInterface doc mod -- fixme is loadPluginInterface correct?
            case mb_iface of
                Just iface -> do
                    -- Try and find the required name in the exports
                    let decl_spec = GHC.ImpDeclSpec { GHC.is_mod = mod_name, GHC.is_as = mod_name
                                                , GHC.is_qual = False, GHC.is_dloc = GHC.noSrcSpan }
                        imp_spec = GHC.ImpSpec decl_spec GHC.ImpAll
                        env = GHC.mkGlobalRdrEnv (GHC.gresFromAvails (Just imp_spec) (GHC.mi_exports iface))
                    case lookupGRE_RdrName rdr_name env of
                        [gre] -> return (Just (GHC.gre_name gre, iface))
                        []    -> return Nothing
                        _     -> GHC.panic "lookupRdrNameInModule"

                Nothing -> liftIO . throwCmdLineErrorS dflags $ GHC.hsep [GHC.text "Could not determine the exports of the module", GHC.ppr mod_name]
        err -> liftIO . throwCmdLineErrorS dflags $ GHC.cannotFindModule dflags mod_name err
  where
    dflags = GHC.hsc_dflags hsc_env
    doc = GHC.text "contains a name used in an invocation of lookupRdrNameInModule"

lookupGRE_RdrName :: GHC.RdrName -> GHC.GlobalRdrEnv -> [GHC.GlobalRdrElt]
lookupGRE_RdrName rdr_name env
  = case GHC.lookupOccEnv env (GHC.rdrNameOcc rdr_name) of
    Nothing   -> []
    Just gres -> GHC.pickGREs rdr_name gres

throwCmdLineErrorS :: GHC.DynFlags -> GHC.SDoc -> IO a
throwCmdLineErrorS dflags = throwCmdLineError . GHC.showSDoc dflags

throwCmdLineError :: String -> IO a
throwCmdLineError = GHC.throwGhcExceptionIO . GHC.CmdLineError

-- | Our plugin works at haskell-module level granularity; the plugin
-- looks at the module's top-level bindings for plc markers and compiles their right-hand-side core expressions.
mkPluginPass :: PluginOptions -> GHC.CoreToDo
mkPluginPass opts = GHC.CoreDoPluginPass "Core to PLC" $ \ guts -> do
    -- Family env code borrowed from SimplCore
    p_fam_env <- GHC.getPackageFamInstEnv
    -- See Note [Marker resolution]
    maybeMarkerName <- thNameToGhcName 'plc
    case maybeMarkerName of
        -- TODO: test that this branch can happen using TH's 'plc exact syntax. See Note [Marker resolution]
        Nothing -> pure guts
        Just markerName ->
            let pctx = PluginCtx { pcOpts = opts
                                 , pcFamEnvs = (p_fam_env, GHC.mg_fam_inst_env guts)
                                 , pcMarkerName = markerName
                                 , pcModuleName = GHC.moduleName $ GHC.mg_module guts
                                 , pcModuleModBreaks = GHC.mg_modBreaks guts
                                 }
                -- start looking for plc calls from the top-level binds
            in GHC.bindsOnlyPass (runPluginM pctx . traverse compileBind) guts

-- | The monad where the plugin runs in for each module.
-- It is a core->core compiler monad, called PluginM, augmented with pure errors.
type PluginM uni fun = ReaderT PluginCtx (ExceptT (CompileError uni fun) GHC.CoreM)

-- | Runs the plugin monad in a given context; throws a Ghc.Exception when compilation fails.
runPluginM :: (PLC.GShow uni, PLC.Closed uni, PLC.Everywhere uni PLC.PrettyConst, PP.Pretty fun)
           => PluginCtx -> PluginM uni fun a -> GHC.CoreM a
runPluginM pctx act = do
    res <- runExceptT $ runReaderT act pctx
    case res of
        Right x -> pure x
        Left err ->
            let errInGhc = GHC.ProgramError $ "GHC Core to PLC plugin: " ++ show (PP.pretty (errorCode err) <> ":" <> PP.pretty err)
            in liftIO $ GHC.throwGhcExceptionIO errInGhc

-- | Compiles all the marked expressions in the given binder into PLC literals.
compileBind :: GHC.CoreBind -> PluginM PLC.DefaultUni PLC.DefaultFun GHC.CoreBind
compileBind = \case
    GHC.NonRec b rhs -> GHC.NonRec b <$> compileMarkedExprs rhs
    GHC.Rec bindsRhses -> GHC.Rec <$> (for bindsRhses $ \(b, rhs) -> do
                                             rhs' <- compileMarkedExprs rhs
                                             pure (b, rhs'))

{- Note [Hooking in the plugin]
Working out what to process and where to put it is tricky. We are going to turn the result in
to a 'CompiledCode', not the Haskell expression we started with!

Currently we look for calls to the @plc :: a -> CompiledCode a@ function, and we replace the whole application with the
generated code object, which will still be well-typed.
-}

{- Note [Polymorphic values and Any]
If you try and use the plugin on a polymorphic expression, then GHC will replace the quantified types
with 'Any' and remove the type lambdas. This is pretty annoying, and I don't entirely understand
why it happens, despite poking around in GHC a fair bit.

Possibly it has to do with the type that is given to 'plc' being unconstrained, resulting in GHC
putting 'Any' there, and that then propagating into the type of the quote. It's tricky to experiment
with this, since you can't really specify a polymorphic type in a type application or in the resulting
'CompiledCode' because that's impredicative polymorphism.
-}

-- | Compiles all the core-expressions surrounded by plc in the given expression into PLC literals.
compileMarkedExprs :: GHC.CoreExpr -> PluginM PLC.DefaultUni PLC.DefaultFun GHC.CoreExpr
compileMarkedExprs expr = do
    markerName <- asks pcMarkerName
    case expr of
      GHC.App (GHC.App (GHC.App (GHC.App
                          -- function id
                          -- sometimes GHCi sticks ticks around this for some reason
                          (stripTicks -> (GHC.Var fid))
                          -- first type argument, must be a string literal type
                          (GHC.Type (GHC.isStrLitTy -> Just fs_locStr)))
                     -- second type argument
                     (GHC.Type codeTy))
            _)
            -- value argument
            inner
          | markerName == GHC.idName fid -> compileMarkedExprOrDefer (show fs_locStr) codeTy inner
      e@(GHC.Var fid) | markerName == GHC.idName fid -> throwError . NoContext . InvalidMarkerError . GHC.showSDocUnsafe $ GHC.ppr e
      GHC.App e a -> GHC.App <$> compileMarkedExprs e <*> compileMarkedExprs a
      GHC.Lam b e -> GHC.Lam b <$> compileMarkedExprs e
      GHC.Let bnd e -> GHC.Let <$> compileBind bnd <*> compileMarkedExprs e
      GHC.Case e b t alts -> do
            e' <- compileMarkedExprs e
            let expAlt (a, bs, rhs) = (,,) a bs <$> compileMarkedExprs rhs
            alts' <- mapM expAlt alts
            pure $ GHC.Case e' b t alts'
      GHC.Cast e c -> flip GHC.Cast c <$> compileMarkedExprs e
      GHC.Tick t e -> GHC.Tick t <$> compileMarkedExprs e
      e@(GHC.Coercion _) -> pure e
      e@(GHC.Lit _) -> pure e
      e@(GHC.Var _) -> pure e
      e@(GHC.Type _) -> pure e

-- | Behaves the same as 'compileMarkedExpr', unless a compilation error occurs ;
-- if a compilation error happens and the 'defer-errors' option is turned on,
-- the compilation error is suppressed and the original hs expression is replaced with a
-- haskell runtime-error expression.
compileMarkedExprOrDefer :: String -> GHC.Type -> GHC.CoreExpr -> PluginM PLC.DefaultUni PLC.DefaultFun GHC.CoreExpr
compileMarkedExprOrDefer locStr codeTy origE = do
    opts <- asks pcOpts
    let compileAct = compileMarkedExpr locStr codeTy origE
    if poDeferErrors opts
      -- TODO: we could perhaps move this catchError to the "runExceptT" module-level, but
      -- it leads to uglier code and difficulty of handling other pure errors
      then compileAct `catchError` emitRuntimeError codeTy
      else compileAct

-- | Given an expected Haskell type 'a', it generates Haskell code which throws a GHC runtime error "as" 'CompiledCode a'.
emitRuntimeError :: (PLC.GShow uni, PLC.Closed uni, PP.Pretty fun, PLC.Everywhere uni PLC.PrettyConst)
                 => GHC.Type -> CompileError uni fun -> PluginM uni fun GHC.CoreExpr
emitRuntimeError codeTy e = do
    opts <- asks pcOpts
    let shown = show $ PP.pretty (pruneContext (poContextLevel opts) e)
    tcName <- thNameToGhcNameOrFail ''CompiledCode
    tc <- lift . lift $ GHC.lookupTyCon tcName
    pure $ GHC.mkRuntimeErrorApp GHC.rUNTIME_ERROR_ID (GHC.mkTyConApp tc [codeTy]) shown

-- | Compile the core expression that is surrounded by a 'plc' marker,
-- and return a core expression which evaluates to the compiled plc AST as a serialized bytestring,
-- to be injected back to the Haskell program.
compileMarkedExpr :: String -> GHC.Type -> GHC.CoreExpr -> PluginM PLC.DefaultUni PLC.DefaultFun GHC.CoreExpr
compileMarkedExpr locStr codeTy origE = do
    flags <- GHC.getDynFlags
    famEnvs <- asks pcFamEnvs
    opts <- asks pcOpts
    moduleName <- asks pcModuleName
    let moduleNameStr = GHC.showSDocForUser flags GHC.alwaysQualify (GHC.ppr moduleName)
    -- We need to do this out here, since it has to run in CoreM
    nameInfo <- makePrimitiveNameInfo $ builtinNames ++ [''Bool, 'False, 'True, 'traceBool]
    modBreaks <- asks pcModuleModBreaks
    let ctx = CompileContext {
            ccOpts = CompileOptions {coProfile=poProfile opts,coCoverage=poCoverage opts,coRemoveTrace=poRemoveTrace opts},
            ccFlags = flags,
            ccFamInstEnvs = famEnvs,
            ccNameInfo = nameInfo,
            ccScopes = initialScopeStack,
            ccBlackholed = mempty,
            ccCurDef = Nothing,
            ccModBreaks = modBreaks
            }

    ((pirP,uplcP), covIdx) <- runWriterT . runQuoteT . flip runReaderT ctx $ withContextM 1 (sdToTxt $ "Compiling expr at" GHC.<+> GHC.text locStr) $ runCompiler moduleNameStr opts origE

    -- serialize the PIR, PLC, and coverageindex outputs into a bytestring.
    bsPir <- makeByteStringLiteral $ flat pirP
    bsPlc <- makeByteStringLiteral $ flat uplcP
    covIdxFlat <- makeByteStringLiteral $ flat covIdx

    builder <- lift . lift . GHC.lookupId =<< thNameToGhcNameOrFail 'mkCompiledCode

    -- inject the three bytestrings back as Haskell code.
    pure $
        GHC.Var builder
        `GHC.App` GHC.Type codeTy
        `GHC.App` bsPlc
        `GHC.App` bsPir
        `GHC.App` covIdxFlat

-- | The GHC.Core to PIR to PLC compiler pipeline. Returns both the PIR and PLC output.
-- It invokes the whole compiler chain:  Core expr -> PIR expr -> PLC expr -> UPLC expr.
runCompiler
    :: forall uni fun m . (uni ~ PLC.DefaultUni, fun ~ PLC.DefaultFun, MonadReader (CompileContext uni fun) m, MonadWriter CoverageIndex m, MonadQuote m, MonadError (CompileError uni fun) m, MonadIO m)
    => String
    -> PluginOptions
    -> GHC.CoreExpr
    -> m (PIRProgram uni fun, UPLCProgram uni fun)
runCompiler moduleName opts expr = do
    -- Plc configuration
    plcTcConfig <- PLC.getDefTypeCheckConfig PIR.noProvenance

    -- Pir configuration
    let pirTcConfig = if poDoTypecheck opts
                      -- pir's tc-config is based on plc tcconfig
                      then Just $ PIR.PirTCConfig plcTcConfig PIR.YesEscape
                      else Nothing
        pirCtx = PIR.toDefaultCompilationCtx plcTcConfig
                 & set (PIR.ccOpts . PIR.coOptimize) (poOptimize opts)
                 & set (PIR.ccOpts . PIR.coPedantic) (poPedantic opts)
                 & set (PIR.ccOpts . PIR.coVerbose) (poVerbose opts)
                 & set (PIR.ccOpts . PIR.coDebug) (poDebug opts)
                 & set (PIR.ccOpts . PIR.coMaxSimplifierIterations) (poMaxSimplifierIterations opts)
                 & set PIR.ccTypeCheckConfig pirTcConfig
                 -- Simplifier options
                 & set (PIR.ccOpts . PIR.coDoSimplifierUnwrapCancel)       (poDoSimplifierUnwrapCancel opts)
                 & set (PIR.ccOpts . PIR.coDoSimplifierBeta)               (poDoSimplifierBeta opts)
                 & set (PIR.ccOpts . PIR.coDoSimplifierInline)             (poDoSimplifierInline opts)

    -- GHC.Core -> Pir translation.
    pirT <- PIR.runDefT () $ compileExprWithDefs expr
    when (poDumpPir opts) . liftIO $ dumpFlat (PIR.Program () pirT) "initial PIR program" (moduleName ++ ".pir-initial.flat")

    -- Pir -> (Simplified) Pir pass. We can then dump/store a more legible PIR program.
    spirT <- flip runReaderT pirCtx $ PIR.compileToReadable pirT
    let spirP = PIR.Program () . void $ spirT
    when (poDumpPir opts) . liftIO $ dumpFlat spirP "simplified PIR program" (moduleName ++ ".pir-simplified.flat")

    -- (Simplified) Pir -> Plc translation.
    plcT <- flip runReaderT pirCtx $ PIR.compileReadableToPlc spirT
    let plcP = PLC.Program () (PLC.defaultVersion ()) $ void plcT
    when (poDumpPlc opts) . liftIO $ dumpFlat plcP "typed PLC program" (moduleName ++ ".plc.flat")

    -- We do this after dumping the programs so that if we fail typechecking we still get the dump.
    when (poDoTypecheck opts) . void $
        liftExcept $ PLC.typecheckPipeline plcTcConfig plcP

    uplcP <- liftExcept $ UPLC.deBruijnProgram $ UPLC.simplifyProgram $ UPLC.eraseProgram plcP
    when (poDumpUPlc opts) . liftIO $ dumpFlat uplcP "untyped PLC program" (moduleName ++ ".uplc.flat")
    pure (spirP, uplcP)

  where
      -- ugly trick to take out the concrete plc.error and in case of error, map it / rethrow it using our 'CompileError'
      liftExcept :: ExceptT (PLC.Error PLC.DefaultUni PLC.DefaultFun ()) m b -> m b
      liftExcept act = do
        plcTcError <- runExceptT act
        -- also wrap the PLC Error annotations into Original provenances, to match our expected 'CompileError'
        liftEither $ first (view (re PIR._PLCError) . fmap PIR.Original) plcTcError

      dumpFlat :: Flat t => t -> String -> String -> IO ()
      dumpFlat t desc fileName = do
        (tPath, tHandle) <- openTempFile "." fileName
        putStrLn $ "!!! dumping " ++ desc ++ " to " ++ show tPath
        BS.hPut tHandle $ flat t

-- | Get the 'GHC.Name' corresponding to the given 'TH.Name'
--
-- We cannot use 'GHC.thNameToGhcName' here, because the Template Haskell
-- names refer to the packages used for building the plugin.
--
-- When we're cross compiling, these are different from the ones we actually
-- need.
--
-- Instead we drop the package key and version and use GHC's 'Finder' to
-- locate the names.
thNameToGhcName :: TH.Name -> GHC.CoreM (Maybe GHC.Name)
thNameToGhcName name =
  case name of
    (TH.Name (TH.OccName occ) flav) -> do
      case flav of
        TH.NameG nameSpace (TH.PkgName pkg) (TH.ModName mod_name) -> do
          let real_pkg = dropVersion pkg
              ghc_ns = case nameSpace of
                         TH.VarName   -> GHCO.varName
                         TH.DataName  -> GHCO.dataName
                         TH.TcClsName -> GHCO.tcClsName
          loadName_maybe real_pkg (GHC.mkModuleName mod_name) ghc_ns occ >>= \case
            Just ghcName -> do
              thing <- GHC.lookupThing ghcName
              pure (Just ghcName)
            _            -> pure Nothing -- flav was ok, but name was not found
        _ -> pure Nothing

-- | Get the 'GHC.Name' corresponding to the given 'TH.Name', or throw an error if we can't get it.
thNameToGhcNameOrFail :: TH.Name -> PluginM uni fun GHC.Name
thNameToGhcNameOrFail name = do
    maybeName <- lift . lift $ thNameToGhcName name
    case maybeName of
        Just n  -> pure n
        Nothing -> throwError . NoContext $ CoreNameLookupError name

-- | Create a GHC Core expression that will evaluate to the given ByteString at runtime.
makeByteStringLiteral :: BS.ByteString -> PluginM uni fun GHC.CoreExpr
makeByteStringLiteral bs = do
    flags <- GHC.getDynFlags

    {-
    This entire section will crash horribly in a number of circumstances. Such is life.
    - If any of the names we need can't be found as GHC Names
    - If we then can't look up those GHC Names to get their IDs/types
    - If we make any mistakes creating the Core expression
    -}

    -- Get the names of functions/types that we need for our expression
    upio <- lift . lift . GHC.lookupId =<< thNameToGhcNameOrFail 'unsafePerformIO
    bsTc <- lift . lift . GHC.lookupTyCon =<< thNameToGhcNameOrFail ''BS.ByteString
    upal <- lift . lift . GHC.lookupId =<< thNameToGhcNameOrFail 'BSUnsafe.unsafePackAddressLen

    -- We construct the following expression:
    -- unsafePerformIO $ unsafePackAddressLen <length as int literal> <data as string literal address>
    -- This technique gratefully borrowed from the file-embed package

    -- The flags here are so GHC can check whether the int is in range for the current platform.
    let lenLit = GHC.mkIntExpr flags $ fromIntegral $ BS.length bs
    -- This will have type Addr#, which is right for unsafePackAddressLen
    let bsLit = GHC.Lit (GHC.LitString bs)
    let upaled = GHC.mkCoreApps (GHC.Var upal) [lenLit, bsLit]
    let upioed = GHC.mkCoreApps (GHC.Var upio) [GHC.Type (GHC.mkTyConTy bsTc), upaled]

    pure upioed

dropVersion :: String -> String
dropVersion pkg = intercalate "-" (mkParts pkg)
  where mkParts xs =
          let (a,b) = break (=='-') xs
          in if all (\x -> isDigit x || x == '.') a
             then []
             else a : mkParts (drop 1 b)

-- | Strips all enclosing 'GHC.Tick's off an expression.
stripTicks :: GHC.CoreExpr -> GHC.CoreExpr
stripTicks = \case
    GHC.Tick _ e -> stripTicks e
    e            -> e

-- | Helper to avoid doing too much construction of Core ourselves
mkCompiledCode :: forall a . BS.ByteString -> BS.ByteString -> BS.ByteString -> CompiledCode a
mkCompiledCode plcBS pirBS ci = SerializedCode plcBS (Just pirBS) (fold . unflat $ ci)

-- | Make a 'NameInfo' mapping the given set of TH names to their
-- 'GHC.TyThing's for later reference.
makePrimitiveNameInfo :: [TH.Name] -> PluginM uni fun NameInfo
makePrimitiveNameInfo names = do
    infos <- for names $ \name -> do
        ghcName <- thNameToGhcNameOrFail name
        thing <- lift . lift $ GHC.lookupThing ghcName
        pure (name, thing)
    pure $ Map.fromList infos
