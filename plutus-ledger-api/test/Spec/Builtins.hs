{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations=0 #-}
module Spec.Builtins where

import Data.Proxy
import Plutus.V1.Ledger.Api
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code
import PlutusTx.Plugin

import Codec.Serialise
import Data.ByteString.Lazy as BSL
import Data.ByteString.Short
import Data.Foldable (fold, for_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.ApiCommon
import Test.Tasty
import Test.Tasty.HUnit

serialiseDataEx :: CompiledCode Builtins.BuiltinByteString
serialiseDataEx = plc (Proxy @"serialiseDataEx") (Builtins.serialiseData (Builtins.mkI 1))

serialiseDataExScript :: SerializedScript
serialiseDataExScript = toShort . toStrict . serialise $ fromCompiledCode serialiseDataEx

tests :: TestTree
tests =
  testGroup
    "builtins"
    [ testCase "all builtins are available some time" $
            let allPvBuiltins = fold $ Map.elems $ builtinsIntroducedIn
                allBuiltins = [(toEnum 0)..]
            in for_ allBuiltins $ \f -> assertBool (show f) (f `Set.member` allPvBuiltins)
    , testCase "builtins aren't available before v5" $ assertBool "empty" (Set.null $ builtinsAvailableIn (ProtocolVersion 4 0))
    , testCase "serializeData is only available in v6" $ do
         assertBool "in v5 " $ not $ isScriptWellFormed (ProtocolVersion 5 0) serialiseDataExScript
         assertBool "not in v6" $ isScriptWellFormed (ProtocolVersion 6 0) serialiseDataExScript
    ]
