{- hlint ignore "Use lambda-case" -}
{- hlint ignore "Use tuple-section" -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Data as Data
import qualified Data.Either as Either
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Tuple as Tuple
import qualified Data.Version as Version
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Core.Unfold as Unfold
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Driver.Backend as Backend
import qualified GHC.Driver.Session as Session
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type as X
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Errors.Types as PsErr
import qualified GHC.Parser.Header as Header
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Platform as Platform
import qualified GHC.Settings as Settings
import qualified GHC.Stack as Stack
import qualified GHC.Types.Name.Occurrence as OccName
import qualified GHC.Types.Name.Reader as RdrName
import qualified GHC.Types.SafeHaskell as SafeHaskell
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Error as ErrUtil
import qualified Language.Haskell.Syntax as HS
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_scrod as Package
import qualified Scrod.Extra.Data as Data
import qualified Scrod.Extra.Haddock as Haddock
import qualified Scrod.Type.Direction as Direction
import qualified Scrod.Type.Item as Item
import qualified Scrod.Type.LHsModule as LHsModule
import qualified Scrod.Type.Messages as Messages
import qualified Scrod.Type.Name as Name
import qualified Scrod.Type.Position as Position
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified Test.Hspec as Hspec
import qualified Text.Printf as Printf

-- Test Suite -----------------------------------------------------------------

testSuite :: IO ()
testSuite = Hspec.hspec . Hspec.parallel . Hspec.describe "Scrod" $ do
  Hspec.describe "getItems" $ do
    let f :: (Stack.HasCallStack) => String -> [(Item.Item, [String])]
        f string =
          let lHsModule = either (error . show) id $ parseLHsModule "<interactive>" string
              items = getItems lHsModule
              lHsDocStrings = getLHsDocStrings lHsModule
           in mergeItems $ associateDocStrings items lHsDocStrings

        p l c = Position.Position {Position.line = l, Position.column = c}

    Hspec.it "empty" $ do
      f "" `Hspec.shouldBe` []

    Hspec.it "type family" $ do
      f "type family A" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "A", Item.position = p 1 13}, [])]

    Hspec.it "data family" $ do
      f "data family B" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "B", Item.position = p 1 13}, [])]

    Hspec.it "type synonym" $ do
      f "type C = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "C", Item.position = p 1 6}, [])]

    Hspec.it "data" $ do
      f "data D" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "D", Item.position = p 1 6}, [])]

    Hspec.it "data constructor" $ do
      f "data X = E" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "X", Item.position = p 1 6}, []), (Item.Item {Item.name = Name.Other "E", Item.position = p 1 10}, [])]

    Hspec.it "data constructor gadt" $ do
      f "data X where E :: X" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "X", Item.position = p 1 6}, []), (Item.Item {Item.name = Name.Other "E", Item.position = p 1 14}, [])]

    Hspec.it "record field" $ do
      f "newtype T = C { f :: () }" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "T", Item.position = p 1 9}, []), (Item.Item {Item.name = Name.Other "C", Item.position = p 1 13}, []), (Item.Item {Item.name = Name.Other "f", Item.position = p 1 17}, [])]

    Hspec.it "record fields with one signature" $ do
      f "newtype T = C { f, g :: () }" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "T", Item.position = p 1 9}, []), (Item.Item {Item.name = Name.Other "C", Item.position = p 1 13}, []), (Item.Item {Item.name = Name.Other "f", Item.position = p 1 17}, []), (Item.Item {Item.name = Name.Other "g", Item.position = p 1 20}, [])]

    Hspec.it "record fields with separate signatures" $ do
      f "newtype T = C { f :: (), g :: () }" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "T", Item.position = p 1 9}, []), (Item.Item {Item.name = Name.Other "C", Item.position = p 1 13}, []), (Item.Item {Item.name = Name.Other "f", Item.position = p 1 17}, []), (Item.Item {Item.name = Name.Other "g", Item.position = p 1 26}, [])]

    Hspec.it "record field gadt" $ do
      f "newtype T where C :: { f :: () } -> T" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "T", Item.position = p 1 9}, []), (Item.Item {Item.name = Name.Other "C", Item.position = p 1 17}, []), (Item.Item {Item.name = Name.Other "f", Item.position = p 1 24}, [])]

    Hspec.it "class" $ do
      f "class E" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "E", Item.position = p 1 7}, [])]

    Hspec.it "class instance" $ do
      f "instance F" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "F", Item.position = p 1 10}, [])]

    Hspec.it "class instance with arguments" $ do
      f "instance G a" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "G", Item.position = p 1 10}, [])]

    Hspec.it "class instance with parentheses" $ do
      f "instance (H)" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "H", Item.position = p 1 11}, [])]

    Hspec.it "class instance with context" $ do
      f "instance () => I" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "I", Item.position = p 1 16}, [])]

    Hspec.it "data instance" $ do
      f "data instance G" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "G", Item.position = p 1 15}, [])]

    Hspec.it "newtype instance" $ do
      f "newtype instance H = X" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "H", Item.position = p 1 18}, [])]

    Hspec.it "type instance" $ do
      f "type instance I = X" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "I", Item.position = p 1 15}, [])]

    Hspec.it "deriving instance" $ do
      f "deriving instance J" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "J", Item.position = p 1 19}, [])]

    Hspec.it "function" $ do
      f "h x = x" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "h", Item.position = p 1 1}, [])]

    Hspec.it "infix function" $ do
      f "_ `f` _ = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "f", Item.position = p 1 3}, [])]

    Hspec.it "infix operator" $ do
      f "_ & _ = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "&", Item.position = p 1 3}, [])]

    Hspec.it "prefix operator" $ do
      -- TODO: Why is this at column 1 rather than 2?
      f "(&) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "&", Item.position = p 1 1}, [])]

    Hspec.it "variable" $ do
      f "i = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "i", Item.position = p 1 1}, [])]

    Hspec.it "variable on another line" $ do
      f "\ni = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "i", Item.position = p 2 1}, [])]

    Hspec.it "variable indented" $ do
      f " i = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "i", Item.position = p 1 2}, [])]

    Hspec.it "strict variable" $ do
      f "!j = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "j", Item.position = p 1 2}, [])]

    Hspec.it "wildcard pattern" $ do
      f "_ = ()" `Hspec.shouldBe` []

    Hspec.it "lazy pattern" $ do
      f "~k = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "k", Item.position = p 1 2}, [])]

    Hspec.it "as pattern" $ do
      f "l@m = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "l", Item.position = p 1 1}, []), (Item.Item {Item.name = Name.Other "m", Item.position = p 1 3}, [])]

    Hspec.it "patenthesized pattern" $ do
      f "(n) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "n", Item.position = p 1 2}, [])]

    Hspec.it "bang pattern" $ do
      -- Note that this is different than the "strict variable" test case!
      f "(!o) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "o", Item.position = p 1 3}, [])]

    Hspec.it "list pattern" $ do
      f "[p] = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "p", Item.position = p 1 2}, [])]

    Hspec.it "tuple pattern" $ do
      f "(q, r) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "q", Item.position = p 1 2}, []), (Item.Item {Item.name = Name.Other "r", Item.position = p 1 5}, [])]

    Hspec.it "anonymous sum pattern" $ do
      f "{-# language UnboxedSums #-} (# s | #) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "s", Item.position = p 1 33}, [])]

    Hspec.it "prefix constructor pattern" $ do
      f "Just t = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "t", Item.position = p 1 6}, [])]

    Hspec.it "record constructor pattern" $ do
      f "X { u = v } = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "v", Item.position = p 1 9}, [])]

    Hspec.it "punned record pattern" $ do
      f "X { w } = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "w", Item.position = p 1 5}, [])]

    Hspec.it "wild card record pattern" $ do
      f "X { .. } = ()" `Hspec.shouldBe` []

    Hspec.it "infix constructor pattern" $ do
      f "(x : _) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "x", Item.position = p 1 2}, [])]

    Hspec.it "view pattern" $ do
      f "(f -> y) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "y", Item.position = p 1 7}, [])]

    Hspec.it "splice pattern" $ do
      f "{-# language TemplateHaskellQuotes #-} $( x ) = ()" `Hspec.shouldBe` []

    Hspec.it "literal pattern" $ do
      f "'x' = ()" `Hspec.shouldBe` []

    Hspec.it "natural pattern" $ do
      f "0 = ()" `Hspec.shouldBe` []

    Hspec.it "n+k pattern" $ do
      f "{-# language NPlusKPatterns #-} (z + 1) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "z", Item.position = p 1 34}, [])]

    Hspec.it "signature pattern" $ do
      f "(a :: ()) = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "a", Item.position = p 1 2}, [])]

    Hspec.it "bidirectional pattern synonym" $ do
      f "{-# language PatternSynonyms #-} pattern B = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "B", Item.position = p 1 42}, [])]

    Hspec.it "unidirectional pattern synonym" $ do
      f "{-# language PatternSynonyms #-} pattern C <- ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "C", Item.position = p 1 42}, [])]

    Hspec.it "explicitly bidirectional pattern synonym" $ do
      f "{-# language PatternSynonyms #-} pattern D <- () where D = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "D", Item.position = p 1 42}, [])]

    Hspec.it "type signature" $ do
      f "e :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "e", Item.position = p 1 1}, [])]

    Hspec.it "pattern type signature" $ do
      f "{-# language PatternSynonyms #-} pattern F :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "F", Item.position = p 1 42}, [])]

    Hspec.it "method signature" $ do
      f "class X where g :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "X", Item.position = p 1 7}, []), (Item.Item {Item.name = Name.Other "g", Item.position = p 1 15}, [])]

    Hspec.it "default method signature" $ do
      f "class X where default h :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "X", Item.position = p 1 7}, []), (Item.Item {Item.name = Name.Other "h", Item.position = p 1 23}, [])]

    Hspec.it "fixity declaration" $ do
      f "infix 5 %" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "%", Item.position = p 1 9}, [])]

    Hspec.it "inline pragma" $ do
      f "{-# inline i #-}" `Hspec.shouldBe` []

    Hspec.it "inline pragma with phase control" $ do
      f "{-# inline [1] j #-}" `Hspec.shouldBe` []

    Hspec.it "inline pragma with inverted phase control" $ do
      f "{-# inline [~2] k #-}" `Hspec.shouldBe` []

    Hspec.it "noinline pragma" $ do
      f "{-# noinline l #-}" `Hspec.shouldBe` []

    Hspec.it "specialize pragma" $ do
      f "{-# specialize j :: () #-}" `Hspec.shouldBe` []

    Hspec.it "specialize instance pragma" $ do
      f "{-# specialize instance K #-}" `Hspec.shouldBe` []

    Hspec.it "minimal pragma" $ do
      f "{-# minimal l #-}" `Hspec.shouldBe` []

    Hspec.it "set cost center pragma" $ do
      f "{-# scc m #-}" `Hspec.shouldBe` []

    Hspec.it "complete pragma" $ do
      f "{-# complete N #-}" `Hspec.shouldBe` []

    Hspec.it "standalone kind signature" $ do
      f "type O :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "O", Item.position = p 1 6}, [])]

    Hspec.it "default declaration" $ do
      f "default ()" `Hspec.shouldBe` []

    Hspec.it "foreign import" $ do
      f "{-# language ForeignFunctionInterface #-} foreign import ccall \"\" p :: ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "p", Item.position = p 1 67}, [])]

    Hspec.it "warning pragma" $ do
      f "{-# warning x \"\" #-}" `Hspec.shouldBe` []

    Hspec.it "value annotation" $ do
      f "{-# ann x () #-}" `Hspec.shouldBe` []

    Hspec.it "type annotation" $ do
      f "{-# ann type X () #-}" `Hspec.shouldBe` []

    Hspec.it "module annotation" $ do
      f "{-# ann module () #-}" `Hspec.shouldBe` []

    Hspec.it "rules pragma" $ do
      f "{-# rules \"q\" x = () #-}" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "q", Item.position = p 1 11}, [])]

    Hspec.it "splice declaration" $ do
      f "{-# language TemplateHaskellQuotes #-} $( x )" `Hspec.shouldBe` []

    Hspec.it "documentation" $ do
      f "-- | x" `Hspec.shouldBe` []

    Hspec.it "role annotation" $ do
      f "type role R nominal" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "R", Item.position = p 1 11}, [])]

    Hspec.it "documentation before item" $ do
      f "-- | x\ny = ()" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "y", Item.position = p 2 1}, [" x"])]

    Hspec.it "documentation after item" $ do
      f "x = ()\n-- ^ y" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "x", Item.position = p 1 1}, [" y"])]

    Hspec.it "documentation around item" $ do
      f "-- | x\ny = ()\n-- ^ z" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "y", Item.position = p 2 1}, [" x", " z"])]

    Hspec.it "" $ do
      f "a = ()\n-- ^ 1\nb = ()\n-- ^ 2" `Hspec.shouldBe` [(Item.Item {Item.name = Name.Other "a", Item.position = p 1 1}, [" 1"]), (Item.Item {Item.name = Name.Other "b", Item.position = p 3 1}, [" 2"])]

  Hspec.describe "associateDocStrings" $ do
    let mkItem n l c = Item.Item {Item.name = Name.Other n, Item.position = Position.Position {Position.line = l, Position.column = c}}
        mkSrcLoc = SrcLoc.mkSrcLoc $ FastString.mkFastString ""
        mkSrcSpan (l1, c1) (l2, c2) = SrcLoc.mkSrcSpan (mkSrcLoc l1 c1) (mkSrcLoc l2 c2)
        mkDocString (l1, c1) (l2, c2) d =
          GHC.Hs.MultiLineDocString
            ( case d of
                Direction.Next -> GHC.Hs.HsDocStringNext
                Direction.Previous -> GHC.Hs.HsDocStringPrevious
            )
            . pure
            . SrcLoc.L (mkSrcSpan (l1, c1) (l2, c2))
            . GHC.Hs.mkHsDocStringChunk

    Hspec.it "empty" $ do
      associateDocStrings [] [] `Hspec.shouldBe` []

    Hspec.it "item only" $ do
      let item = mkItem "a" 1 1
      associateDocStrings [item] [] `Hspec.shouldBe` [(item, [])]

    Hspec.it "doc only" $ do
      let lHsDocString =
            SrcLoc.L (mkSrcSpan (1, 1) (1, 1)) $
              mkDocString (1, 1) (1, 1) Direction.Next "a"
      associateDocStrings [] [lHsDocString] `Hspec.shouldBe` []

    Hspec.it "doc before item" $ do
      let item = mkItem "a" 2 1
          lHsDocString =
            SrcLoc.L (mkSrcSpan (1, 1) (1, 1)) $
              mkDocString (1, 1) (1, 1) Direction.Next "b"
      associateDocStrings [item] [lHsDocString] `Hspec.shouldBe` [(item, ["b"])]

    Hspec.it "doc after item" $ do
      let item = mkItem "a" 1 1
          lHsDocString =
            SrcLoc.L (mkSrcSpan (2, 1) (2, 1)) $
              mkDocString (2, 1) (2, 1) Direction.Previous "b"
      associateDocStrings [item] [lHsDocString] `Hspec.shouldBe` [(item, ["b"])]

    Hspec.it "docs around item" $ do
      let item = mkItem "b" 2 1
          d1 =
            SrcLoc.L (mkSrcSpan (1, 1) (1, 1)) $
              mkDocString (1, 1) (1, 1) Direction.Next "a"
          d2 =
            SrcLoc.L (mkSrcSpan (3, 1) (3, 1)) $
              mkDocString (3, 1) (3, 1) Direction.Previous "c"
      associateDocStrings [item] [d1, d2] `Hspec.shouldBe` [(item, ["a", "c"])]

  Hspec.describe "discoverExtensions" $ do
    Hspec.it "discovers an enabled extension" $ do
      discoverExtensions "{-# language CPP #-}" `Hspec.shouldBe` (Nothing, [Session.On X.Cpp])

    Hspec.it "discovers a disabled extension" $ do
      discoverExtensions "{-# language NoCPP #-}" `Hspec.shouldBe` (Nothing, [Session.Off X.Cpp])

    Hspec.it "discovers an extension from a ghc option" $ do
      discoverExtensions "{-# options_ghc -XCPP #-}" `Hspec.shouldBe` (Nothing, [Session.On X.Cpp])

    Hspec.it "discovers two extensions in one pragma" $ do
      discoverExtensions "{-# language CPP, DeriveGeneric #-}" `Hspec.shouldBe` (Nothing, [Session.On X.DeriveGeneric, Session.On X.Cpp])

    Hspec.it "discovers two extensions in separate pragmas" $ do
      discoverExtensions "{-# language CPP #-} {-# language DeriveGeneric #-}" `Hspec.shouldBe` (Nothing, [Session.On X.DeriveGeneric, Session.On X.Cpp])

    Hspec.it "discovers the same extension twice" $ do
      discoverExtensions "{-# language CPP #-} {-# language CPP #-}" `Hspec.shouldBe` (Nothing, [Session.On X.Cpp, Session.On X.Cpp])

    Hspec.it "discovers the same extension on then off" $ do
      discoverExtensions "{-# language CPP #-} {-# language NoCPP #-}" `Hspec.shouldBe` (Nothing, [Session.Off X.Cpp, Session.On X.Cpp])

    Hspec.it "discovers a language edition" $ do
      discoverExtensions "{-# language GHC2021 #-}" `Hspec.shouldBe` (Just Session.GHC2021, [])

mergeItems :: (Monoid a) => [(Item.Item, a)] -> [(Item.Item, a)]
mergeItems items = case items of
  [] -> items
  (i, a) : is ->
    let (ts, fs) = List.partition ((==) (Item.name i) . Item.name . fst) is
     in (i, a <> foldMap snd ts) : mergeItems fs

discoverExtensions :: String -> (Maybe Session.Language, [Session.OnOff X.Extension])
discoverExtensions = Unsafe.unsafePerformIO . discoverExtensionsIO

discoverExtensionsIO :: String -> IO (Maybe Session.Language, [Session.OnOff X.Extension])
discoverExtensionsIO string = do
  let dynFlags =
        Session.DynFlags
          { Session.backend = Backend.noBackend,
            Session.dmdUnboxWidth = 0,
            Session.dynamicNow = False,
            Session.enableTimeStats = False,
            Session.extensions = mempty,
            Session.extensionFlags = mempty,
            Session.fileSettings =
              Session.FileSettings
                { Session.fileSettings_ghciUsagePath = error "fileSettings_ghciUsagePath",
                  Session.fileSettings_ghcUsagePath = error "fileSettings_ghcUsagePath",
                  Session.fileSettings_globalPackageDatabase = error "fileSettings_globalPackageDatabase",
                  Session.fileSettings_toolDir = error "fileSettings_toolDir",
                  Session.fileSettings_topDir = error "fileSettings_topDir"
                },
            Session.generalFlags = mempty,
            Session.ghcHeapSize = Nothing,
            Session.ghcLink = Session.NoLink,
            Session.ghcNameVersion =
              Session.GhcNameVersion
                { Session.ghcNameVersion_programName = error "ghcNameVersion_programName",
                  Session.ghcNameVersion_projectVersion = error "ghcNameVersion_projectVersion"
                },
            Session.language = Nothing,
            Session.platformMisc =
              Session.PlatformMisc
                { Session.platformMisc_ghcWithInterpreter = error "platformMisc_ghcWithInterpreter",
                  Session.platformMisc_libFFI = error "platformMisc_libFFI",
                  Session.platformMisc_llvmTarget = error "platformMisc_llvmTarget",
                  Session.platformMisc_targetPlatformString = error "platformMisc_targetPlatformString"
                },
            Session.safeHaskell = SafeHaskell.Sf_Ignore,
            Session.safeInfer = False,
            Session.targetPlatform = Platform.genericPlatform,
            Session.targetWays_ = mempty,
            Session.toolSettings =
              Settings.ToolSettings
                { Settings.toolSettings_arSupportsDashL = error "toolSettings_arSupportsDashL",
                  Settings.toolSettings_ccSupportsNoPie = error "toolSettings_ccSupportsNoPie",
                  Settings.toolSettings_extraGccViaCFlags = error "toolSettings_extraGccViaCFlags",
                  Settings.toolSettings_ldIsGnuLd = error "toolSettings_ldIsGnuLd",
                  Settings.toolSettings_ldSupportsCompactUnwind = error "toolSettings_ldSupportsCompactUnwind",
                  Settings.toolSettings_ldSupportsFilelist = error "toolSettings_ldSupportsFilelist",
                  Settings.toolSettings_ldSupportsSingleModule = error "toolSettings_ldSupportsSingleModule",
                  Settings.toolSettings_mergeObjsSupportsResponseFiles = error "toolSettings_mergeObjsSupportsResponseFiles",
                  Settings.toolSettings_opt_a = error "toolSettings_opt_a",
                  Settings.toolSettings_opt_c = error "toolSettings_opt_c",
                  Settings.toolSettings_opt_cxx = error "toolSettings_opt_cxx",
                  Settings.toolSettings_opt_F = error "toolSettings_opt_F",
                  Settings.toolSettings_opt_i = error "toolSettings_opt_i",
                  Settings.toolSettings_opt_l = error "toolSettings_opt_l",
                  Settings.toolSettings_opt_L = error "toolSettings_opt_L",
                  Settings.toolSettings_opt_las = error "toolSettings_opt_las",
                  Settings.toolSettings_opt_lc = error "toolSettings_opt_lc",
                  Settings.toolSettings_opt_lm = error "toolSettings_opt_lm",
                  Settings.toolSettings_opt_lo = error "toolSettings_opt_lo",
                  Settings.toolSettings_opt_P = error "toolSettings_opt_P",
                  Settings.toolSettings_opt_P_fingerprint = error "toolSettings_opt_P_fingerprint",
                  Settings.toolSettings_opt_windres = error "toolSettings_opt_windres",
                  Settings.toolSettings_pgm_a = error "toolSettings_pgm_a",
                  Settings.toolSettings_pgm_ar = error "toolSettings_pgm_ar",
                  Settings.toolSettings_pgm_c = error "toolSettings_pgm_c",
                  Settings.toolSettings_pgm_cpp = error "toolSettings_pgm_cpp",
                  Settings.toolSettings_pgm_cxx = error "toolSettings_pgm_cxx",
                  Settings.toolSettings_pgm_F = error "toolSettings_pgm_F",
                  Settings.toolSettings_pgm_i = error "toolSettings_pgm_i",
                  Settings.toolSettings_pgm_install_name_tool = error "toolSettings_pgm_install_name_tool",
                  Settings.toolSettings_pgm_l = error "toolSettings_pgm_l",
                  Settings.toolSettings_pgm_L = error "toolSettings_pgm_L",
                  Settings.toolSettings_pgm_las = error "toolSettings_pgm_las",
                  Settings.toolSettings_pgm_lc = error "toolSettings_pgm_lc",
                  Settings.toolSettings_pgm_lm = error "toolSettings_pgm_lm",
                  Settings.toolSettings_pgm_lo = error "toolSettings_pgm_lo",
                  Settings.toolSettings_pgm_otool = error "toolSettings_pgm_otool",
                  Settings.toolSettings_pgm_P = error "toolSettings_pgm_P",
                  Settings.toolSettings_pgm_ranlib = error "toolSettings_pgm_ranlib",
                  Settings.toolSettings_pgm_windres = error "toolSettings_pgm_windres",
                  Settings.toolSettings_useInplaceMinGW = error "toolSettings_useInplaceMinGW"
                },
            Session.unfoldingOpts = Unfold.defaultUnfoldingOpts,
            Session.avx = error "avx",
            Session.avx2 = error "avx2",
            Session.avx512cd = error "avx512cd",
            Session.avx512er = error "avx512er",
            Session.avx512f = error "avx512f",
            Session.avx512pf = error "avx512pf",
            Session.binBlobThreshold = error "binBlobThreshold",
            Session.bmiVersion = error "bmiVersion",
            Session.callerCcFilters = error "callerCcFilters",
            Session.canUseColor = error "canUseColor",
            Session.canUseErrorLinks = error "canUseErrorLinks",
            Session.cfgWeights = error "cfgWeights",
            Session.cmdlineFrameworks = error "cmdlineFrameworks",
            Session.cmmProcAlignment = error "cmmProcAlignment",
            Session.colScheme = error "colScheme",
            Session.customWarningCategories = error "customWarningCategories",
            Session.debugLevel = error "debugLevel",
            Session.depExcludeMods = error "depExcludeMods",
            Session.depIncludeCppDeps = error "depIncludeCppDeps",
            Session.depIncludePkgDeps = error "depIncludePkgDeps",
            Session.depMakefile = error "depMakefile",
            Session.depSuffixes = error "depSuffixes",
            Session.deriveViaOnLoc = error "deriveViaOnLoc",
            Session.dumpDir = error "dumpDir",
            Session.dumpFlags = error "dumpFlags",
            Session.dumpPrefix = error "dumpPrefix",
            Session.dumpPrefixForce = error "dumpPrefixForce",
            Session.dylibInstallName = error "dylibInstallName",
            Session.dynHiSuf_ = error "dynHiSuf_",
            Session.dynLibLoader = error "dynLibLoader",
            Session.dynObjectSuf_ = error "dynObjectSuf_",
            Session.dynOutputFile_ = error "dynOutputFile_",
            Session.dynOutputHi = error "dynOutputHi",
            Session.externalPluginSpecs = error "externalPluginSpecs",
            Session.fatalCustomWarningCategories = error "fatalCustomWarningCategories",
            Session.fatalWarningFlags = error "fatalWarningFlags",
            Session.floatLamArgs = error "floatLamArgs",
            Session.flushOut = error "flushOut",
            Session.fma = error "fma",
            Session.frameworkPaths = error "frameworkPaths",
            Session.frontendPluginOpts = error "frontendPluginOpts",
            Session.ghciHistSize = error "ghciHistSize",
            Session.ghciScripts = error "ghciScripts",
            Session.ghcMode = error "ghcMode",
            Session.ghcVersionFile = error "ghcVersionFile",
            Session.givensFuel = error "givensFuel",
            Session.haddockOptions = error "haddockOptions",
            Session.hcSuf = error "hcSuf",
            Session.hiddenModules = error "hiddenModules",
            Session.hiDir = error "hiDir",
            Session.hieDir = error "hieDir",
            Session.hieSuf = error "hieSuf",
            Session.historySize = error "historySize",
            Session.hiSuf_ = error "hiSuf_",
            Session.homeUnitId_ = error "homeUnitId_",
            Session.homeUnitInstanceOf_ = error "homeUnitInstanceOf_",
            Session.homeUnitInstantiations_ = error "homeUnitInstantiations_",
            Session.hpcDir = error "hpcDir",
            Session.ignorePackageFlags = error "ignorePackageFlags",
            Session.importPaths = error "importPaths",
            Session.includePaths = error "includePaths",
            Session.incoherentOnLoc = error "incoherentOnLoc",
            Session.initialUnique = error "initialUnique",
            Session.interactivePrint = error "interactivePrint",
            Session.ldInputs = error "ldInputs",
            Session.liberateCaseThreshold = error "liberateCaseThreshold",
            Session.libraryPaths = error "libraryPaths",
            Session.liftLamsKnown = error "liftLamsKnown",
            Session.liftLamsNonRecArgs = error "liftLamsNonRecArgs",
            Session.liftLamsRecArgs = error "liftLamsRecArgs",
            Session.llvmOptLevel = error "llvmOptLevel",
            Session.mainFunIs = error "mainFunIs",
            Session.mainModuleNameIs = error "mainModuleNameIs",
            Session.maxErrors = error "maxErrors",
            Session.maxInlineAllocSize = error "maxInlineAllocSize",
            Session.maxInlineMemcpyInsns = error "maxInlineMemcpyInsns",
            Session.maxInlineMemsetInsns = error "maxInlineMemsetInsns",
            Session.maxPmCheckModels = error "maxPmCheckModels",
            Session.maxRefHoleFits = error "maxRefHoleFits",
            Session.maxRelevantBinds = error "maxRelevantBinds",
            Session.maxSimplIterations = error "maxSimplIterations",
            Session.maxUncoveredPatterns = error "maxUncoveredPatterns",
            Session.maxValidHoleFits = error "maxValidHoleFits",
            Session.maxWorkerArgs = error "maxWorkerArgs",
            Session.newDerivOnLoc = error "newDerivOnLoc",
            Session.objectDir = error "objectDir",
            Session.objectSuf_ = error "objectSuf_",
            Session.outputFile_ = error "outputFile_",
            Session.outputHi = error "outputHi",
            Session.overlapInstLoc = error "overlapInstLoc",
            Session.packageDBFlags = error "packageDBFlags",
            Session.packageEnv = error "packageEnv",
            Session.packageFlags = error "packageFlags",
            Session.parMakeCount = error "parMakeCount",
            Session.pkgTrustOnLoc = error "pkgTrustOnLoc",
            Session.pluginModNameOpts = error "pluginModNameOpts",
            Session.pluginModNames = error "pluginModNames",
            Session.pluginPackageFlags = error "pluginPackageFlags",
            Session.pprCols = error "pprCols",
            Session.pprUserLength = error "pprUserLength",
            Session.profAuto = error "profAuto",
            Session.qcsFuel = error "qcsFuel",
            Session.rawSettings = error "rawSettings",
            Session.reductionDepth = error "reductionDepth",
            Session.reexportedModules = error "reexportedModules",
            Session.refLevelHoleFits = error "refLevelHoleFits",
            Session.reverseErrors = error "reverseErrors",
            Session.rtsOpts = error "rtsOpts",
            Session.rtsOptsEnabled = error "rtsOptsEnabled",
            Session.rtsOptsSuggestions = error "rtsOptsSuggestions",
            Session.ruleCheck = error "ruleCheck",
            Session.safeInferred = error "safeInferred",
            Session.simplPhases = error "simplPhases",
            Session.simplTickFactor = error "simplTickFactor",
            Session.solverIterations = error "solverIterations",
            Session.specConstrCount = error "specConstrCount",
            Session.specConstrRecursive = error "specConstrRecursive",
            Session.specConstrThreshold = error "specConstrThreshold",
            Session.splitInfo = error "splitInfo",
            Session.sseVersion = error "sseVersion",
            Session.strictnessBefore = error "strictnessBefore",
            Session.stubDir = error "stubDir",
            Session.thisPackageName = error "thisPackageName",
            Session.thOnLoc = error "thOnLoc",
            Session.tmpDir = error "tmpDir",
            Session.trustFlags = error "trustFlags",
            Session.trustworthyOnLoc = error "trustworthyOnLoc",
            Session.uniqueIncrement = error "uniqueIncrement",
            Session.useColor = error "useColor",
            Session.useErrorLinks = error "useErrorLinks",
            Session.useUnicode = error "useUnicode",
            Session.verbosity = error "verbosity",
            Session.wantedsFuel = error "wantedsFuel",
            Session.warningFlags = error "warningFlags",
            Session.warnSafeOnLoc = error "warnSafeOnLoc",
            Session.warnUnsafeOnLoc = error "warnUnsafeOnLoc",
            Session.workingDirectory = error "workingDirectory"
          }
      stringBuffer = StringBuffer.stringToStringBuffer string
      supported =
        Session.supportedLanguagesAndExtensions
          Platform.ArchOS
            { Platform.archOS_arch = Platform.ArchUnknown,
              Platform.archOS_OS = Platform.OSUnknown
            }
      parserOpts =
        Lexer.mkParserOpts
          mempty
          ErrUtil.emptyDiagOpts
          supported
          False
          False
          False
          False
      (_, locatedStrings) = Header.getOptions parserOpts stringBuffer "<interactive>"
  (newDynFlags, _, _) <- Session.parseDynamicFilePragma dynFlags locatedStrings
  pure (Session.language newDynFlags, Session.extensions newDynFlags)

-- Executable -----------------------------------------------------------------

executable :: IO ()
executable = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  Warp.runSettings settings $ \request respond ->
    case (Http.parseMethod $ Wai.requestMethod request, Text.unpack <$> Wai.pathInfo request) of
      (Right Http.GET, []) -> application request respond
      (Right Http.POST, []) -> application request respond
      _ -> respond $ statusResponse Http.status404 defaultHeaders

defaultInput :: Text.Text
defaultInput =
  Text.pack $
    unlines
      [ "-- | A [__Bool__ean data type](https://en.wikipedia.org/wiki/Boolean_data_type), introduced by George Boole in /The Mathematical Analysis of Logic/.",
        "data Bool",
        "  = False -- ^ Also known as 0.",
        "  | True -- ^ Also known as 1.",
        "",
        "-- | Negates the input. Sometimes denoted as @¬@. Hopefully \\(O(1)\\).",
        "--",
        "-- prop> not (not x) == x",
        "not :: Bool -> Bool",
        "not x = case x of",
        "  False -> True",
        "  _ -> False",
        "",
        "-- | A 'first' and 'last' name together in one record.",
        "data Name = Name",
        "  { first :: String -- ^ The first name, like \\\"Haskell\\\".",
        "  , last :: String -- ^ The last name, like \\\"Curry\\\"",
        "  }",
        "",
        "-- | A type class for things that can be compared for __eq__uality.",
        "class Eq a where",
        "  -- | Returns 'True' if the two values are equal, 'False' otherwise.",
        "  (==) :: a -> a -> Bool"
      ]

application :: Wai.Application
application request respond = do
  body <- Wai.strictRequestBody request
  let input =
        Text.unpack
          . maybe defaultInput Encoding.decodeUtf8Lenient
          . Monad.join
          . lookup "input"
          . Http.parseQuery
          $ LazyByteString.toStrict body
      result = parseLHsModule "<interactive>" input
      headers =
        (Http.hContentType, "text/html;charset=utf-8")
          : defaultHeaders
  respond
    . Wai.responseLBS Http.ok200 headers
    . H.renderBS
    . H.doctypehtml_
    $ do
      H.head_ $ do
        H.meta_ [H.charset_ "utf-8"]
        H.meta_
          [ H.name_ "viewport",
            H.content_ "initial-scale = 1, width = device-width"
          ]
        H.title_ "Scrod"
        H.link_
          [ H.crossorigin_ "anonymous",
            H.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css",
            H.integrity_ "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH",
            H.rel_ "stylesheet"
          ]
        H.script_
          [ H.async_ "async",
            H.crossorigin_ "anonymous",
            H.integrity_ "sha384-Wuix6BuhrWbjDBs24bXrjf4ZQ5aFeFWBuKkFekO2t8xFU0iNaLQfp2K6/1Nxveei",
            H.src_ "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-mml-chtml.js"
          ]
          (mempty :: H.Html ())
      H.body_ $ do
        H.header_ [H.class_ "bg-primary mb-3 navbar"] $ do
          H.div_ [H.class_ "container"] $ do
            H.a_ [H.class_ "navbar-brand text-light", H.href_ "/"] "Scrod"

        H.main_ [H.class_ "my-3"] $ do
          H.div_ [H.class_ "container"] $ do
            H.div_ [H.class_ "row"] $ do
              H.div_ [H.class_ "col-lg"] $ do
                H.form_ [H.method_ "post"] $ do
                  H.textarea_
                    [ H.class_ "font-monospace form-control mb-3",
                      H.name_ "input",
                      H.rows_ "10"
                    ]
                    $ H.toHtml input
                  H.button_ [H.class_ "btn btn-primary", H.type_ "submit"] "Submit"
              H.div_ [H.class_ "col-lg"] $ do
                case result of
                  Left message -> H.div_ [H.class_ "alert alert-danger"] $ do
                    H.h2_ [H.class_ "alert-heading"] "Error"
                    H.pre_ [H.class_ "text-break text-wrap"] . H.code_ . H.toHtml $ show message
                  Right lHsModule -> do
                    let items = getItems lHsModule
                        lHsDocStrings = getLHsDocStrings lHsModule
                        tuples = mergeItems $ associateDocStrings items lHsDocStrings
                    if null tuples
                      then H.p_ "Nothing to see here."
                      else H.ul_ . Monad.forM_ tuples $ \(item, docStrings) -> H.li_ $ do
                        case Item.name item of
                          Name.Other string -> H.code_ $ H.toHtml string
                        Haddock.docHToHtml
                          . Haddock.overIdentifier (curry Just)
                          . Haddock._doc
                          . Haddock.parseParas Nothing
                          $ List.intercalate "\n\n" docStrings

        H.footer_ [H.class_ "my-3 text-secondary"] $ do
          H.div_ [H.class_ "border-top container pt-3"] $ do
            "Powered by "
            H.a_ [H.class_ "link-secondary", H.href_ "https://github.com/tfausak/scrod"] "tfausak/scrod"
            " version "
            H.toHtml $ Version.showVersion Package.version
            "."

defaultHeaders :: Http.ResponseHeaders
defaultHeaders =
  [ ("Referrer-Policy", "no-referrer"),
    ("Strict-Transport-Security", "max-age=31536000"),
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("X-XSS-Protection", "1; mode=block")
  ]

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS
    status
    ((Http.hContentType, "text/plain;charset=utf-8") : headers)
    . LazyByteString.fromStrict
    . (\bs -> bs <> " " <> Http.statusMessage status)
    . Encoding.encodeUtf8
    . Text.pack
    . show
    $ Http.statusCode status

settings :: Warp.Settings
settings =
  let host = String.fromString "*" :: Warp.HostPreference
      port = 3000 :: Warp.Port
   in Warp.defaultSettings
        & Warp.setBeforeMainLoop
          ( Printf.printf
              "Listening on %s port %d\n"
              (show host)
              port
          )
        & Warp.setHost host
        & Warp.setLogger
          ( \request status _ ->
              Printf.printf
                "%d %s %s %s\n"
                (Http.statusCode status)
                (Encoding.decodeUtf8Lenient $ Wai.requestMethod request)
                (Encoding.decodeUtf8Lenient $ Wai.rawPathInfo request)
                (Encoding.decodeUtf8Lenient $ Wai.rawQueryString request)
          )
        & Warp.setPort port

-- GHC ------------------------------------------------------------------------

parseLHsModule ::
  -- | Included in source spans but really only needed for error messages.
  FilePath ->
  -- | The source code to parse. Not currently clear which encoding Haskell
  -- files are expected to use. Probably UTF-8?
  String ->
  Either (Messages.Messages PsErr.PsMessage) (LHsModule.LHsModule GHC.Hs.GhcPs)
parseLHsModule filePath string =
  let onToJust :: Session.OnOff a -> Maybe a
      onToJust x = case x of
        Session.On y -> Just y
        Session.Off _ -> Nothing
      (lang, exts) = discoverExtensions string
      extensions = EnumSet.fromList $ Session.languageExtensions lang <> Maybe.mapMaybe onToJust exts
      parserOpts =
        Lexer.mkParserOpts
          extensions -- enabled extensions
          ErrUtil.emptyDiagOpts -- diagnostic options
          mempty -- supported extensions
          False -- enable safe imports?
          True -- keep Haddock comments?
          False -- keep regular comments?
          False -- allow position to be updated by pragmas?
      realSrcLoc =
        SrcLoc.mkRealSrcLoc
          (FastString.mkFastString filePath)
          1 -- first row
          1 -- first column
      pState =
        Lexer.initParserState
          parserOpts
          (StringBuffer.stringToStringBuffer string)
          realSrcLoc
   in case Lexer.unP Parser.parseModule pState of
        Lexer.PFailed newPState -> Left . Messages.Messages $ Lexer.getPsErrorMessages newPState
        Lexer.POk _warnings lHsModule -> Right $ LHsModule.LHsModule lHsModule

extractDocs :: (Data.Data a) => a -> [[SrcLoc.Located (GHC.Hs.WithHsDocIdentifiers GHC.Hs.HsDocString GHC.Hs.GhcPs)]]
extractDocs = Data.gmapQ $ \d -> case Data.cast d of
  Nothing -> concat $ extractDocs d
  Just hds -> [hds]

getLHsDocStrings :: LHsModule.LHsModule GHC.Hs.GhcPs -> [GHC.Hs.LHsDocString]
getLHsDocStrings = fmap (fmap GHC.Hs.hsDocString) . concat . extractDocs . LHsModule.unwrap

associateDocStrings :: [Item.Item] -> [GHC.Hs.LHsDocString] -> [(Item.Item, [String])]
associateDocStrings items lHsDocStrings =
  -- TODO: Do something with these other (named and group) doc strings.
  let (_other, xs) = associateDocStrings2 lHsDocStrings items
   in fmap Tuple.swap xs

associateDocStrings2 ::
  [GHC.Hs.LHsDocString] ->
  [Item.Item] ->
  ([GHC.Hs.LHsDocString], [([String], Item.Item)])
associateDocStrings2 lHsDocStrings items =
  let es = fmap simplifyDocString lHsDocStrings
      (xs, ts) = Either.partitionEithers es
      (ns, ps) =
        List.partition
          ( \t -> case snd $ fst t of
              Direction.Next -> True
              Direction.Previous -> False
          )
          ts
      sp = reverse ps
      -- TODO: Account for doc strings that don't get associated with an item,
      -- like a "previous" doc string at the beginning of the file.
      is1 = associateDocStringsHelper Direction.Next (fmap (\((p, _), s) -> (p, s)) ns) $ fmap ((,) []) items
      is2 = associateDocStringsHelper Direction.Previous (fmap (\((p, _), s) -> (p, s)) sp) $ reverse is1
   in (xs, reverse is2)

associateDocStringsHelper ::
  Direction.Direction ->
  [(Position.Position, String)] ->
  [([String], Item.Item)] ->
  [([String], Item.Item)]
associateDocStringsHelper x ds items = case items of
  [] -> []
  (ss, i) : is ->
    let (before, after) = span (\d -> (if x == Direction.Next then (<=) else (>=)) (fst d) (Item.position i)) ds
     in (ss <> fmap snd before, i) : associateDocStringsHelper x after is

simplifyDocString ::
  GHC.Hs.LHsDocString ->
  Either GHC.Hs.LHsDocString ((Position.Position, Direction.Direction), String)
simplifyDocString lHsDocString = Maybe.fromMaybe (Left lHsDocString) $ do
  let hsDocString = SrcLoc.unLoc lHsDocString
  hsDocStringDecorator <- getHsDocStringDecorator hsDocString
  direction <- case hsDocStringDecorator of
    GHC.Hs.HsDocStringNext -> Just Direction.Next
    GHC.Hs.HsDocStringPrevious -> Just Direction.Previous
    GHC.Hs.HsDocStringNamed {} -> Nothing
    GHC.Hs.HsDocStringGroup {} -> Nothing
  position <- Position.fromLocated lHsDocString
  pure $ Right ((position, direction), GHC.Hs.renderHsDocString hsDocString)

getHsDocStringDecorator :: GHC.Hs.HsDocString -> Maybe GHC.Hs.HsDocStringDecorator
getHsDocStringDecorator x = case x of
  GHC.Hs.MultiLineDocString y _ -> Just y
  GHC.Hs.NestedDocString y _ -> Just y
  GHC.Hs.GeneratedDocString {} -> Nothing

getItems :: LHsModule.LHsModule GHC.Hs.GhcPs -> [Item.Item]
getItems lHsModule = case SrcLoc.unLoc $ LHsModule.unwrap lHsModule of
  HS.HsModule {HS.hsmodDecls = lHsDecls} -> concatMap getLHsDeclItems lHsDecls

getLHsDeclItems :: HS.LHsDecl GHC.Hs.GhcPs -> [Item.Item]
getLHsDeclItems lHsDecl = case SrcLoc.unLoc lHsDecl of
  HS.TyClD _ tyClDecl -> case tyClDecl of
    HS.FamDecl {HS.tcdFam = familyDecl} -> case familyDecl of
      HS.FamilyDecl {HS.fdLName = lIdP} -> [lIdPToItem lIdP]
    HS.SynDecl {HS.tcdLName = lIdP} -> [lIdPToItem lIdP]
    HS.DataDecl {HS.tcdLName = lIdP, HS.tcdDataDefn = hsDataDefn} ->
      lIdPToItem lIdP
        : case HS.dd_cons hsDataDefn of
          HS.NewTypeCon lConDecl -> conDeclToItems $ SrcLoc.unLoc lConDecl
          HS.DataTypeCons _ lConDecls -> concatMap (conDeclToItems . SrcLoc.unLoc) lConDecls
    HS.ClassDecl {HS.tcdLName = lIdP, HS.tcdSigs = lSigs} ->
      lIdPToItem lIdP
        : concatMap (sigToItems . SrcLoc.unLoc) lSigs
  HS.InstD _ instDecl -> case instDecl of
    HS.ClsInstD {HS.cid_inst = clsInstDecl} -> case clsInstDecl of
      HS.ClsInstDecl {HS.cid_poly_ty = lHsSigType} -> hsSigTypeToItems $ SrcLoc.unLoc lHsSigType
    HS.DataFamInstD {HS.dfid_inst = dataFamInstDecl} -> famEqnToItems $ HS.dfid_eqn dataFamInstDecl
    HS.TyFamInstD {HS.tfid_inst = tyFamInstDecl} -> case tyFamInstDecl of
      HS.TyFamInstDecl {HS.tfid_eqn = tyFamInstEqn} -> famEqnToItems tyFamInstEqn
  HS.DerivD _ derivDecl -> case derivDecl of
    HS.DerivDecl {HS.deriv_type = lHsSigWcType} -> case lHsSigWcType of
      HS.HsWC {HS.hswc_body = lHsSigType} -> hsSigTypeToItems $ SrcLoc.unLoc lHsSigType
  HS.ValD _ hsBind -> case hsBind of
    HS.FunBind {HS.fun_id = lIdP} -> [lIdPToItem lIdP]
    HS.PatBind {HS.pat_lhs = lPat} -> patToItems $ SrcLoc.unLoc lPat
    HS.PatSynBind _ patSynBind -> case patSynBind of
      HS.PSB {HS.psb_id = lIdP, HS.psb_dir = hsPatSynDir} ->
        lIdPToItem lIdP
          : case hsPatSynDir of
            HS.Unidirectional -> []
            HS.ImplicitBidirectional -> []
            HS.ExplicitBidirectional matchGroup -> case matchGroup of
              HS.MG {HS.mg_alts = lMatches} ->
                ( \HS.Match {HS.m_ctxt = hsMatchContext} -> case hsMatchContext of
                    HS.FunRhs {HS.mc_fun = lIdP2} -> lIdPToItem lIdP2
                    _ -> error $ Data.showS hsMatchContext " -- unknown HsMatchContext"
                )
                  . SrcLoc.unLoc
                  <$> SrcLoc.unLoc lMatches
    HS.VarBind {} -> error "impossible: unexpected VarBind"
  HS.SigD _ sig -> sigToItems sig
  HS.KindSigD _ standaloneKindSig -> case standaloneKindSig of
    HS.StandaloneKindSig _ lIdP _ -> [lIdPToItem lIdP]
  HS.DefD {} ->
    -- TODO: This currently doesn't introduce anything that can be exported,
    -- but it will after this GHC proposal is implemented:
    -- <https://github.com/ghc-proposals/ghc-proposals/pull/409>
    []
  HS.ForD _ foreignDecl -> case foreignDecl of
    HS.ForeignImport {HS.fd_name = lIdP} -> [lIdPToItem lIdP]
    HS.ForeignExport {} -> []
  HS.WarningD _ warnDecls -> case warnDecls of
    HS.Warnings {HS.wd_warnings = lWarnDecls} ->
      concatMap
        ( \lWarnDecl -> case SrcLoc.unLoc lWarnDecl of
            HS.Warning {} ->
              -- TODO: This doesn't introduce any items, but it's associated
              -- with another identifier.
              []
        )
        lWarnDecls
  HS.AnnD _ annDecl -> case annDecl of
    HS.HsAnnotation {} ->
      -- TODO: This doesn't introduce any items, but it's associated with
      -- another identifier.
      []
  HS.RuleD _ ruleDecls -> case ruleDecls of
    HS.HsRules {HS.rds_rules = lRuleDecls} ->
      concatMap
        ( \lRuleDecl -> case SrcLoc.unLoc lRuleDecl of
            HS.HsRule {HS.rd_name = lRuleName} -> [Item.Item (Name.Other . FastString.unpackFS $ SrcLoc.unLoc lRuleName) (Position.fromLocatedAn lRuleName)]
        )
        lRuleDecls
  HS.SpliceD {} ->
    -- TODO: Warn that splices can't be resolved by the parser.
    []
  HS.DocD {} -> []
  HS.RoleAnnotD _ roleAnnotDecl -> case roleAnnotDecl of
    HS.RoleAnnotDecl _ lIdP _ -> [lIdPToItem lIdP]

conDeclToItems :: HS.ConDecl GHC.Hs.GhcPs -> [Item.Item]
conDeclToItems conDecl = case conDecl of
  HS.ConDeclH98 {HS.con_name = lIdP, HS.con_args = hsConDetails} ->
    lIdPToItem lIdP
      : case hsConDetails of
        HS.PrefixCon {} -> []
        HS.RecCon lConDeclFields -> concatMap lConDeclFieldToItems $ SrcLoc.unLoc lConDeclFields
        HS.InfixCon {} -> []
  HS.ConDeclGADT {HS.con_names = lIdPs, HS.con_g_args = hsConDeclGADTDetails} ->
    NonEmpty.toList (fmap lIdPToItem lIdPs)
      <> case hsConDeclGADTDetails of
        HS.PrefixConGADT {} -> []
        HS.RecConGADT _ lConDeclFields -> concatMap lConDeclFieldToItems $ SrcLoc.unLoc lConDeclFields

lConDeclFieldToItems :: HS.LConDeclField GHC.Hs.GhcPs -> [Item.Item]
lConDeclFieldToItems lConDeclField = case SrcLoc.unLoc lConDeclField of
  HS.ConDeclField {HS.cd_fld_names = lFieldOccs} ->
    fmap
      ( \lFieldOcc -> case SrcLoc.unLoc lFieldOcc of
          HS.FieldOcc {HS.foLabel = lRdrName} -> lIdPToItem lRdrName
      )
      lFieldOccs

sigToItems :: HS.Sig GHC.Hs.GhcPs -> [Item.Item]
sigToItems sig = case sig of
  HS.TypeSig _ lIdPs _ -> fmap lIdPToItem lIdPs
  HS.PatSynSig _ lIdPs _ -> fmap lIdPToItem lIdPs
  HS.ClassOpSig _ _ lIdPs _ -> fmap lIdPToItem lIdPs
  HS.FixSig _ fixitySig -> case fixitySig of
    HS.FixitySig _ lIdPs _ -> fmap lIdPToItem lIdPs
  HS.InlineSig {} -> []
  HS.SpecSig {} -> []
  HS.SpecInstSig {} -> []
  HS.MinimalSig {} ->
    -- TODO: This doesn't introduce any items, but it's associated with a type
    -- class.
    []
  HS.SCCFunSig {} -> []
  HS.CompleteMatchSig {} ->
    -- TODO: This doesn't introduce any items, but it's associated with a
    -- pattern synonym.
    []

patToItems :: HS.Pat GHC.Hs.GhcPs -> [Item.Item]
patToItems pat = case pat of
  HS.WildPat {} -> []
  HS.VarPat _ lIdP -> [lIdPToItem lIdP]
  HS.LazyPat _ lPat2 -> patToItems $ SrcLoc.unLoc lPat2
  HS.AsPat _ lIdP lPat2 -> lIdPToItem lIdP : patToItems (SrcLoc.unLoc lPat2)
  HS.ParPat _ lPat2 -> patToItems $ SrcLoc.unLoc lPat2
  HS.BangPat _ lPat2 -> patToItems $ SrcLoc.unLoc lPat2
  HS.ListPat _ lPats -> concatMap (patToItems . SrcLoc.unLoc) lPats
  HS.TuplePat _ lPats _ -> concatMap (patToItems . SrcLoc.unLoc) lPats
  HS.SumPat _ lPat2 _ _ -> patToItems $ SrcLoc.unLoc lPat2
  HS.ConPat {HS.pat_args = hsConPatDetails} -> case hsConPatDetails of
    HS.PrefixCon _ lPats -> concatMap (patToItems . SrcLoc.unLoc) lPats
    HS.RecCon hsRecFields ->
      concatMap
        ( ( \hsRecField ->
              if HS.hfbPun hsRecField
                then case SrcLoc.unLoc $ HS.hfbLHS hsRecField of
                  HS.FieldOcc {HS.foLabel = lRdrName} -> [lIdPToItem lRdrName]
                else patToItems . SrcLoc.unLoc $ HS.hfbRHS hsRecField
          )
            . SrcLoc.unLoc
        )
        (HS.rec_flds hsRecFields)
        -- TODO: Warn that record wild cards can't be resolved by the parser.
        <> maybe [] (const []) (HS.rec_dotdot hsRecFields)
    HS.InfixCon l r -> patToItems (SrcLoc.unLoc l) <> patToItems (SrcLoc.unLoc r)
  HS.ViewPat _ _ lPat2 -> patToItems $ SrcLoc.unLoc lPat2
  HS.SplicePat {} ->
    -- TODO: Warn that splices can't be resolved by the parser.
    []
  HS.LitPat {} -> []
  HS.NPat {} -> []
  HS.NPlusKPat _ lIdP _ _ _ _ -> [lIdPToItem lIdP]
  HS.SigPat _ lPat2 _ -> patToItems $ SrcLoc.unLoc lPat2
  HS.EmbTyPat {} -> error "impossible: unexpected EmbTyPat"
  HS.InvisPat {} -> error "impossible: unexpected InvisPat"

hsSigTypeToItems :: HS.HsSigType GHC.Hs.GhcPs -> [Item.Item]
hsSigTypeToItems hsSigType = case hsSigType of
  HS.HsSig {HS.sig_body = lHsType} -> hsTypeToItems $ SrcLoc.unLoc lHsType

hsTypeToItems :: HS.HsType GHC.Hs.GhcPs -> [Item.Item]
hsTypeToItems hsType = case hsType of
  HS.HsTyVar _ _ lIdP -> [lIdPToItem lIdP]
  HS.HsAppTy _ lHsType _ -> hsTypeToItems $ SrcLoc.unLoc lHsType
  HS.HsParTy _ lHsType -> hsTypeToItems $ SrcLoc.unLoc lHsType
  HS.HsQualTy {HS.hst_body = lHsType} -> hsTypeToItems $ SrcLoc.unLoc lHsType
  _ -> error $ Data.showS hsType " -- unknown HsType"

famEqnToItems :: HS.FamEqn GHC.Hs.GhcPs rhs -> [Item.Item]
famEqnToItems famEqn = case famEqn of
  HS.FamEqn {HS.feqn_tycon = lIdP} -> [lIdPToItem lIdP]

rdrNameToString :: RdrName.RdrName -> String
rdrNameToString rdrName = case rdrName of
  RdrName.Unqual occName -> OccName.occNameString occName
  -- RdrName.Qual moduleName occName -> HS.moduleNameString moduleName <> "." <> OccName.occNameString occName
  _ -> error $ Data.showS rdrName " -- unknown RdrName"

lIdPToItem :: HS.LIdP GHC.Hs.GhcPs -> Item.Item
lIdPToItem lIdP =
  Item.Item
    { Item.name = Name.Other . rdrNameToString $ SrcLoc.unLoc lIdP,
      Item.position = Position.fromLocatedAn lIdP
    }
