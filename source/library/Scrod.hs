{-# LANGUAGE FlexibleInstances #-}

module Scrod where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Data as Data
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Data.Void as Void
import qualified Documentation.Haddock.Markup as Haddock
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type as X
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Errors.Types as PsErr
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Stack as Stack
import qualified GHC.Types.Name.Occurrence as OccName
import qualified GHC.Types.Name.Reader as RdrName
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Error as ErrUtil
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as HS
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_scrod as Package
import qualified System.IO as IO
import qualified Test.Hspec as Hspec
import qualified Text.Printf as Printf

-- Test Suite -----------------------------------------------------------------

testSuite :: IO ()
testSuite = Hspec.hspec . Hspec.parallel . Hspec.describe "Scrod" $ do
  Hspec.describe "getItems" $ do
    let f = either (error . show) getItems . parseLHsModule ""

    Hspec.it "empty" $ do
      f "" `Hspec.shouldBe` []

    Hspec.it "type family" $ do
      f "type family A" `Hspec.shouldBe` [Item "A" $ Position 1 13]

    Hspec.it "data family" $ do
      f "data family B" `Hspec.shouldBe` [Item "B" $ Position 1 13]

    Hspec.it "type synonym" $ do
      f "type C = ()" `Hspec.shouldBe` [Item "C" $ Position 1 6]

    Hspec.it "data" $ do
      f "data D" `Hspec.shouldBe` [Item "D" $ Position 1 6]

    Hspec.it "data constructor" $ do
      f "data X = E" `Hspec.shouldBe` [Item "X" $ Position 1 6, Item "E" $ Position 1 10]

    Hspec.it "data constructor gadt" $ do
      f "data X where E :: X" `Hspec.shouldBe` [Item "X" $ Position 1 6, Item "E" $ Position 1 14]

    Hspec.it "record field" $ do
      f "newtype T = C { f :: () }" `Hspec.shouldBe` [Item "T" $ Position 1 9, Item "C" $ Position 1 13, Item "f" $ Position 1 17]

    Hspec.it "record fields with one signature" $ do
      f "newtype T = C { f, g :: () }" `Hspec.shouldBe` [Item "T" $ Position 1 9, Item "C" $ Position 1 13, Item "f" $ Position 1 17, Item "g" $ Position 1 20]

    Hspec.it "record fields with separate signatures" $ do
      f "newtype T = C { f :: (), g :: () }" `Hspec.shouldBe` [Item "T" $ Position 1 9, Item "C" $ Position 1 13, Item "f" $ Position 1 17, Item "g" $ Position 1 26]

    Hspec.it "record field gadt" $ do
      f "newtype T where C :: { f :: () } -> T" `Hspec.shouldBe` [Item "T" $ Position 1 9, Item "C" $ Position 1 17, Item "f" $ Position 1 24]

    Hspec.it "class" $ do
      f "class E" `Hspec.shouldBe` [Item "E" $ Position 1 7]

    Hspec.it "class instance" $ do
      f "instance F" `Hspec.shouldBe` [Item "F" $ Position 1 10]

    Hspec.it "data instance" $ do
      f "data instance G" `Hspec.shouldBe` [Item "G" $ Position 1 15]

    Hspec.it "newtype instance" $ do
      f "newtype instance H = X" `Hspec.shouldBe` [Item "H" $ Position 1 18]

    Hspec.it "type instance" $ do
      f "type instance I = X" `Hspec.shouldBe` [Item "I" $ Position 1 15]

    Hspec.it "deriving instance" $ do
      f "deriving instance J" `Hspec.shouldBe` [Item "J" $ Position 1 19]

    Hspec.it "function" $ do
      f "h x = x" `Hspec.shouldBe` [Item "h" $ Position 1 1]

    Hspec.it "infix function" $ do
      f "_ `f` _ = ()" `Hspec.shouldBe` [Item "f" $ Position 1 3]

    Hspec.it "infix operator" $ do
      f "_ & _ = ()" `Hspec.shouldBe` [Item "&" $ Position 1 3]

    Hspec.it "prefix operator" $ do
      -- TODO: Why is this at column 1 rather than 2?
      f "(&) = ()" `Hspec.shouldBe` [Item "&" $ Position 1 1]

    Hspec.it "variable" $ do
      f "i = ()" `Hspec.shouldBe` [Item "i" $ Position 1 1]

    Hspec.it "variable on another line" $ do
      f "\ni = ()" `Hspec.shouldBe` [Item "i" $ Position 2 1]

    Hspec.it "variable indented" $ do
      f " i = ()" `Hspec.shouldBe` [Item "i" $ Position 1 2]

    Hspec.it "strict variable" $ do
      f "!j = ()" `Hspec.shouldBe` [Item "j" $ Position 1 2]

    Hspec.it "wildcard pattern" $ do
      f "_ = ()" `Hspec.shouldBe` []

    Hspec.it "lazy pattern" $ do
      f "~k = ()" `Hspec.shouldBe` [Item "k" $ Position 1 2]

    Hspec.it "as pattern" $ do
      f "l@m = ()" `Hspec.shouldBe` [Item "l" $ Position 1 1, Item "m" $ Position 1 3]

    Hspec.it "patenthesized pattern" $ do
      f "(n) = ()" `Hspec.shouldBe` [Item "n" $ Position 1 2]

    Hspec.it "bang pattern" $ do
      -- Note that this is different than the "strict variable" test case!
      f "(!o) = ()" `Hspec.shouldBe` [Item "o" $ Position 1 3]

    Hspec.it "list pattern" $ do
      f "[p] = ()" `Hspec.shouldBe` [Item "p" $ Position 1 2]

    Hspec.it "tuple pattern" $ do
      f "(q, r) = ()" `Hspec.shouldBe` [Item "q" $ Position 1 2, Item "r" $ Position 1 5]

    Hspec.it "anonymous sum pattern" $ do
      f "(# s | #) = ()" `Hspec.shouldBe` [Item "s" $ Position 1 4]

    Hspec.it "prefix constructor pattern" $ do
      f "Just t = ()" `Hspec.shouldBe` [Item "t" $ Position 1 6]

    Hspec.it "record constructor pattern" $ do
      f "X { u = v } = ()" `Hspec.shouldBe` [Item "v" $ Position 1 9]

    Hspec.it "punned record pattern" $ do
      f "X { w } = ()" `Hspec.shouldBe` [Item "w" $ Position 1 5]

    Hspec.it "wild card record pattern" $ do
      f "X { .. } = ()" `Hspec.shouldBe` []

    Hspec.it "infix constructor pattern" $ do
      f "(x : _) = ()" `Hspec.shouldBe` [Item "x" $ Position 1 2]

    Hspec.it "view pattern" $ do
      f "(f -> y) = ()" `Hspec.shouldBe` [Item "y" $ Position 1 7]

    Hspec.it "splice pattern" $ do
      f "$( x ) = ()" `Hspec.shouldBe` []

    Hspec.it "literal pattern" $ do
      f "'x' = ()" `Hspec.shouldBe` []

    Hspec.it "natural pattern" $ do
      f "0 = ()" `Hspec.shouldBe` []

    Hspec.it "n+k pattern" $ do
      f "(z + 1) = ()" `Hspec.shouldBe` [Item "z" $ Position 1 2]

    Hspec.it "signature pattern" $ do
      f "(a :: ()) = ()" `Hspec.shouldBe` [Item "a" $ Position 1 2]

    Hspec.it "bidirectional pattern synonym" $ do
      f "pattern B = ()" `Hspec.shouldBe` [Item "B" $ Position 1 9]

    Hspec.it "unidirectional pattern synonym" $ do
      f "pattern C <- ()" `Hspec.shouldBe` [Item "C" $ Position 1 9]

    Hspec.it "explicitly bidirectional pattern synonym" $ do
      -- The two names always have to match, so this is only a single item.
      f "pattern D <- () where D = ()" `Hspec.shouldBe` [Item "D" $ Position 1 9]

    Hspec.it "type signature" $ do
      f "e :: ()" `Hspec.shouldBe` [Item "e" $ Position 1 1]

    Hspec.it "pattern type signature" $ do
      f "pattern F :: ()" `Hspec.shouldBe` [Item "F" $ Position 1 9]

    Hspec.it "method signature" $ do
      f "class X where g :: ()" `Hspec.shouldBe` [Item "X" $ Position 1 7, Item "g" $ Position 1 15]

    Hspec.it "default method signature" $ do
      f "class X where default h :: ()" `Hspec.shouldBe` [Item "X" $ Position 1 7, Item "h" $ Position 1 23]

    Hspec.it "fixity declaration" $ do
      f "infix 5 %" `Hspec.shouldBe` [Item "%" $ Position 1 9]

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
      f "type O :: ()" `Hspec.shouldBe` [Item "O" $ Position 1 6]

    Hspec.it "default declaration" $ do
      f "default ()" `Hspec.shouldBe` []

    Hspec.it "foreign import" $ do
      f "foreign import ccall \"\" p :: ()" `Hspec.shouldBe` [Item "p" $ Position 1 25]

    Hspec.it "warning pragma" $ do
      f "{-# warning x \"\" #-}" `Hspec.shouldBe` []

    Hspec.it "value annotation" $ do
      f "{-# ann x () #-}" `Hspec.shouldBe` []

    Hspec.it "type annotation" $ do
      f "{-# ann type X () #-}" `Hspec.shouldBe` []

    Hspec.it "module annotation" $ do
      f "{-# ann module () #-}" `Hspec.shouldBe` []

    Hspec.it "rules pragma" $ do
      f "{-# rules \"q\" x = () #-}" `Hspec.shouldBe` [Item "q" $ Position 1 11]

    Hspec.it "splice declaration" $ do
      f "$( x )" `Hspec.shouldBe` []

    Hspec.it "documentation" $ do
      f "-- | x" `Hspec.shouldBe` []

    Hspec.it "role annotation" $ do
      f "type role R nominal" `Hspec.shouldBe` [Item "R" $ Position 1 11]

  Hspec.describe "associateDocStrings" $ do
    let mkItem n l = Item n . Position l
        mkSrcLoc = SrcLoc.mkSrcLoc $ FastString.mkFastString ""
        mkSrcSpan (l1, c1) (l2, c2) = SrcLoc.mkSrcSpan (mkSrcLoc l1 c1) (mkSrcLoc l2 c2)

    Hspec.it "empty" $ do
      associateDocStrings [] [] `Hspec.shouldBe` []

    Hspec.it "item only" $ do
      let item = mkItem "a" 1 1
      associateDocStrings [item] [] `Hspec.shouldBe` [(item, [])]

    Hspec.it "doc only" $ do
      let lHsDocString =
            SrcLoc.L
              (mkSrcSpan (1, 1) (1, 1))
              ( GHC.Hs.MultiLineDocString
                  GHC.Hs.HsDocStringNext
                  ( pure
                      ( SrcLoc.L
                          (mkSrcSpan (1, 1) (1, 1))
                          (GHC.Hs.mkHsDocStringChunk "a")
                      )
                  )
              )
      associateDocStrings [] [lHsDocString] `Hspec.shouldBe` []

    Hspec.it "doc before item" $ do
      let item = mkItem "a" 2 1
          lHsDocString =
            SrcLoc.L
              (mkSrcSpan (1, 1) (1, 1))
              ( GHC.Hs.MultiLineDocString
                  GHC.Hs.HsDocStringNext
                  ( pure
                      ( SrcLoc.L
                          (mkSrcSpan (1, 1) (1, 1))
                          (GHC.Hs.mkHsDocStringChunk "b")
                      )
                  )
              )
      associateDocStrings [item] [lHsDocString] `Hspec.shouldBe` [(item, ["b"])]

-- Executable -----------------------------------------------------------------

executable :: IO ()
executable = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  Warp.runSettings settings $ \request respond ->
    case (Http.parseMethod $ Wai.requestMethod request, Text.unpack <$> Wai.pathInfo request) of
      (Right Http.GET, []) -> application request respond
      (Right Http.POST, []) -> application request respond
      _ -> respond $ statusResponse Http.status404 defaultHeaders

application :: Wai.Application
application request respond = do
  body <- Wai.strictRequestBody request
  let input =
        Text.unpack
          . maybe Text.empty Encoding.decodeUtf8Lenient
          . Monad.join
          . lookup (utf8 "input")
          . Http.parseQuery
          $ LazyByteString.toStrict body
      result = parseLHsModule "<interactive>" input
      headers =
        (Http.hContentType, utf8 "text/html;charset=utf-8")
          : defaultHeaders
  respond
    . Wai.responseLBS Http.ok200 headers
    . H.renderBS
    . H.doctypehtml_
    $ do
      H.head_ $ do
        H.meta_ [H.charset_ $ text "utf-8"]
        H.meta_
          [ H.name_ $ text "viewport",
            H.content_ $ text "initial-scale = 1, width = device-width"
          ]
        H.title_ $ html "Scrod"
        H.style_ $ text "pre { white-space: pre-wrap; }"
      H.body_ $ do
        H.h1_ $ html "Scrod"

        H.form_ [H.method_ $ text "post"] $ do
          H.textarea_ [H.name_ $ text "input"] $ html input
          H.button_ [] $ html "Submit"

        case result of
          Left message -> do
            H.h2_ $ html "Error"
            H.pre_ . H.code_ . html $ show message
          Right lHsModule -> do
            H.h2_ $ html "Parsed"
            H.details_ $ do
              H.summary_ $ html "Click to expand."
              H.pre_ . H.code_ . html $ show lHsModule

            H.h2_ $ html "Items"
            H.ul_ $ Monad.forM_ (getItems lHsModule) $ \item ->
              H.li_ $ do
                html . show . positionLine $ itemPosition item
                html ":"
                html . show . positionColumn $ itemPosition item
                html ": "
                html $ itemName item

            H.h2_ $ html "Haddocks"
            H.ul_ $ Monad.forM_ (getLHsDocStrings lHsModule) $ \lHsDocString -> do
              H.li_ $ do
                html . maybe "?" (show . positionLine) $ locatedToPosition lHsDocString
                html ":"
                html . maybe "?" (show . positionColumn) $ locatedToPosition lHsDocString
                html ": "
                docHToHtml . hsDocStringToDocH $ SrcLoc.unLoc lHsDocString

        html "Powered by "
        H.a_ [H.href_ $ text "https://github.com/tfausak/scrod"] $ html "tfausak/scrod"
        html " version "
        html $ Version.showVersion Package.version
        html "."

defaultHeaders :: Http.ResponseHeaders
defaultHeaders =
  [ (ci $ utf8 "Referrer-Policy", utf8 "no-referrer"),
    (ci $ utf8 "Strict-Transport-Security", utf8 "max-age=31536000"),
    (ci $ utf8 "X-Content-Type-Options", utf8 "nosniff"),
    (ci $ utf8 "X-Frame-Options", utf8 "DENY"),
    (ci $ utf8 "X-XSS-Protection", utf8 "1; mode=block")
  ]

statusResponse :: Http.Status -> Http.ResponseHeaders -> Wai.Response
statusResponse status headers =
  Wai.responseLBS
    status
    ((Http.hContentType, utf8 "text/plain;charset=utf-8") : headers)
    . LazyByteString.fromStrict
    . (\bs -> bs <> utf8 " " <> Http.statusMessage status)
    . utf8
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
  Either (Messages PsErr.PsMessage) (LHsModule GHC.Hs.GhcPs)
parseLHsModule filePath string =
  let parserOpts =
        Lexer.mkParserOpts
          -- TODO: Parsing extension pragmas requires dealing with DynFlags.
          -- https://github.com/tfausak/monadoc-5/blob/22a743f6/src/lib/Monadoc/Utility/Ghc.hs#L62
          ( EnumSet.fromList
              [ X.ForeignFunctionInterface,
                X.NPlusKPatterns,
                X.PatternSynonyms,
                X.TemplateHaskellQuotes,
                X.UnboxedSums
              ]
          ) -- enabled extensions
          ErrUtil.emptyDiagOpts -- diagnostic options
          [] -- supported extensions
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
        Lexer.PFailed newPState -> Left . Messages $ Lexer.getPsErrorMessages newPState
        Lexer.POk _warnings lHsModule -> Right $ LHsModule lHsModule

-- | Wrapper to avoid orphans.
newtype LHsModule a = LHsModule
  { unwrapLHsModule :: SrcLoc.Located (HS.HsModule a)
  }

-- | Provided for convenience.
instance Show (LHsModule GHC.Hs.GhcPs) where
  show = ($ "") . dataShowS . unwrapLHsModule

-- | Wrapper to avoid orphans.
newtype Messages a = Messages
  { unwrapMessages :: ErrUtil.Messages a
  }

-- | Provided for convenience.
instance (ErrUtil.Diagnostic a) => Show (Messages a) where
  show = Outputable.showPprUnsafe . unwrapMessages

extractDocs :: (Data.Data a) => a -> [[SrcLoc.Located (GHC.Hs.WithHsDocIdentifiers GHC.Hs.HsDocString GHC.Hs.GhcPs)]]
extractDocs = Data.gmapQ $ \d -> case Data.cast d of
  Nothing -> concat $ extractDocs d
  Just hds -> [hds]

getLHsDocStrings :: LHsModule GHC.Hs.GhcPs -> [GHC.Hs.LHsDocString]
getLHsDocStrings = fmap (fmap GHC.Hs.hsDocString) . concat . extractDocs . unwrapLHsModule

associateDocStrings :: [Item] -> [GHC.Hs.LHsDocString] -> [(Item, [String])]
associateDocStrings items lHsDocStrings = case items of
  [] -> []
  i : is ->
    let (before, after) =
          span
            ( \lHsDocString -> case locatedToPosition lHsDocString of
                Nothing -> True
                Just p -> case getHsDocStringDecorator $ SrcLoc.unLoc lHsDocString of
                  Nothing -> True
                  Just hsDocStringDecorator -> case hsDocStringDecorator of
                    GHC.Hs.HsDocStringNext -> p <= itemPosition i
                    GHC.Hs.HsDocStringPrevious -> error "TODO"
                    GHC.Hs.HsDocStringNamed {} -> error "TODO"
                    GHC.Hs.HsDocStringGroup {} -> error "TODO"
            )
            lHsDocStrings
     in (i, fmap (GHC.Hs.renderHsDocString . SrcLoc.unLoc) before)
          : associateDocStrings is after

getHsDocStringDecorator :: GHC.Hs.HsDocString -> Maybe GHC.Hs.HsDocStringDecorator
getHsDocStringDecorator x = case x of
  GHC.Hs.MultiLineDocString y _ -> Just y
  GHC.Hs.NestedDocString y _ -> Just y
  GHC.Hs.GeneratedDocString {} -> Nothing

data Item = Item
  { itemName :: String,
    itemPosition :: Position
  }
  deriving (Eq, Show)

data Position = Position
  { positionLine :: Int,
    positionColumn :: Int
  }
  deriving (Eq, Ord, Show)

locatedToPosition :: SrcLoc.Located a -> Maybe Position
locatedToPosition = srcSpanToPosition . SrcLoc.getLoc

srcSpanToPosition :: SrcLoc.SrcSpan -> Maybe Position
srcSpanToPosition x = case x of
  SrcLoc.RealSrcSpan y _ -> Just $ realSrcSpanToPosition y
  SrcLoc.UnhelpfulSpan {} -> Nothing

locatedAnToPosition :: GHC.Hs.LocatedAn a b -> Position
locatedAnToPosition = epAnnToPosition . SrcLoc.getLoc

epAnnToPosition :: GHC.Hs.EpAnn a -> Position
epAnnToPosition = realSrcSpanToPosition . GHC.Hs.epaLocationRealSrcSpan . GHC.Hs.entry

realSrcSpanToPosition :: SrcLoc.RealSrcSpan -> Position
realSrcSpanToPosition = realSrcLocToPosition . SrcLoc.realSrcSpanStart

realSrcLocToPosition :: SrcLoc.RealSrcLoc -> Position
realSrcLocToPosition x =
  Position
    { positionLine = SrcLoc.srcLocLine x,
      positionColumn = SrcLoc.srcLocCol x
    }

getItems :: LHsModule GHC.Hs.GhcPs -> [Item]
getItems lHsModule = case SrcLoc.unLoc $ unwrapLHsModule lHsModule of
  HS.HsModule {HS.hsmodDecls = lHsDecls} -> concatMap getLHsDeclItems lHsDecls

getLHsDeclItems :: HS.LHsDecl GHC.Hs.GhcPs -> [Item]
getLHsDeclItems lHsDecl = case SrcLoc.unLoc lHsDecl of
  HS.TyClD _ tyClDecl -> case tyClDecl of
    HS.FamDecl {HS.tcdFam = familyDecl} -> case familyDecl of
      HS.FamilyDecl {HS.fdLName = lIdP} -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
    HS.SynDecl {HS.tcdLName = lIdP} -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
    HS.DataDecl {HS.tcdLName = lIdP, HS.tcdDataDefn = hsDataDefn} ->
      Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)
        : case HS.dd_cons hsDataDefn of
          HS.NewTypeCon lConDecl -> conDeclToItems $ SrcLoc.unLoc lConDecl
          HS.DataTypeCons _ lConDecls -> concatMap (conDeclToItems . SrcLoc.unLoc) lConDecls
    HS.ClassDecl {HS.tcdLName = lIdP, HS.tcdSigs = lSigs} ->
      Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)
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
    HS.FunBind {HS.fun_id = lIdP} -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
    HS.PatBind {HS.pat_lhs = lPat} -> patToItems $ SrcLoc.unLoc lPat
    HS.PatSynBind _ patSynBind ->
      let lIdP = HS.psb_id patSynBind
       in [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
    HS.VarBind {} -> impossible "unexpected VarBind"
  HS.SigD _ sig -> sigToItems sig
  HS.KindSigD _ standaloneKindSig -> case standaloneKindSig of
    HS.StandaloneKindSig _ lIdP _ -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
  HS.DefD _ _ ->
    -- TODO: This currently doesn't introduce anything that can be exported,
    -- but it will after this GHC proposal is implemented:
    -- <https://github.com/ghc-proposals/ghc-proposals/pull/409>
    []
  HS.ForD _ foreignDecl -> case foreignDecl of
    HS.ForeignImport {HS.fd_name = lIdP} -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
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
            HS.HsRule {HS.rd_name = lRuleName} -> [Item (FastString.unpackFS $ SrcLoc.unLoc lRuleName) (locatedAnToPosition lRuleName)]
        )
        lRuleDecls
  HS.SpliceD {} ->
    -- TODO: Warn that splices can't be resolved by the parser.
    []
  HS.DocD {} -> []
  HS.RoleAnnotD _ roleAnnotDecl -> case roleAnnotDecl of
    HS.RoleAnnotDecl _ lIdP _ -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]

conDeclToItems :: HS.ConDecl GHC.Hs.GhcPs -> [Item]
conDeclToItems conDecl = case conDecl of
  HS.ConDeclH98 {HS.con_name = lIdP, HS.con_args = hsConDetails} ->
    Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)
      : case hsConDetails of
        HS.PrefixCon {} -> []
        HS.RecCon lConDeclFields -> concatMap lConDeclFieldToItems $ SrcLoc.unLoc lConDeclFields
        HS.InfixCon {} -> []
  HS.ConDeclGADT {HS.con_names = lIdPs, HS.con_g_args = hsConDeclGADTDetails} ->
    NonEmpty.toList (fmap (\lIdP -> Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)) lIdPs)
      <> case hsConDeclGADTDetails of
        HS.PrefixConGADT {} -> []
        HS.RecConGADT _ lConDeclFields -> concatMap lConDeclFieldToItems $ SrcLoc.unLoc lConDeclFields

lConDeclFieldToItems :: HS.LConDeclField GHC.Hs.GhcPs -> [Item]
lConDeclFieldToItems lConDeclField = case SrcLoc.unLoc lConDeclField of
  HS.ConDeclField {HS.cd_fld_names = lFieldOccs} ->
    fmap
      ( \lFieldOcc -> case SrcLoc.unLoc lFieldOcc of
          HS.FieldOcc {HS.foLabel = lRdrName} -> Item (rdrNameToString $ SrcLoc.unLoc lRdrName) (locatedAnToPosition lRdrName)
      )
      lFieldOccs

sigToItems :: HS.Sig GHC.Hs.GhcPs -> [Item]
sigToItems sig = case sig of
  HS.TypeSig _ lIdPs _ -> fmap (\lIdP -> Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)) lIdPs
  HS.PatSynSig _ lIdPs _ -> fmap (\lIdP -> Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)) lIdPs
  HS.ClassOpSig _ _ lIdPs _ -> fmap (\lIdP -> Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)) lIdPs
  HS.FixSig _ fixitySig -> case fixitySig of
    HS.FixitySig _ lIdPs _ -> fmap (\lIdP -> Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)) lIdPs
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

patToItems :: HS.Pat GHC.Hs.GhcPs -> [Item]
patToItems pat = case pat of
  HS.WildPat {} -> []
  HS.VarPat _ lIdP -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
  HS.LazyPat _ lPat2 -> patToItems $ SrcLoc.unLoc lPat2
  HS.AsPat _ lIdP lPat2 -> Item (rdrNameToString (SrcLoc.unLoc lIdP)) (locatedAnToPosition lIdP) : patToItems (SrcLoc.unLoc lPat2)
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
                  HS.FieldOcc {HS.foLabel = lRdrName} -> [Item (rdrNameToString $ SrcLoc.unLoc lRdrName) (locatedAnToPosition lRdrName)]
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
  HS.NPlusKPat _ lIdP _ _ _ _ -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
  HS.SigPat _ lPat2 _ -> patToItems $ SrcLoc.unLoc lPat2
  HS.EmbTyPat {} -> impossible "unexpected EmbTyPat"
  HS.InvisPat {} -> impossible "unexpected InvisPat"

hsSigTypeToItems :: HS.HsSigType GHC.Hs.GhcPs -> [Item]
hsSigTypeToItems hsSigType = case hsSigType of
  HS.HsSig {HS.sig_body = lHsType} -> hsTypeToItems $ SrcLoc.unLoc lHsType

hsTypeToItems :: HS.HsType GHC.Hs.GhcPs -> [Item]
hsTypeToItems hsType = case hsType of
  HS.HsTyVar _ _ lIdP -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]
  _ -> error $ dataShowS hsType " -- unknown HsType"

famEqnToItems :: HS.FamEqn GHC.Hs.GhcPs rhs -> [Item]
famEqnToItems famEqn = case famEqn of
  HS.FamEqn {HS.feqn_tycon = lIdP} -> [Item (rdrNameToString $ SrcLoc.unLoc lIdP) (locatedAnToPosition lIdP)]

rdrNameToString :: RdrName.RdrName -> String
rdrNameToString rdrName = case rdrName of
  RdrName.Unqual occName -> OccName.occNameString occName
  -- RdrName.Qual moduleName occName -> HS.moduleNameString moduleName <> "." <> OccName.occNameString occName
  _ -> error $ dataShowS rdrName " -- unknown RdrName"

impossible :: (Stack.HasCallStack) => String -> a
impossible = error . mappend "impossible: "

-- Haddock --------------------------------------------------------------------

hsDocStringToDocH :: GHC.Hs.HsDocString -> Haddock.DocH Void.Void (Haddock.Namespace, String)
hsDocStringToDocH =
  Haddock.overIdentifier (curry Just)
    . Haddock._doc
    . Haddock.parseParas Nothing
    . GHC.Hs.renderHsDocString

docHToHtml :: Haddock.DocH Void.Void (Haddock.Namespace, String) -> H.Html ()
docHToHtml = Haddock.markup htmlDocMarkupH

htmlDocMarkupH :: Haddock.DocMarkupH Void.Void (Haddock.Namespace, String) (H.Html ())
htmlDocMarkupH =
  Haddock.Markup
    { Haddock.markupAName = \x -> H.a_ [H.name_ $ text x] mempty,
      Haddock.markupAppend = mappend,
      Haddock.markupBold = H.strong_,
      Haddock.markupCodeBlock = H.pre_ . H.code_,
      Haddock.markupDefList = H.dl_ . foldMap (\(t, d) -> H.dt_ t <> H.dd_ d),
      Haddock.markupEmphasis = H.em_,
      Haddock.markupEmpty = mempty,
      Haddock.markupExample = foldMap $ \x -> H.pre_ . H.code_ . html . List.intercalate "\n" $ (">>> " <> Haddock.exampleExpression x) : Haddock.exampleResult x,
      Haddock.markupHeader = \x -> H.term (text $ "h" <> show (Haddock.headerLevel x)) $ Haddock.headerTitle x,
      Haddock.markupHyperlink = \x -> let url = text $ Haddock.hyperlinkUrl x in H.a_ [H.href_ url, H.rel_ $ text "nofollow"] . Maybe.fromMaybe (html url) $ Haddock.hyperlinkLabel x,
      Haddock.markupIdentifier = H.code_ . html . snd,
      Haddock.markupIdentifierUnchecked = Void.absurd,
      Haddock.markupMathDisplay = \x -> H.div_ . html $ "\\[" <> x <> "\\]",
      Haddock.markupMathInline = \x -> H.span_ . html $ "\\(" <> x <> "\\)",
      Haddock.markupModule = \x -> H.code_ . Maybe.fromMaybe (html $ Haddock.modLinkName x) $ Haddock.modLinkLabel x,
      Haddock.markupMonospaced = H.code_,
      Haddock.markupOrderedList = H.ol_ . foldMap (\(i, x) -> H.li_ [H.value_ . text $ show i] x),
      Haddock.markupParagraph = H.p_,
      Haddock.markupPic = \x -> H.img_ [H.alt_ . maybe Text.empty text $ Haddock.pictureTitle x, H.src_ . text $ Haddock.pictureUri x],
      Haddock.markupProperty = H.pre_ . H.code_ . html . mappend "prop> ",
      Haddock.markupString = html,
      Haddock.markupTable = impossible "markupTable",
      Haddock.markupUnorderedList = H.ul_ . foldMap H.li_,
      Haddock.markupWarning = impossible "markupWarning"
    }

-- Helpers --------------------------------------------------------------------

ci :: (CI.FoldCase a) => a -> CI.CI a
ci = CI.mk

-- | Adapted from <https://hackage.haskell.org/package/syb-0.7.2.4>.
dataShowS :: (Data.Data a) => a -> ShowS
dataShowS =
  let extQ ::
        (Data.Typeable a, Data.Typeable b) =>
        (a -> r) -> (b -> r) -> a -> r
      extQ f g a = maybe (f a) g (Data.cast a)
   in ( \t ->
          showChar '('
            . showString (Data.showConstr $ Data.toConstr t)
            . ( case () of
                  ()
                    | Just x <- Data.cast t -> showString . show $ GHC.Hs.renderHsDocString x
                    | Just x <- Data.cast t -> showString . show $ OccName.occNameString x
                    | otherwise -> foldr (.) id $ Data.gmapQ ((showChar ' ' .) . dataShowS) t
              )
            . showChar ')'
      )
        `extQ` (shows :: String -> ShowS)

html :: (H.ToHtml a) => a -> H.Html ()
html = H.toHtml

text :: String -> Text.Text
text = Text.pack

utf8 :: String -> ByteString.ByteString
utf8 = Encoding.encodeUtf8 . text
