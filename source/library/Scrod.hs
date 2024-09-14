{-# LANGUAGE FlexibleInstances #-}

module Scrod where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.Data as Data
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Void as Void
import qualified Documentation.Haddock.Markup as Haddock
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.StringBuffer as StringBuffer
import qualified GHC.Hs
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Errors.Types as PsErr
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Utils.Error as ErrUtil
import qualified GHC.Utils.Outputable as Outputable
import qualified Language.Haskell.Syntax as HS
import qualified Lucid as H
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.IO as IO
import qualified Test.Hspec as Hspec
import qualified Text.Printf as Printf

-- Test Suite -----------------------------------------------------------------

testSuite :: IO ()
testSuite = Hspec.hspec . Hspec.parallel . Hspec.describe "Scrod" $ do
  pure ()

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
        H.h1_ $ do
          H.a_ [H.href_ $ text "https://github.com/tfausak/scrod"] $ html "Scrod"

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

            H.h2_ $ html "Haddocks"
            H.ul_ $ Monad.forM_ (getHsDocStrings lHsModule) $ \hsDocString -> do
              H.li_ . html $ docHToHtml . hsDocStringToDocH $ hsDocString

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
          EnumSet.empty -- enabled extensions
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

extractHsDocStrings :: (Data.Data a) => a -> [[GHC.Hs.HsDocString]]
extractHsDocStrings = Data.gmapQ $ \d -> case Data.cast d of
  Nothing -> concat $ extractHsDocStrings d
  Just hds -> [hds]

getHsDocStrings :: LHsModule GHC.Hs.GhcPs -> [GHC.Hs.HsDocString]
getHsDocStrings = concat . extractHsDocStrings . unwrapLHsModule

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
      Haddock.markupTable = error "impossible: markupTable",
      Haddock.markupUnorderedList = H.ul_ . foldMap H.li_,
      Haddock.markupWarning = error "impossible: markupWarning"
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
            . ( case Data.cast t of
                  Nothing -> foldr (.) id $ Data.gmapQ ((showChar ' ' .) . dataShowS) t
                  Just hdsc -> showString . show $ GHC.Hs.renderHsDocString hdsc
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
