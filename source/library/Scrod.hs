{-# LANGUAGE FlexibleInstances #-}

module Scrod where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
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

defaultMain :: IO ()
defaultMain = do
  contents <- getContents
  lHsModule <- case parseLHsModule "<interactive>" contents of
    Left messages -> fail $ show messages
    Right lHsModule -> pure lHsModule
  LazyByteString.putStr . H.renderBS . H.doctypehtml_ $ do
    H.head_ $ do
      H.meta_ [H.charset_ $ Text.pack "utf-8"]
      H.title_ $ H.toHtml "Scrod"
      H.style_ $ Text.pack "pre { white-space: pre-wrap; }"
    H.body_ $ do
      H.h1_ $ H.toHtml "Scrod"

      H.h2_ [H.name_ $ Text.pack "input"] $ H.toHtml "Input"
      H.details_ $ do
        H.summary_ $ H.toHtml "Click to expand."
        H.pre_ . H.code_ $ H.toHtml contents

      H.h2_ [H.name_ $ Text.pack "parsed"] $ H.toHtml "Parsed"
      H.details_ $ do
        H.summary_ $ H.toHtml "Click to expand."
        H.pre_ . H.code_ . H.toHtml $ show lHsModule

      H.h2_ [H.name_ $ Text.pack "haddocks"] $ H.toHtml "Haddocks"
      H.ul_ $ Monad.forM_ (getHsDocStrings lHsModule) $ \hsDocString -> do
        H.li_ . H.toHtml $ renderHaddock . parseHsDocString $ hsDocString

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

-- | Provided for convenience in GHCi.
instance Show (LHsModule GHC.Hs.GhcPs) where
  show = ($ "") . gshows . unwrapLHsModule

-- | Wrapper to avoid orphans.
newtype Messages a = Messages
  { unwrapMessages :: ErrUtil.Messages a
  }

-- | Provided for convenience in GHCi.
instance (ErrUtil.Diagnostic a) => Show (Messages a) where
  show = Outputable.showPprUnsafe . unwrapMessages

-- | Adapted from <https://hackage.haskell.org/package/syb-0.7.2.4>.
gshows :: (Data.Data a) => a -> ShowS
gshows =
  let extQ ::
        (Data.Typeable a, Data.Typeable b) =>
        (a -> r) -> (b -> r) -> a -> r
      extQ f g a = maybe (f a) g (Data.cast a)
   in ( \t ->
          showChar '('
            . showString (Data.showConstr $ Data.toConstr t)
            . ( case Data.cast t of
                  Nothing -> foldr (.) id $ Data.gmapQ ((showChar ' ' .) . gshows) t
                  Just hdsc -> showString . show $ GHC.Hs.renderHsDocString hdsc
              )
            . showChar ')'
      )
        `extQ` (shows :: String -> ShowS)

-- TODO: Also handle @EpaDocOptions@.

extractHsDocStrings :: (Data.Data a) => a -> [[GHC.Hs.HsDocString]]
extractHsDocStrings = Data.gmapQ $ \d -> case Data.cast d of
  Nothing -> concat $ extractHsDocStrings d
  Just hds -> [hds]

getHsDocStrings :: LHsModule GHC.Hs.GhcPs -> [GHC.Hs.HsDocString]
getHsDocStrings = concat . extractHsDocStrings . unwrapLHsModule

parseHsDocString :: GHC.Hs.HsDocString -> Haddock.DocH Void.Void (Haddock.Namespace, String)
parseHsDocString =
  Haddock.overIdentifier (curry Just)
    . Haddock._doc
    . Haddock.parseParas Nothing
    . GHC.Hs.renderHsDocString

renderHaddock :: Haddock.DocH Void.Void (Haddock.Namespace, String) -> H.Html ()
renderHaddock = Haddock.markup htmlMarkup

htmlMarkup :: Haddock.DocMarkupH Void.Void (Haddock.Namespace, String) (H.Html ())
htmlMarkup =
  Haddock.Markup
    { Haddock.markupAName = \x -> H.a_ [H.name_ $ Text.pack x] mempty,
      Haddock.markupAppend = mappend,
      Haddock.markupBold = H.strong_,
      Haddock.markupCodeBlock = H.pre_ . H.code_,
      Haddock.markupDefList = H.dl_ . foldMap (\(t, d) -> H.dt_ t <> H.dd_ d),
      Haddock.markupEmphasis = H.em_,
      Haddock.markupEmpty = mempty,
      Haddock.markupExample = foldMap $ \x -> H.pre_ . H.code_ . H.toHtml . List.intercalate "\n" $ (">>> " <> Haddock.exampleExpression x) : Haddock.exampleResult x,
      Haddock.markupHeader = \x -> H.term (Text.pack $ "h" <> show (Haddock.headerLevel x)) $ Haddock.headerTitle x,
      Haddock.markupHyperlink = \x -> let url = Text.pack $ Haddock.hyperlinkUrl x in H.a_ [H.href_ url, H.rel_ $ Text.pack "nofollow"] . Maybe.fromMaybe (H.toHtml url) $ Haddock.hyperlinkLabel x,
      Haddock.markupIdentifier = H.code_ . H.toHtml . snd,
      Haddock.markupIdentifierUnchecked = Void.absurd,
      Haddock.markupMathDisplay = \x -> H.div_ . H.toHtml $ "\\[" <> x <> "\\]",
      Haddock.markupMathInline = \x -> H.span_ . H.toHtml $ "\\(" <> x <> "\\)",
      Haddock.markupModule = \x -> H.code_ . Maybe.fromMaybe (H.toHtml $ Haddock.modLinkName x) $ Haddock.modLinkLabel x,
      Haddock.markupMonospaced = H.code_,
      Haddock.markupOrderedList = H.ol_ . foldMap (\(i, x) -> H.li_ [H.value_ . Text.pack $ show i] x),
      Haddock.markupParagraph = H.p_,
      Haddock.markupPic = \x -> H.img_ [H.alt_ . maybe Text.empty Text.pack $ Haddock.pictureTitle x, H.src_ . Text.pack $ Haddock.pictureUri x],
      Haddock.markupProperty = H.pre_ . H.code_ . H.toHtml . mappend "prop> ",
      Haddock.markupString = H.toHtml,
      Haddock.markupTable = error "impossible: markupTable",
      Haddock.markupUnorderedList = H.ul_ . foldMap H.li_,
      Haddock.markupWarning = error "impossible: markupWarning"
    }
