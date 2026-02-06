module Scrod.TestSuite.All where

import qualified Scrod.Convert.FromHaddock
import qualified Scrod.Convert.ToHtml
import qualified Scrod.Convert.ToJson
import qualified Scrod.Css.AtRule
import qualified Scrod.Css.Block
import qualified Scrod.Css.BlockContent
import qualified Scrod.Css.Declaration
import qualified Scrod.Css.Item
import qualified Scrod.Css.Name
import qualified Scrod.Css.Rule
import qualified Scrod.Css.Selector
import qualified Scrod.Css.Stylesheet
import qualified Scrod.Decimal
import qualified Scrod.Executable.Config
import qualified Scrod.Executable.Flag
import qualified Scrod.Executable.Format
import qualified Scrod.Extra.Builder
import qualified Scrod.Extra.Either
import qualified Scrod.Extra.Maybe
import qualified Scrod.Extra.Monoid
import qualified Scrod.Extra.Ord
import qualified Scrod.Extra.Parsec
import qualified Scrod.Extra.Read
import qualified Scrod.Extra.Semigroup
import qualified Scrod.Ghc.OnOff
import qualified Scrod.Ghc.Parse
import qualified Scrod.Json.Array
import qualified Scrod.Json.Boolean
import qualified Scrod.Json.Null
import qualified Scrod.Json.Number
import qualified Scrod.Json.Object
import qualified Scrod.Json.Pair
import qualified Scrod.Json.String
import qualified Scrod.Json.Value
import qualified Scrod.JsonPointer.Evaluate
import qualified Scrod.JsonPointer.Pointer
import qualified Scrod.JsonPointer.Token
import qualified Scrod.Spec as Spec
import qualified Scrod.TestSuite.Integration
import qualified Scrod.Version
import qualified Scrod.Xml.Attribute
import qualified Scrod.Xml.Comment
import qualified Scrod.Xml.Content
import qualified Scrod.Xml.Declaration
import qualified Scrod.Xml.Document
import qualified Scrod.Xml.Element
import qualified Scrod.Xml.Instruction
import qualified Scrod.Xml.Misc
import qualified Scrod.Xml.Name
import qualified Scrod.Xml.Text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Scrod.Convert.FromHaddock.spec s
  Scrod.Convert.ToHtml.spec s
  Scrod.Convert.ToJson.spec s
  Scrod.Css.AtRule.spec s
  Scrod.Css.Block.spec s
  Scrod.Css.BlockContent.spec s
  Scrod.Css.Declaration.spec s
  Scrod.Css.Item.spec s
  Scrod.Css.Name.spec s
  Scrod.Css.Rule.spec s
  Scrod.Css.Selector.spec s
  Scrod.Css.Stylesheet.spec s
  Scrod.Decimal.spec s
  Scrod.Executable.Config.spec s
  Scrod.Executable.Format.spec s
  Scrod.Executable.Flag.spec s
  Scrod.Extra.Builder.spec s
  Scrod.Extra.Either.spec s
  Scrod.Extra.Maybe.spec s
  Scrod.Extra.Monoid.spec s
  Scrod.Extra.Ord.spec s
  Scrod.Extra.Parsec.spec s
  Scrod.Extra.Read.spec s
  Scrod.Extra.Semigroup.spec s
  Scrod.Ghc.OnOff.spec s
  Scrod.Ghc.Parse.spec s
  Scrod.Json.Array.spec s
  Scrod.Json.Boolean.spec s
  Scrod.Json.Null.spec s
  Scrod.Json.Number.spec s
  Scrod.Json.Object.spec s
  Scrod.Json.Pair.spec s
  Scrod.Json.String.spec s
  Scrod.Json.Value.spec s
  Scrod.JsonPointer.Evaluate.spec s
  Scrod.JsonPointer.Pointer.spec s
  Scrod.JsonPointer.Token.spec s
  Scrod.TestSuite.Integration.spec s
  Scrod.Version.spec s
  Scrod.Xml.Attribute.spec s
  Scrod.Xml.Comment.spec s
  Scrod.Xml.Content.spec s
  Scrod.Xml.Declaration.spec s
  Scrod.Xml.Document.spec s
  Scrod.Xml.Element.spec s
  Scrod.Xml.Instruction.spec s
  Scrod.Xml.Misc.spec s
  Scrod.Xml.Name.spec s
  Scrod.Xml.Text.spec s
