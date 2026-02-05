module LegendaryChainsaw.TestSuite where

import qualified LegendaryChainsaw.Convert.FromGhc
import qualified LegendaryChainsaw.Convert.FromHaddock
import qualified LegendaryChainsaw.Convert.ToHtml
import qualified LegendaryChainsaw.Css.AtRule
import qualified LegendaryChainsaw.Css.Block
import qualified LegendaryChainsaw.Css.BlockContent
import qualified LegendaryChainsaw.Css.Declaration
import qualified LegendaryChainsaw.Css.Item
import qualified LegendaryChainsaw.Css.Name
import qualified LegendaryChainsaw.Css.Rule
import qualified LegendaryChainsaw.Css.Selector
import qualified LegendaryChainsaw.Css.Stylesheet
import qualified LegendaryChainsaw.Decimal
import qualified LegendaryChainsaw.Executable.Config
import qualified LegendaryChainsaw.Executable.Flag
import qualified LegendaryChainsaw.Executable.Format
import qualified LegendaryChainsaw.Extra.Builder
import qualified LegendaryChainsaw.Extra.Either
import qualified LegendaryChainsaw.Extra.Maybe
import qualified LegendaryChainsaw.Extra.Monoid
import qualified LegendaryChainsaw.Extra.Ord
import qualified LegendaryChainsaw.Extra.Parsec
import qualified LegendaryChainsaw.Extra.Read
import qualified LegendaryChainsaw.Extra.Semigroup
import qualified LegendaryChainsaw.Ghc.OnOff
import qualified LegendaryChainsaw.Ghc.Parse
import qualified LegendaryChainsaw.Json.Array
import qualified LegendaryChainsaw.Json.Boolean
import qualified LegendaryChainsaw.Json.Null
import qualified LegendaryChainsaw.Json.Number
import qualified LegendaryChainsaw.Json.Object
import qualified LegendaryChainsaw.Json.Pair
import qualified LegendaryChainsaw.Json.String
import qualified LegendaryChainsaw.Json.Value
import qualified LegendaryChainsaw.JsonPointer.Evaluate
import qualified LegendaryChainsaw.JsonPointer.Pointer
import qualified LegendaryChainsaw.JsonPointer.Token
import qualified LegendaryChainsaw.Spec as Spec
import qualified LegendaryChainsaw.Version
import qualified LegendaryChainsaw.Xml.Attribute
import qualified LegendaryChainsaw.Xml.Comment
import qualified LegendaryChainsaw.Xml.Content
import qualified LegendaryChainsaw.Xml.Declaration
import qualified LegendaryChainsaw.Xml.Document
import qualified LegendaryChainsaw.Xml.Element
import qualified LegendaryChainsaw.Xml.Instruction
import qualified LegendaryChainsaw.Xml.Misc
import qualified LegendaryChainsaw.Xml.Name
import qualified LegendaryChainsaw.Xml.Text

testSuite :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
testSuite s = do
  LegendaryChainsaw.Convert.FromGhc.spec s
  LegendaryChainsaw.Convert.FromHaddock.spec s
  LegendaryChainsaw.Convert.ToHtml.spec s
  LegendaryChainsaw.Css.AtRule.spec s
  LegendaryChainsaw.Css.Block.spec s
  LegendaryChainsaw.Css.BlockContent.spec s
  LegendaryChainsaw.Css.Declaration.spec s
  LegendaryChainsaw.Css.Item.spec s
  LegendaryChainsaw.Css.Name.spec s
  LegendaryChainsaw.Css.Rule.spec s
  LegendaryChainsaw.Css.Selector.spec s
  LegendaryChainsaw.Css.Stylesheet.spec s
  LegendaryChainsaw.Decimal.spec s
  LegendaryChainsaw.Executable.Config.spec s
  LegendaryChainsaw.Executable.Format.spec s
  LegendaryChainsaw.Executable.Flag.spec s
  LegendaryChainsaw.Extra.Builder.spec s
  LegendaryChainsaw.Extra.Either.spec s
  LegendaryChainsaw.Extra.Maybe.spec s
  LegendaryChainsaw.Extra.Monoid.spec s
  LegendaryChainsaw.Extra.Ord.spec s
  LegendaryChainsaw.Extra.Parsec.spec s
  LegendaryChainsaw.Extra.Read.spec s
  LegendaryChainsaw.Extra.Semigroup.spec s
  LegendaryChainsaw.Ghc.OnOff.spec s
  LegendaryChainsaw.Ghc.Parse.spec s
  LegendaryChainsaw.Json.Array.spec s
  LegendaryChainsaw.Json.Boolean.spec s
  LegendaryChainsaw.Json.Null.spec s
  LegendaryChainsaw.Json.Number.spec s
  LegendaryChainsaw.Json.Object.spec s
  LegendaryChainsaw.Json.Pair.spec s
  LegendaryChainsaw.Json.String.spec s
  LegendaryChainsaw.Json.Value.spec s
  LegendaryChainsaw.JsonPointer.Evaluate.spec s
  LegendaryChainsaw.JsonPointer.Pointer.spec s
  LegendaryChainsaw.JsonPointer.Token.spec s
  LegendaryChainsaw.Version.spec s
  LegendaryChainsaw.Xml.Attribute.spec s
  LegendaryChainsaw.Xml.Comment.spec s
  LegendaryChainsaw.Xml.Content.spec s
  LegendaryChainsaw.Xml.Declaration.spec s
  LegendaryChainsaw.Xml.Document.spec s
  LegendaryChainsaw.Xml.Element.spec s
  LegendaryChainsaw.Xml.Instruction.spec s
  LegendaryChainsaw.Xml.Misc.spec s
  LegendaryChainsaw.Xml.Name.spec s
  LegendaryChainsaw.Xml.Text.spec s
