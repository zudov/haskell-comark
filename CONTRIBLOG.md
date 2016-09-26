# [2016-08-27]
  - Worked on docs. Covered all exposed API except some of the sort-of
    internal functions in `Commonmark.Parser.*`.

# [2016-08-13]
  - Updated the spec, a new test case found me a bug in the space collapsing
	inside code spans.

	I have way too many variosly named spacy predicates/parsers/scanners.
	Those need to be reduced to a smaller and sensible set.

# [2016-08-12]
  - Today I plan to finish revising the current 'simple' AST. I want
    to agree with myself on names, that's always useful. Along the way
	it won't hurt to finish documenting those modules.

    Later I want to try applying more elaborate and typed AST definition
	that I've sketched long time ago:

    ```haskell
    data Blocks = Blocks [Block]
    deriving instance Show Blocks

    -- | Typeclasses
    class Show a => IsBlock a
    class Show a => IsInline a

    -- | Blocks
    data Block = forall block. IsBlock block => Block block
    deriving instance Show Block

    data Header = forall inline. IsInline inline => Header Int [inline]
    instance IsBlock Header where
    deriving instance Show Header

    data Code = Code (Maybe Text) Text deriving Show
    instance IsBlock Code where

    data Html = Html Text deriving Show
    instance IsBlock Html

    data Reference = forall inline. IsInline inline
                   => Reference Text (Maybe Text) [inline]
    deriving instance Show Reference
    instance IsBlock Reference

    data Paragraph = forall inline. IsInline inline
                   => Paragraph [inline]
    deriving instance Show Paragraph
    instance IsBlock Paragraph

    data Quote = forall block. IsBlock block
               => Quote [block]
    deriving instance Show Quote
    instance IsBlock Quote

    data List = forall block. IsBlock block
              => List ListType Bool Int [block]
    deriving instance Show List
    instance IsBlock List

    data ListType = Ordered Delimiter
                  | Bullet
                  deriving (Show, Eq)

    data Delimiter = Period
                   | Paren
                   deriving (Show, Eq)

    -- | Inlines
    data InlineText = InlineText Text deriving Show
    instance IsInline InlineText where


    docExample :: Blocks
    docExample =
        Blocks [ Block (Header 1 [InlineText "Some text"])
               , Block (Paragraph [InlineText "bla bla bla"])
               ]

	```

	One other idea is to plainly represent the AST with cmark-like homogenic
	nodes, while encoding all the syntactic elements and relations on the type
	level (datakinds and a bunch of type families I guess). Hey GADT's are
	promotable in ghc-8.0, I really need to build nightly resolver locally.
	Let's start with that.

	I'd want to play with this cool AST representations in separate branches
	while improving the parser in the main branch.
