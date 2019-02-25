{-# OPTIONS_GHC -Wall #-}

module SemanticVersioningParser where
  
import Control.Applicative
import Text.Trifecta
  
data NumberOrString = NOSS String | NOSI Integer
  deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)
  
instance Ord SemVer where
  compare 
    (SemVer maj1 min1 pat1 rel1 _)
    (SemVer maj2 min2 pat2 rel2 _)
      = let maj = compare maj1 maj2
            mno = compare min1 min2
            pat = compare pat1 pat2
            rel = compare rel1 rel2
        in if maj /= EQ then maj else
           if mno /= EQ then mno else
           if pat /= EQ then pat else rel

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  _ <- try $ char '-'
  rel <- try relParser
  return $ SemVer major minor patch rel []
  

relParser :: Parser Release
relParser = some $ skipMany (oneOf ".") >> 
  (NOSS <$> try (some letter)) <|> 
  (NOSI <$> try integer)
  

-- TODO: 
-- * negative numbers
-- * fix - error
-- * metadata