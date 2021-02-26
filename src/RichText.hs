{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RichText (RichText(..), LinkTarget(..), space, link, icon) where 

import qualified Agda.Syntax.Position as Agda
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON))

import Data.Sequence (Seq(..)  )
import qualified Data.Sequence as Seq

-- for instance ToJSON Agda.Range 
import Agda.IR ()


--------------------------------------------------------------------------------

newtype RichText = RichText (Seq Element)

-- Represent RichText with String literals
instance IsString RichText where 
  fromString s = RichText (Seq.singleton (Elem " " mempty))
 
instance Semigroup RichText where 
  RichText xs <> RichText ys = RichText (merge xs ys)
    where 
      mergeOne :: Seq Element -> Element -> Seq Element
      mergeOne (xs :|> Elem x a) (Elem y b) = 
        if a == b then xs :|> Elem (x <> y) a else xs :|> Elem x a :|> Elem y b

      merge :: Seq Element -> Seq Element -> Seq Element
      merge xs Seq.Empty = xs
      merge xs (y :<| ys) = merge (mergeOne xs y) ys

instance Monoid RichText where 
  mempty = RichText mempty 

-- | Whitespace  
space :: RichText 
space = " " 

link :: LinkTarget -> RichText -> RichText
link l (RichText xs) = RichText $ fmap (overwriteLink l) xs

icon :: String -> RichText
icon s = RichText $ Seq.singleton $ Elem mempty (mempty { attrIcon = Just s })

--------------------------------------------------------------------------------

data LinkTarget 
  -- | Pointing to some hole 
  = Hole Int 
  -- | Pointing to some places in a file
  | SrcLoc Agda.Range 
  deriving (Generic, Eq)

instance ToJSON LinkTarget

--------------------------------------------------------------------------------

data Attributes = Attributes 
  { attrLink :: Maybe LinkTarget
  , attrIcon :: Maybe String 
  } deriving (Generic, Eq)

instance ToJSON Attributes

-- | Merging Attributes with (<>)
instance Semigroup Attributes where 
  Attributes a1 b1 <> Attributes a2 b2 = Attributes (a1 <+> a2) (b1 <+> b2) 
    where 
      (<+>) :: Maybe a -> Maybe a -> Maybe a
      (<+>) (Just x) (Just _) = Just x
      (<+>) Nothing x = x
      (<+>) x Nothing = x

instance Monoid Attributes where 
  mempty = Attributes Nothing Nothing

overwriteLink :: LinkTarget -> Element -> Element
overwriteLink target (Elem s attr) = Elem s (attr { attrLink = Just target })

--------------------------------------------------------------------------------

-- | Internal type, to be converted to JSON values
data Element = Elem String Attributes
  deriving (Generic)

instance ToJSON Element
