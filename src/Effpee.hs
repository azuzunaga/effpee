module Effpee
  ( todo
  , Bool (..)
  , (&&)
  , (||)
  , Generic
  , Num (..)
  , Show (..)
  , Integer
  , Int
  , ($)
  , (<$>)
  , (<*>)
  , (<)
  , (<<<)
  , (>>>)
  , id
  , Eq (..)
  , Enum (..)
  , Bounded (..)
  , TextShow (..)
  , fromString
  , length
  , toString
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Category    ((<<<), (>>>))
import Data.Bool           (Bool (..), (&&), (||))
import Data.Eq             (Eq (..))
import Data.Function       (id, ($))
import Data.Int            (Int)
import Data.List           (length)
import Data.Monoid         (mconcat)
import Data.Ord            ((<))
import Data.String         (String)
import Data.Text           (Text, unpack)
import GHC.Enum            (Bounded (..), Enum (..))
import GHC.Err             (error)
import GHC.Generics        (Generic)
import GHC.Num             (Integer, Num (..))
import GHC.Show            (Show (..))
import TextShow            (TextShow (..), fromString, toString)

todo :: String -> a
todo s = error (mconcat ["TODO ", s])
