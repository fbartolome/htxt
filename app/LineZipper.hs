module LineZipper
  ( LineZipper (..)
  , insert
  , delete
  , moveLeft
  , moveRight
  , moveToStart
  , moveToEnd
  , selectLeft
  , selectRight
  , toList
  ) where

import           Prelude hiding (Left, Right)

data Direction = Left
               | Right
               deriving (Show, Eq)

data Selection a = Selection [a] Direction deriving (Show, Eq)

data LineZipper a = LineZipper
                    { left :: [a]
                    , right :: [a]
                    , selection :: Maybe (Selection a)
                    , whenSelected :: (a -> a)
                    , whenUnselected :: (a -> a)
                    }

insert :: a -> LineZipper a -> LineZipper a
insert e (LineZipper ls rs _ ws wu) = LineZipper (e:ls) rs Nothing ws wu

delete :: LineZipper a -> LineZipper a
delete (LineZipper (e:ls) rs Nothing ws wu) = LineZipper ls rs Nothing ws wu
delete (LineZipper ls rs (Just s) ws wu)    = LineZipper ls rs Nothing ws wu
delete lz                                   = lz

moveLeft :: LineZipper a -> LineZipper a
moveLeft (LineZipper (e:ls) rs Nothing ws wu)             = LineZipper ls (e:rs) Nothing ws wu
moveLeft (LineZipper ls rs (Just (Selection ss d)) ws wu) = moveToSelectionStart $ LineZipper ls rs (Just (Selection ss d)) ws wu
moveLeft lz                                               = lz

moveRight :: LineZipper a -> LineZipper a
moveRight (LineZipper ls (e:rs) Nothing ws wu)             = LineZipper (e:ls) rs Nothing ws wu
moveRight (LineZipper ls rs (Just (Selection ss d)) ws wu) = moveToSelectionEnd $ LineZipper ls rs (Just (Selection ss d)) ws wu
moveRight lz                                               = lz

-- TODO: probar si anda moveToStart y moveToEnd xq en mi terminal no me agarra el CTRL
moveToStart :: LineZipper a -> LineZipper a
moveToStart lz = lz' {left = [], right = reverse (left lz') ++ right lz'} where
  lz' = moveToSelectionStart lz

moveToEnd :: LineZipper a -> LineZipper a
moveToEnd lz = lz' {left = reverse (right lz') ++ left lz', right = []} where
  lz' = moveToSelectionEnd lz

selectLeft :: LineZipper a -> LineZipper a
selectLeft (LineZipper (l:ls) rs Nothing ws wu)                     = LineZipper ls rs (Just (Selection [ws l] Left)) ws wu
selectLeft (LineZipper (l:ls) rs (Just (Selection ss Left)) ws wu)  = LineZipper ls rs (Just (Selection ((ws l):ss) Left)) ws wu
selectLeft (LineZipper ls rs (Just (Selection (s:[]) Right)) ws wu) = LineZipper ls (wu s:rs) Nothing ws wu
selectLeft (LineZipper ls rs (Just (Selection (s:ss) Right)) ws wu) = LineZipper ls (wu s:rs) (Just (Selection ss Right)) ws wu
selectLeft lz                                                       = lz

selectRight :: LineZipper a -> LineZipper a
selectRight (LineZipper ls (r:rs) Nothing ws wu)                     = LineZipper ls rs (Just (Selection [ws r] Right)) ws wu
selectRight (LineZipper ls (r:rs) (Just (Selection ss Right)) ws wu) = LineZipper ls rs (Just (Selection ((ws r):ss) Right)) ws wu
selectRight (LineZipper ls rs (Just (Selection (s:[]) Left)) ws wu)  = LineZipper (wu s:ls) rs Nothing ws wu
selectRight (LineZipper ls rs (Just (Selection (s:ss) Left)) ws wu)  = LineZipper (wu s:ls) rs (Just (Selection ss Left)) ws wu
selectRight lz                                                       = lz

toList :: LineZipper a -> [a]
toList (LineZipper ls rs Nothing ws wu)                     = reverse ls ++ rs
toList (LineZipper ls rs (Just (Selection ss Left)) ws wu)  = reverse ls ++ ss ++ rs
toList (LineZipper ls rs (Just (Selection ss Right)) ws wu) = reverse (ss ++ ls) ++ rs

-- Private

moveToSelectionStart :: LineZipper a -> LineZipper a
moveToSelectionStart (LineZipper ls rs (Just (Selection ss Left)) ws wu)  = LineZipper ls (map wu ss ++ rs) Nothing ws wu
moveToSelectionStart (LineZipper ls rs (Just (Selection ss Right)) ws wu) = LineZipper ls (reverse (map wu ss) ++ rs) Nothing ws wu
moveToSelectionStart lz                                                   = lz

moveToSelectionEnd :: LineZipper a -> LineZipper a
moveToSelectionEnd (LineZipper ls rs (Just (Selection ss Left)) ws wu)  = LineZipper (reverse (map wu ss) ++ ls) rs Nothing ws wu
moveToSelectionEnd (LineZipper ls rs (Just (Selection ss Right)) ws wu) = LineZipper (map wu ss ++ ls) rs Nothing ws wu
moveToSelectionEnd lz                                                   = lz
