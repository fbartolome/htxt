module Data.Cursor
  ( Cursor (..)
  , Direction (..)
  , Selection (..)
  , Position
  , empty
  , newCursor
  , insert
  , insertLine
  , deleteLeft
  , deleteRight
  , moveLinesWithSelectionUp
  , moveLinesWithSelectionDown
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , moveToLineStart
  , moveToLineEnd
  , moveToScreenTop
  , moveToScreenBottom
  , moveToScreenStart
  , moveToScreenEnd
  , moveToPosition
  , selectLeft
  , selectRight
  , mapSelected
  , mapUnselected
  , searchAndReplace
  , replace
  , getCurrentPosition
  , getLines
  , (-:)
  ) where

import           Prelude hiding (Left, Right)
import qualified Data.List.Utils as DLU
import           Data.List.Split

data Direction = Left
               | Right
               deriving (Show)

type Position = (Int,Int)

data Selection a = SL -- Single line
                   { line      :: [a]
                   , direction :: Direction
                   }
                 | ML -- Multi line
                   { firstLine    :: [a]
                   , linesBetween :: [[a]]
                   , lastLine     :: [a]
                   , direction    :: Direction
                   }
                 deriving (Show)

data Cursor a = Cursor
                { left      :: [a]
                , right     :: [a]
                , up        :: [[a]]
                , down      :: [[a]]
                , selection :: Maybe (Selection a)
                }
              deriving (Show)

empty :: Cursor a
empty = Cursor [] [] [] [] Nothing

newCursor :: [[a]] -> Cursor a
newCursor []     = empty
newCursor (e:es) = Cursor [] e [] es Nothing

-- Content

insert :: a -> Cursor a -> Cursor a
insert e (Cursor ls rs us ds _)  = Cursor (e:ls) rs us ds Nothing

insertLine :: Cursor a -> Cursor a
insertLine (Cursor ls rs us ds _)  = Cursor [] rs (ls:us) ds Nothing

deleteLeft :: Cursor a -> Cursor a
deleteLeft (Cursor (l:ls) rs us ds Nothing) = Cursor ls rs us ds Nothing
deleteLeft (Cursor [] rs (u:us) ds Nothing) = Cursor u rs us ds Nothing
deleteLeft (Cursor ls rs us ds (Just s))    = Cursor ls rs us ds Nothing
deleteLeft c                                = c

deleteRight :: Cursor a -> Cursor a
deleteRight (Cursor ls (r:rs) us ds Nothing) = Cursor ls rs us ds Nothing
deleteRight (Cursor ls [] us (d:ds) Nothing) = Cursor ls d us ds Nothing
deleteRight (Cursor ls rs us ds (Just s))    = Cursor ls rs us ds Nothing
deleteRight c                                = c

moveLinesWithSelectionUp :: Cursor a -> Cursor a
moveLinesWithSelectionUp (Cursor ls rs (u:us) ds s) = Cursor ls rs us ((reverse u):ds) s
moveLinesWithSelectionUp c                          = c

moveLinesWithSelectionDown :: Cursor a -> Cursor a
moveLinesWithSelectionDown (Cursor ls rs us (d:ds) s) = Cursor ls rs ((reverse d):us) ds s
moveLinesWithSelectionDown c                          = c

-- Navigation

moveLeft :: Cursor a -> Cursor a
moveLeft (Cursor [] rs (u:us) ds Nothing) = Cursor u [] us (rs:ds) Nothing
moveLeft (Cursor (l:ls) rs us ds Nothing) = Cursor ls (l:rs) us ds Nothing
moveLeft (Cursor ls rs us ds (Just s))    = moveToSelectionStart (Cursor ls rs us ds (Just s))
moveLeft c                                = c

moveRight :: Cursor a -> Cursor a
moveRight (Cursor ls [] us (d:ds) Nothing) = Cursor [] d (ls:us) ds Nothing
moveRight (Cursor ls (r:rs) us ds Nothing) = Cursor (r:ls) rs us ds Nothing
moveRight (Cursor ls rs us ds (Just s))    = moveToSelectionEnd (Cursor ls rs us ds (Just s))
moveRight c                                = c

moveUp :: Cursor a -> Cursor a
moveUp (Cursor ls rs [] ds s)           = moveToLineStart (Cursor ls rs [] ds s)
moveUp (Cursor ls rs (u:us) ds Nothing) = Cursor firstPart (reverse lastPart) us ((reverse ls ++ rs):ds) Nothing where
  (lastPart, firstPart) = splitAt (length u - length ls) u
moveUp (Cursor ls rs us ds (Just s))    = moveUp $ moveToSelectionStart (Cursor ls rs us ds (Just s))

moveDown :: Cursor a -> Cursor a
moveDown (Cursor ls rs us [] Nothing)     = moveToLineEnd (Cursor ls rs us [] Nothing)
moveDown (Cursor ls rs us (d:ds) Nothing) = Cursor (reverse firstPart) lastPart ((reverse rs ++ ls):us) ds Nothing where
  (firstPart, lastPart) = splitAt (length ls) d
moveDown (Cursor ls rs us ds (Just s))    = moveDown $ moveToSelectionEnd (Cursor ls rs us ds (Just s))

moveToLineStart :: Cursor a -> Cursor a
moveToLineStart (Cursor ls rs us ds Nothing) = Cursor [] (reverse ls ++ rs) us ds Nothing
moveToLineStart c                            = moveToLineStart $ moveToSelectionStart c

moveToLineEnd :: Cursor a -> Cursor a
moveToLineEnd (Cursor ls rs us ds Nothing) = Cursor (reverse rs ++ ls) [] us ds Nothing
moveToLineEnd c                            = moveToLineEnd $ moveToSelectionEnd c

moveToScreenTop :: Cursor a -> Cursor a
moveToScreenTop (Cursor ls rs [] ds Nothing)  = Cursor ls rs [] ds Nothing
moveToScreenTop (Cursor ls rs us ds (Just s)) = moveToLineStart $ moveToSelectionStart (Cursor ls rs us ds (Just s))
moveToScreenTop c                             = moveToScreenTop $ moveUp c

moveToScreenBottom :: Cursor a -> Cursor a
moveToScreenBottom (Cursor ls rs us [] Nothing)  = Cursor ls rs us [] Nothing
moveToScreenBottom (Cursor ls rs us ds (Just s)) = moveToLineEnd $ moveToSelectionEnd (Cursor ls rs us ds (Just s))
moveToScreenBottom c                             = moveToScreenBottom $ moveDown c

moveToScreenStart :: Cursor a -> Cursor a
moveToScreenStart c = moveToScreenTop $ moveToLineStart c

moveToScreenEnd :: Cursor a -> Cursor a
moveToScreenEnd c = moveToScreenBottom $ moveToLineEnd c

moveToPosition :: Cursor a -> Position -> Cursor a
moveToPosition c (rows,cols) = moveToPosition' c' (rows',cols')
  where
    c'    = moveToScreenStart c
    rows' = min rows (length $ down c')
    cols'
      | rows' == 0 = min cols (length $ right c')
      | otherwise = min cols (length $ (down c') !! (rows' - 1))

-- private
moveToPosition' :: Cursor a -> Position -> Cursor a
moveToPosition' c (0,0) = c
moveToPosition' c (0, cols) = moveToPosition' (moveRight c) (0, cols - 1)
moveToPosition' c (rows, cols) = moveToPosition' (moveDown c) (rows - 1, cols)
  -- | rows > (length ds) = moveToPosition' (Cursor ls rs us ds Nothing) (0, cols)


-- Selection

-- TODO ver si se puede sacar Maybe y reemplazarlo por SL vacio
selectLeft :: Cursor a -> Cursor a
selectLeft (Cursor [] rs (u:us) ds Nothing)                        = Cursor u rs us ds (Just (ML [] [] [] Left))
selectLeft (Cursor (l:ls) rs us ds Nothing)                        = Cursor ls rs us ds (Just (SL [l] Left))
selectLeft (Cursor [] rs (u:us) ds (Just (SL ss Left)))            = Cursor u rs us ds (Just (ML [] [] ss Left))
selectLeft (Cursor (l:ls) rs us ds (Just (SL ss Left)))            = Cursor ls rs us ds (Just (SL (l:ss) Left))
selectLeft (Cursor [] rs (u:us) ds (Just (ML sus sls sds Left)))   = Cursor u rs us ds (Just (ML [] (sus:sls) sds Left))
selectLeft (Cursor (l:ls) rs us ds (Just (ML sus sls sds Left)))   = Cursor ls rs us ds (Just (ML (l:sus) sls sds Left))
selectLeft (Cursor ls rs us ds (Just (SL [s] Right)))              = Cursor ls (s:rs) us ds Nothing
selectLeft (Cursor ls rs us ds (Just (SL (s:ss) Right)))           = Cursor ls (s:rs) us ds (Just (SL ss Right))
selectLeft (Cursor ls rs (u:us) ds (Just (ML [] [] [] Right)))     = Cursor u [] us (rs:ds) Nothing
selectLeft (Cursor ls rs us ds (Just (ML sus [] [] Right)))        = Cursor ls [] us (rs:ds) (Just (SL sus Right))
selectLeft (Cursor ls rs us ds (Just (ML sus (sl:sls) [] Right)))  = Cursor ls [] us (rs:ds) (Just (ML sus [] sl Right))
selectLeft (Cursor ls rs us ds (Just (ML sus sls (sd:sds) Right))) = Cursor ls (sd:rs) us ds (Just (ML sus sls sds Right))
selectLeft c                                                       = c

selectRight :: Cursor a -> Cursor a
selectRight (Cursor ls [] us (d:ds) Nothing)                       = Cursor ls d us ds (Just (ML [] [] [] Right))
selectRight (Cursor ls (r:rs) us ds Nothing)                       = Cursor ls rs us ds (Just (SL [r] Right))
selectRight (Cursor ls [] us (d:ds) (Just (SL ss Right)))          = Cursor ls d us ds (Just (ML ss [] [] Right))
selectRight (Cursor ls (r:rs) us ds (Just (SL ss Right)))          = Cursor ls rs us ds (Just (SL (r:ss) Right))
selectRight (Cursor ls [] us (d:ds) (Just (ML sus sls sds Right))) = Cursor ls d us ds (Just (ML sus (sds:sls) [] Right))
selectRight (Cursor ls (r:rs) us ds (Just (ML sus sls sds Right))) = Cursor ls rs us ds (Just (ML sus sls (r:sds) Right))
selectRight (Cursor ls rs us ds (Just (SL [s] Left)))              = Cursor (s:ls) rs us ds Nothing
selectRight (Cursor ls rs us ds (Just (SL (s:ss) Left)))           = Cursor (s:ls) rs us ds (Just (SL ss Left))
selectRight (Cursor ls rs us (d:ds) (Just (ML [] [] [] Left)))     = Cursor [] d (ls:us) ds Nothing
selectRight (Cursor ls rs us ds (Just (ML [] [] sds Left)))        = Cursor [] rs (ls:us) ds (Just (SL sds Left))
selectRight (Cursor ls rs us ds (Just (ML [] (sl:sls) sds Left)))  = Cursor [] rs (ls:us) ds (Just (ML sl sls sds Left))
selectRight (Cursor ls rs us ds (Just (ML (su:sus) sls sds Left))) = Cursor (su:ls) rs us ds (Just (ML sus sls sds Left))
selectRight c                                                      = c

-- selectAll :: Cursor a -> Cursor a
-- selectAll (Cursor ls rs [] [] Nothing) = Cursor [] [] [] [] (Just (SL (reverse ls ++ rs) Right))
-- selectAll (Cursor ls [] us [] Nothing) = Cursor [] [] [] [] (Just (ML _ ls _ Right))
-- selectAll (Cursor ls rs us ds Nothing) = selectAll $ moveToScreenEnd

mapSelected :: (a -> a) -> Cursor a -> Cursor a
mapSelected f (Cursor ls rs us ds (Just (SL ss d)))          = Cursor ls rs us ds (Just (SL (map f ss) d))
mapSelected f (Cursor ls rs us ds (Just (ML sus sls sds d))) = Cursor ls rs us ds (Just (ML (map f sus) (map (map f) sls) (map f sds) d))
mapSelected f c                                              = c

mapUnselected :: (a -> a) -> Cursor a -> Cursor a
mapUnselected f (Cursor ls rs us ds s) = Cursor (map f ls) (map f rs) (map (map f) us) (map (map f) ds) s

-- Replace

searchAndReplace :: Eq a => [a] -> [a] -> Cursor a -> (Cursor a, [Position])
searchAndReplace [] new c  = (c,[])
searchAndReplace old new c = (moveToPosition c' p, ps)
  where
    p = getCurrentPosition c
    (c', ps) = searchAndReplaceFromActualPosition old new $ (moveToScreenStart c,[])

-- assumes that selection is Nothing
-- private
searchAndReplaceFromActualPosition :: Eq a => [a] -> [a] -> (Cursor a, [Position]) -> (Cursor a, [Position])
searchAndReplaceFromActualPosition _ _ (Cursor l [] u [] s, p) = (Cursor l [] u [] s, p)
searchAndReplaceFromActualPosition old new (c, ps) = searchAndReplaceFromActualPosition old new (moveDown $ c {right = replaced}, positions)
  where split'    = splitOn old $ right c
        replaced  = DLU.replace old new $ right c
        positions = calculatePositions split' ps 0
        calculatePositions (x:[]) ps _ = ps
        calculatePositions [] ps _     = ps
        calculatePositions (x:xs) ps a = calculatePositions xs (ps ++ [(length $ up c, a + (length x))]) (a + (length new) + (length x))

replace :: Eq a => [a] -> [a] -> Cursor a -> Cursor a
replace [] _ c                         = c
replace old new (Cursor ls rs us ds s) = (Cursor ls' rs' us' ds' s) where
  ls' = DLU.replace (reverse old) (reverse new) ls
  rs' = DLU.replace old new rs
  us' = map (DLU.replace (reverse old) (reverse new)) us
  ds' = map (DLU.replace old new) ds

-- Properties

getCurrentPosition :: Cursor a -> Position
getCurrentPosition (Cursor ls _ us _ (Just (SL ss Right)))       = (length us, length ls + length ss)
getCurrentPosition (Cursor _ _ us _ (Just (ML _ sls sds Right))) = (length us + length sls + 1, length sds)
getCurrentPosition (Cursor ls _ us _ _)                          = (length us, length ls)

getLines :: Cursor a -> [[a]]
getLines c = restOfLines $ moveToScreenStart c

-- Testing

(-:) x f = f x -- TODO remove

-- Private

moveToSelectionStart :: Cursor a -> Cursor a
moveToSelectionStart (Cursor ls rs us ds (Just (SL ss Left)))           = Cursor ls (ss ++ rs) us ds Nothing
moveToSelectionStart (Cursor ls rs us ds (Just (SL ss Right)))          = Cursor ls ((reverse ss) ++ rs) us ds Nothing
moveToSelectionStart (Cursor ls rs us ds (Just (ML sus sls sds Left)))  = Cursor ls sus us (sls ++ (lastLineWithSelection : ds)) Nothing where
  lastLineWithSelection = sds ++ rs
moveToSelectionStart (Cursor ls rs us ds (Just (ML sus sls sds Right))) = Cursor ls (reverse sus) us ((reverse (map reverse sls)) ++ (lastLineWithSelection : ds)) Nothing where
  lastLineWithSelection = (reverse sds) ++ rs
moveToSelectionStart c                                                  = c

moveToSelectionEnd :: Cursor a -> Cursor a
moveToSelectionEnd (Cursor ls rs us ds (Just (SL ss Left)))           = Cursor ((reverse ss) ++ ls) rs us ds Nothing
moveToSelectionEnd (Cursor ls rs us ds (Just (SL ss Right)))          = Cursor (ss ++ ls) rs us ds Nothing
moveToSelectionEnd (Cursor ls rs us ds (Just (ML sus sls sds Left)))  = Cursor (reverse sds) rs ((reverse (map reverse sls)) ++ (firstLineWithSelection : us)) ds Nothing where
  firstLineWithSelection = reverse sus ++ ls
moveToSelectionEnd (Cursor ls rs us ds (Just (ML sus sls sds Right))) = Cursor sds rs (sls ++ (firstLineWithSelection : us)) ds Nothing where
  firstLineWithSelection = sus ++ ls
moveToSelectionEnd c                                                  = c

restOfLines :: Cursor a -> [[a]]
restOfLines (Cursor _ rs _ ds _) = rs:ds
