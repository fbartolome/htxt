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
  , insertBeforeSelection
  , insertAfterSelection
  , insertAtLineStart
  , removeAtLineStart
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
  , selectAll
  , mapSelected
  , mapUnselected
  , getSelectedLines
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

insertBeforeSelection :: a -> Cursor a -> Cursor a
insertBeforeSelection e (Cursor ls rs us ds s) = Cursor (e:ls) rs us ds s

insertAfterSelection :: a -> Cursor a -> Cursor a
insertAfterSelection e (Cursor ls rs us ds s) = Cursor ls (e:rs) us ds s

insertAtLineStart :: a -> Cursor a -> Cursor a
insertAtLineStart e (Cursor ls rs us ds (Just (ML fl lb ll Left))) =
  Cursor (ls ++ [e]) rs us ds (Just (ML fl (map (e:) lb) (e:ll) Left))
insertAtLineStart e (Cursor ls rs us ds (Just (ML fl lb ll Right))) =
  Cursor (ls ++ [e]) rs us ds (Just (ML fl (map (\l -> l ++ [e]) lb) (ll ++ [e]) Right))
insertAtLineStart e c = c {left = (left c) ++ [e]}

removeAtLineStart :: Eq a => a -> Cursor a -> Cursor a
removeAtLineStart e (Cursor ls rs us ds s) =
  case ls of
    [] -> case s of
            Just (ML fl lb ll Left) ->
              Cursor [] rs us ds (Just (ML (removeAtLineStart' e fl) (map (removeAtLineStart' e) lb) (removeAtLineStart' e ll) Left))
            Just (ML fl lb ll Right) ->
              Cursor [] rs us ds (Just (ML (reverseRemoveAtLineStart' e fl) (map (reverseRemoveAtLineStart' e) lb) (reverseRemoveAtLineStart' e ll) Right))
            Just (SL l Left) -> Cursor ls rs us ds (Just (SL (removeAtLineStart' e l) Left))
            Just (SL l Right) -> Cursor ls rs us ds (Just (SL (reverseRemoveAtLineStart' e l) Left))
            _ -> Cursor ls (removeAtLineStart' e rs) us ds s
    ls' -> case s of
            Just (ML fl lb ll Left) ->
              Cursor (reverseRemoveAtLineStart' e ls) rs us ds (Just (ML fl (map (removeAtLineStart' e) lb) (removeAtLineStart' e ll) Left))
            Just (ML fl lb ll Right) ->
              Cursor (reverseRemoveAtLineStart' e ls) rs us ds (Just (ML fl (map (reverseRemoveAtLineStart' e) lb) (reverseRemoveAtLineStart' e ll) Right))
            _ -> Cursor (reverseRemoveAtLineStart' e ls) rs us ds s
  where
    removeAtLineStart' a [] = []
    removeAtLineStart' a (x:xs)
      | x == a = xs
      | otherwise = (x:xs)
    reverseRemoveAtLineStart' a = reverse . ((removeAtLineStart' a) . reverse)

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

-- TODO: ver si lo puedo hacer mas eficiente
moveToPosition :: Cursor a -> Position -> Cursor a
moveToPosition c (rows,cols) = moveToPosition' c' (rows',cols')
  where
    c'    = moveToScreenStart c
    rows' = min rows (length $ down c')
    cols'
      | rows' == 0 = min cols (length $ right c')
      | otherwise = min cols (length $ (down c') !! (rows' - 1))

-- Selection

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

selectAll :: Cursor a -> Cursor a
selectAll (Cursor [] rs [] [] Nothing) = Cursor [] [] [] [] (Just (SL rs Left))
selectAll (Cursor [] rs [] ds Nothing) = Cursor [] [] [] [] (Just (ML rs body lastElem Left))
  where
    (lastElem, body) = (\(x:xs) -> (x, reverse xs)) $ reverse ds
selectAll c = selectAll $ moveToScreenStart c

mapSelected :: (a -> a) -> Cursor a -> Cursor a
mapSelected f (Cursor ls rs us ds (Just (SL ss d)))          = Cursor ls rs us ds (Just (SL (map f ss) d))
mapSelected f (Cursor ls rs us ds (Just (ML sus sls sds d))) = Cursor ls rs us ds (Just (ML (map f sus) (map (map f) sls) (map f sds) d))
mapSelected f c                                              = c

mapUnselected :: (a -> a) -> Cursor a -> Cursor a
mapUnselected f (Cursor ls rs us ds s) = Cursor (map f ls) (map f rs) (map (map f) us) (map (map f) ds) s

getSelectedLines :: Cursor a -> [[a]]
getSelectedLines c =
  case selection c of
    Just (SL ss Left)           -> [ss]
    Just (SL ss Right)          -> [reverse ss]
    Just (ML sus sls sds Left)  -> (sus : sls) ++ [sds]
    Just (ML sus sls sds Right) -> ((reverse sus) : (map reverse sls)) ++ [sds]
    _                           -> []

-- Replace

searchAndReplace :: Eq a => [a] -> [a] -> Cursor a -> (Cursor a, [Position])
searchAndReplace [] new c  = (c,[])
searchAndReplace old new c = (moveToPosition c' p, ps)
  where
    p = getCurrentPosition c
    (c', ps) = searchAndReplaceFromActualPosition old new $ (moveToScreenStart c,[])

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

moveToPosition' :: Cursor a -> Position -> Cursor a
moveToPosition' c (0,0) = c
moveToPosition' c (0, cols) = moveToPosition' (moveRight c) (0, cols - 1)
moveToPosition' c (rows, cols) = moveToPosition' (moveDown c) (rows - 1, cols)

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

searchAndReplaceFromActualPosition :: Eq a => [a] -> [a] -> (Cursor a, [Position]) -> (Cursor a, [Position])
searchAndReplaceFromActualPosition _ _ (Cursor l [] u [] s, p) = (Cursor l [] u [] s, p)
searchAndReplaceFromActualPosition old new (c, ps) = searchAndReplaceFromActualPosition old new (moveDown $ c {right = replaced}, positions)
  where split'    = splitOn old $ right c
        replaced  = DLU.replace old new $ right c
        positions = calculatePositions split' ps 0
        calculatePositions (x:[]) ps _ = ps
        calculatePositions [] ps _     = ps
        calculatePositions (x:xs) ps a = calculatePositions xs (ps ++ [(length $ up c, a + (length x))]) (a + (length new) + (length x))

restOfLines :: Cursor a -> [[a]]
restOfLines (Cursor _ rs _ ds _) = rs:ds
