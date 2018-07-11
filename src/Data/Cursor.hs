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
  , moveToScreenStart
  , moveToScreenEnd
  , moveToPosition
  , moveRightUntil
  , moveLeftUntil
  , selectLeft
  , selectRight
  , selectUp
  , selectDown
  , selectToLineStart
  , selectToLineEnd
  , selectAll
  , getSelectedLines
  , searchAndReplace
  , replace
  , getCurrentPosition
  , getLines
  , doRightUntil
  , doLeftUntil
  ) where

import           Prelude hiding (Left, Right)
import qualified Data.List.Utils as DLU
import           Data.List.Split

data Direction = Left
               | Right
               -- deriving (Show)

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
                 -- deriving (Show)

data Cursor a = Cursor
                { left       :: [a]
                , right      :: [a]
                , up         :: [[a]]
                , down       :: [[a]]
                , selection  :: Maybe (Selection a)
                , onSelect   :: (a -> a)
                , onUnselect :: (a -> a)
                }
              -- deriving (Show)

empty :: Cursor a
empty = Cursor [] [] [] [] Nothing id id

newCursor :: [[a]] -> (a -> a) -> (a -> a) -> Cursor a
newCursor [] os ou     = Cursor [] [] [] [] Nothing os ou
newCursor (e:es) os ou = Cursor [] e [] es Nothing os ou

-- Content

insert :: a -> Cursor a -> Cursor a
insert e (Cursor ls rs us ds _ os ou)  = Cursor (e:ls) rs us ds Nothing os ou

insertLine :: Cursor a -> Cursor a
insertLine (Cursor ls rs us ds _ os ou)  = Cursor [] rs (ls:us) ds Nothing os ou

deleteLeft :: Cursor a -> Cursor a
deleteLeft (Cursor (l:ls) rs us ds Nothing os ou) = Cursor ls rs us ds Nothing os ou
deleteLeft (Cursor [] rs (u:us) ds Nothing os ou) = Cursor u rs us ds Nothing os ou
deleteLeft (Cursor ls rs us ds (Just s) os ou)    = Cursor ls rs us ds Nothing os ou
deleteLeft c                                = c

deleteRight :: Cursor a -> Cursor a
deleteRight (Cursor ls (r:rs) us ds Nothing os ou) = Cursor ls rs us ds Nothing os ou
deleteRight (Cursor ls [] us (d:ds) Nothing os ou) = Cursor ls d us ds Nothing os ou
deleteRight (Cursor ls rs us ds (Just s) os ou)    = Cursor ls rs us ds Nothing os ou
deleteRight c                                = c

moveLinesWithSelectionUp :: Cursor a -> Cursor a
moveLinesWithSelectionUp (Cursor ls rs (u:us) ds s os ou) = Cursor ls rs us ((reverse u):ds) s os ou
moveLinesWithSelectionUp c                          = c

moveLinesWithSelectionDown :: Cursor a -> Cursor a
moveLinesWithSelectionDown (Cursor ls rs us (d:ds) s os ou) = Cursor ls rs ((reverse d):us) ds s os ou
moveLinesWithSelectionDown c                          = c

insertBeforeSelection :: a -> Cursor a -> Cursor a
insertBeforeSelection e (Cursor ls rs us ds s os ou) = Cursor (e:ls) rs us ds s os ou

insertAfterSelection :: a -> Cursor a -> Cursor a
insertAfterSelection e (Cursor ls rs us ds s os ou) = Cursor ls (e:rs) us ds s os ou

insertAtLineStart :: a -> Cursor a -> Cursor a
insertAtLineStart e (Cursor ls rs us ds (Just (ML fl lb ll Left)) os ou) =
  Cursor (ls ++ [e]) rs us ds (Just (ML fl (map (e:) lb) (e:ll) Left)) os ou
insertAtLineStart e (Cursor ls rs us ds (Just (ML fl lb ll Right)) os ou) =
  Cursor (ls ++ [e]) rs us ds (Just (ML fl (map (\l -> l ++ [e]) lb) (ll ++ [e]) Right)) os ou
insertAtLineStart e c = c {left = (left c) ++ [e]}

removeAtLineStart :: Eq a => a -> Cursor a -> Cursor a
removeAtLineStart e (Cursor ls rs us ds s os ou) =
  case ls of
    [] -> case s of
            Just (ML fl lb ll Left) ->
              Cursor [] rs us ds (Just (ML (removeAtLineStart' e fl) (map (removeAtLineStart' e) lb) (removeAtLineStart' e ll) Left)) os ou
            Just (ML fl lb ll Right) ->
              Cursor [] rs us ds (Just (ML (reverseRemoveAtLineStart' e fl) (map (reverseRemoveAtLineStart' e) lb) (reverseRemoveAtLineStart' e ll) Right)) os ou
            Just (SL l Left) -> Cursor ls rs us ds (Just (SL (removeAtLineStart' e l) Left)) os ou
            Just (SL l Right) -> Cursor ls rs us ds (Just (SL (reverseRemoveAtLineStart' e l) Left)) os ou
            _ -> Cursor ls (removeAtLineStart' e rs) us ds s os ou
    ls' -> case s of
            Just (ML fl lb ll Left) ->
              Cursor (reverseRemoveAtLineStart' e ls) rs us ds (Just (ML fl (map (removeAtLineStart' e) lb) (removeAtLineStart' e ll) Left)) os ou
            Just (ML fl lb ll Right) ->
              Cursor (reverseRemoveAtLineStart' e ls) rs us ds (Just (ML fl (map (reverseRemoveAtLineStart' e) lb) (reverseRemoveAtLineStart' e ll) Right)) os ou
            _ -> Cursor (reverseRemoveAtLineStart' e ls) rs us ds s os ou
  where
    removeAtLineStart' a [] = []
    removeAtLineStart' a (x:xs)
      | x == a = xs
      | otherwise = (x:xs)
    reverseRemoveAtLineStart' a = reverse . ((removeAtLineStart' a) . reverse)

-- Navigation

moveLeft :: Cursor a -> Cursor a
moveLeft (Cursor [] rs (u:us) ds Nothing os ou) = Cursor u [] us (rs:ds) Nothing os ou
moveLeft (Cursor (l:ls) rs us ds Nothing os ou) = Cursor ls (l:rs) us ds Nothing os ou
moveLeft (Cursor ls rs us ds (Just s) os ou)    = moveToSelectionStart (Cursor ls rs us ds (Just s) os ou)
moveLeft c                                = c

moveRight :: Cursor a -> Cursor a
moveRight (Cursor ls [] us (d:ds) Nothing os ou) = Cursor [] d (ls:us) ds Nothing os ou
moveRight (Cursor ls (r:rs) us ds Nothing os ou) = Cursor (r:ls) rs us ds Nothing os ou
moveRight (Cursor ls rs us ds (Just s) os ou)    = moveToSelectionEnd (Cursor ls rs us ds (Just s) os ou)
moveRight c                                = c

moveUp :: Cursor a -> Cursor a
moveUp (Cursor ls rs [] ds s os ou)           = moveToLineStart (Cursor ls rs [] ds s os ou)
moveUp (Cursor ls rs (u:us) ds Nothing os ou) = Cursor firstPart (reverse lastPart) us ((reverse ls ++ rs):ds) Nothing  os ou
  where
    (lastPart, firstPart) = splitAt (length u - length ls) u
moveUp (Cursor ls rs us ds (Just s) os ou)    = moveUp $ moveToSelectionStart (Cursor ls rs us ds (Just s) os ou)

moveDown :: Cursor a -> Cursor a
moveDown (Cursor ls rs us [] Nothing os ou)     = moveToLineEnd (Cursor ls rs us [] Nothing os ou)
moveDown (Cursor ls rs us (d:ds) Nothing os ou) = Cursor (reverse firstPart) lastPart ((reverse rs ++ ls):us) ds Nothing  os ou
  where
    (firstPart, lastPart) = splitAt (length ls) d
moveDown (Cursor ls rs us ds (Just s) os ou)    = moveDown $ moveToSelectionEnd (Cursor ls rs us ds (Just s) os ou)

moveLeftUntil :: (a -> Bool) -> Bool -> Cursor a -> Cursor a
moveLeftUntil eq b c = doLeftUntil eq b moveLeft c

moveRightUntil :: (a -> Bool) -> Bool -> Cursor a -> Cursor a
moveRightUntil eq b c = doRightUntil eq b moveRight c

moveToLineStart :: Cursor a -> Cursor a
moveToLineStart c = moveLeftUntil (\_ -> False) True c

moveToLineEnd :: Cursor a -> Cursor a
moveToLineEnd c = moveRightUntil (\_ -> False) True c

moveToScreenStart :: Cursor a -> Cursor a
moveToScreenStart c = moveLeftUntil (\_ -> False) False c

moveToScreenEnd :: Cursor a -> Cursor a
moveToScreenEnd c = moveRightUntil (\_ -> False) False c

moveToPosition ::  Position -> Cursor a -> Cursor a
moveToPosition (row, col) c
  | row < currRow = moveToPosition (row, col) $ moveUp c
  | row > currRow = moveToPosition (row, col) $ moveDown c
  | col < currCol = moveToPosition (row, col) $ moveLeft c
  | col > currCol = moveToPosition (row, col) $ moveRight c
  | otherwise = c
  where
    (currRow, currCol) = getCurrentPosition c

-- Selection

selectLeft :: Cursor a -> Cursor a
selectLeft (Cursor [] rs (u:us) ds Nothing os ou)                        = Cursor u rs us ds (Just (ML [] [] [] Left)) os ou
selectLeft (Cursor (l:ls) rs us ds Nothing os ou)                        = Cursor ls rs us ds (Just (SL [os l] Left)) os ou
selectLeft (Cursor [] rs (u:us) ds (Just (SL ss Left)) os ou)            = Cursor u rs us ds (Just (ML [] [] ss Left)) os ou
selectLeft (Cursor (l:ls) rs us ds (Just (SL ss Left)) os ou)            = Cursor ls rs us ds (Just (SL ((os l):ss) Left)) os ou
selectLeft (Cursor ls rs us ds (Just (SL [s] Right)) os ou)              = Cursor ls ((ou s):rs) us ds Nothing os ou
selectLeft (Cursor ls rs us ds (Just (SL (s:ss) Right)) os ou)           = Cursor ls ((ou s):rs) us ds (Just (SL ss Right)) os ou
selectLeft (Cursor [] rs (u:us) ds (Just (ML sus sls sds Left)) os ou)   = Cursor u rs us ds (Just (ML [] (sus:sls) sds Left)) os ou
selectLeft (Cursor (l:ls) rs us ds (Just (ML sus sls sds Left)) os ou)   = Cursor ls rs us ds (Just (ML ((os l):sus) sls sds Left)) os ou
selectLeft (Cursor ls rs us ds (Just (ML [] [] [] Right)) os ou)         = Cursor ls [] us (rs:ds) Nothing os ou
selectLeft (Cursor ls rs us ds (Just (ML sus [] [] Right)) os ou)        = Cursor ls [] us (rs:ds) (Just (SL sus Right)) os ou
selectLeft (Cursor ls rs us ds (Just (ML sus (sl:sls) [] Right)) os ou)  = Cursor ls [] us (rs:ds) (Just (ML sus sls sl Right)) os ou
selectLeft (Cursor ls rs us ds (Just (ML sus sls (sd:sds) Right)) os ou) = Cursor ls (ou sd:rs) us ds (Just (ML sus sls sds Right)) os ou
selectLeft c                                                             = c

selectRight :: Cursor a -> Cursor a
selectRight (Cursor ls [] us (d:ds) Nothing os ou)                       = Cursor ls d us ds (Just (ML [] [] [] Right)) os ou
selectRight (Cursor ls (r:rs) us ds Nothing os ou)                       = Cursor ls rs us ds (Just (SL [os r] Right)) os ou
selectRight (Cursor ls [] us (d:ds) (Just (SL ss Right)) os ou)          = Cursor ls d us ds (Just (ML ss [] [] Right)) os ou
selectRight (Cursor ls (r:rs) us ds (Just (SL ss Right)) os ou)          = Cursor ls rs us ds (Just (SL (os r:ss) Right)) os ou
selectRight (Cursor ls [] us (d:ds) (Just (ML sus sls sds Right)) os ou) = Cursor ls d us ds (Just (ML sus (sds:sls) [] Right)) os ou
selectRight (Cursor ls (r:rs) us ds (Just (ML sus sls sds Right)) os ou) = Cursor ls rs us ds (Just (ML sus sls (os r:sds) Right)) os ou
selectRight (Cursor ls rs us ds (Just (SL [s] Left)) os ou)              = Cursor (ou s:ls) rs us ds Nothing os ou
selectRight (Cursor ls rs us ds (Just (SL (s:ss) Left)) os ou)           = Cursor (ou s:ls) rs us ds (Just (SL ss Left)) os ou
selectRight (Cursor ls rs us ds (Just (ML [] [] [] Left)) os ou)         = Cursor [] rs (ls:us) ds Nothing os ou
selectRight (Cursor ls rs us ds (Just (ML [] [] sds Left)) os ou)        = Cursor [] rs (ls:us) ds (Just (SL sds Left)) os ou
selectRight (Cursor ls rs us ds (Just (ML [] (sl:sls) sds Left)) os ou)  = Cursor [] rs (ls:us) ds (Just (ML sl sls sds Left)) os ou
selectRight (Cursor ls rs us ds (Just (ML (su:sus) sls sds Left)) os ou) = Cursor (ou su:ls) rs us ds (Just (ML sus sls sds Left)) os ou
selectRight c                                                            = c

selectUp :: Cursor a -> Cursor a
selectUp (Cursor ls rs us ds (Just (ML sus [] sds Right)) os ou) =
  applyNTimes (length sds + 1 + upLeftTimes) selectLeft (Cursor ls rs us ds (Just (ML sus [] sds Right)) os ou)
  where
    upLeftTimes = nonNegative (length ls + length sus - length sds)
selectUp (Cursor ls rs us ds (Just (ML sus (sl:sls) sds Right)) os ou) =
  applyNTimes (length sds + 1 + upLeftTimes) selectLeft (Cursor ls rs us ds (Just (ML sus (sl:sls) sds Right)) os ou)
  where
    upLeftTimes = nonNegative (length sl - length sds)
selectUp (Cursor ls rs [] ds sel os ou) =
  let times =
            case sel of
              Nothing                    -> length ls
              Just (SL ss Left)          -> length ls
              Just (SL ss Right)         -> length ls + length ss
              Just (ML sus sls sds Left) -> length ls
  in applyNTimes times selectLeft (Cursor ls rs [] ds sel os ou)
selectUp (Cursor ls rs (u:us) ds sel os ou) =
  let times =
            case sel of
              Nothing                    -> length ls + 1 + upLeftTimes
              Just (SL ss Left)          -> length ls + 1 + upLeftTimes
              Just (SL ss Right)         -> length ls + 1 + upLeftTimes' ss + length ss
              Just (ML sus sls sds Left) -> length ls + 1 + upLeftTimes
  in applyNTimes times selectLeft (Cursor ls rs (u:us) ds sel os ou)
  where
    upLeftTimes     = nonNegative (length u - length ls)
    upLeftTimes' ss = nonNegative (length u - (length ls + length ss))

selectDown :: Cursor a -> Cursor a
selectDown (Cursor ls rs us ds (Just (ML sus [] sds Left)) os ou) =
  applyNTimes (length sus + 1 + downLeftTimes) selectRight (Cursor ls rs us ds (Just (ML sus [] sds Left)) os ou)
  where
    downLeftTimes
      | length ls < (length sds + length rs) = length ls
      | otherwise                            = length sds + length rs
selectDown (Cursor ls rs us ds (Just (ML sus (sl:sls) sds Left)) os ou) =
  applyNTimes (length sus + 1 + downLeftTimes) selectRight (Cursor ls rs us ds (Just (ML sus (sl:sls) sds Left)) os ou)
  where
    downLeftTimes
      | length ls < length sl = length ls
      | otherwise             = length sl
selectDown (Cursor ls rs us [] sel os ou) =
  let times =
            case sel of
              Nothing                     -> length rs
              Just (SL ss Left)           -> length rs + length ss
              Just (SL ss Right)          -> length rs
              Just (ML sus sls sds Right) -> length rs
  in applyNTimes times selectRight (Cursor ls rs us [] sel os ou)
selectDown (Cursor ls rs us (d:ds) sel os ou) =
  let times =
            case sel of
              Nothing                     -> length rs + 1 + downLeftTimes ls
              Just (SL ss Left)           -> length ss + length rs + 1 + downLeftTimes ls
              Just (SL ss Right)          -> length rs + 1 + downLeftTimes' ss
              Just (ML sus sls sds Right) -> length sus + 1 + downLeftTimes sds
  in applyNTimes times selectRight (Cursor ls rs us (d:ds) sel os ou)
  where
    downLeftTimes s
      | length s < length d = length s
      | otherwise           = length d
    downLeftTimes' ss
      | length ls + length ss < length d = length ls + length ss
      | otherwise                        = length d

applyNTimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes 1 f x = f x
applyNTimes n f x = f (applyNTimes (n-1) f x)

nonNegative :: (Num n, Ord n) => n -> n
nonNegative n
  | n < 0     = 0
  | otherwise = n

selectLeftUntil :: (a -> Bool) -> Bool -> Cursor a -> Cursor a
selectLeftUntil eq b c = doLeftUntil eq b selectLeft c

selectRightUntil :: (a -> Bool) -> Bool -> Cursor a -> Cursor a
selectRightUntil eq b c = doRightUntil eq b selectRight c

selectToLineStart :: Cursor a -> Cursor a
selectToLineStart c = selectLeftUntil (\_ -> False) True c

selectToLineEnd :: Cursor a -> Cursor a
selectToLineEnd c = selectRightUntil (\_ -> False) True c

selectAll :: Cursor a -> Cursor a
selectAll (Cursor [] rs [] ds Nothing os ou) = selectRightUntil (\_ -> False) False $ (Cursor [] rs [] ds Nothing os ou)
selectAll c = selectAll $ (moveLeft . moveToScreenStart) c

getSelectedLines :: Cursor a -> [[a]]
getSelectedLines c =
  case selection c of
    Just (SL ss Left)           -> [ss]
    Just (SL ss Right)          -> [reverse ss]
    Just (ML sus sls sds Left)  -> (sus : sls) ++ [sds]
    Just (ML sus sls sds Right) -> ((reverse sus) : (reverse $ map reverse sls)) ++ [reverse sds]
    _                           -> []

-- Replace

searchAndReplace :: Eq a => [a] -> [a] -> Cursor a -> (Cursor a, [Position])
searchAndReplace [] new c  = (c,[])
searchAndReplace old new c = (moveToPosition p c', ps)
  where
    p = getCurrentPosition c
    (c', ps) = searchAndReplaceFromActualPosition old new $ (moveToScreenStart c,[])

replace :: Eq a => [a] -> [a] -> Cursor a -> Cursor a
replace [] _ c                         = c
replace old new (Cursor ls rs us ds s os ou) = (Cursor ls' rs' us' ds' s os ou) where
  ls' = DLU.replace (reverse old) (reverse new) ls
  rs' = DLU.replace old new rs
  us' = map (DLU.replace (reverse old) (reverse new)) us
  ds' = map (DLU.replace old new) ds

-- Properties

getCurrentPosition :: Cursor a -> Position
getCurrentPosition (Cursor ls _ us _ (Just (SL ss Right)) _ _)       = (length us, length ls + length ss)
getCurrentPosition (Cursor _ _ us _ (Just (ML _ sls sds Right)) _ _) = (length us + length sls + 1, length sds)
getCurrentPosition (Cursor ls _ us _ _ _ _)                          = (length us, length ls)

getLines :: Cursor a -> [[a]]
getLines (Cursor ls rs us ds s _ _) =
  case s of
    Nothing -> us' ++ [ls' ++ rs] ++ ds
    Just (SL l Left) -> us' ++ [ls' ++ l ++ rs] ++ ds
    Just (SL l Right) -> us' ++ [ls' ++ (reverse l) ++ rs] ++ ds
    Just (ML fl lb ll Left) -> us' ++ [ls' ++ fl] ++ lb ++ [ll ++ rs] ++ ds
    Just (ML fl lb ll Right) -> us' ++ [ls' ++ reverse fl] ++ (reverse $ map reverse lb) ++ [(reverse ll) ++ rs] ++ ds
  where
    us' = reverse $ map reverse us
    ls' = reverse ls

-- Abstract

doRightUntil :: (a -> Bool) -> Bool -> (Cursor a -> Cursor a) -> Cursor a -> Cursor a
doRightUntil eq untilLineEnd f (Cursor ls [] us [] s os ou) = (Cursor ls [] us [] s os ou)
doRightUntil eq untilLineEnd f (Cursor ls [] us ds s os ou)
  | untilLineEnd = Cursor ls [] us ds s os ou
  | otherwise = doRightUntil eq untilLineEnd f $ f (Cursor ls [] us ds s os ou)
doRightUntil eq b f (Cursor ls rs us ds (Just (SL (x:xs) Left)) os ou)
  | eq x = Cursor ls rs us ds (Just (SL (x:xs) Left)) os ou
  | otherwise = doRightUntil eq b f $ f (Cursor ls rs us ds (Just (SL (x:xs) Left)) os ou)
doRightUntil eq b f (Cursor ls rs us ds (Just (ML fl lb (x:xs) Left)) os ou)
  | eq x = Cursor ls rs us ds (Just (ML fl lb (x:xs) Left)) os ou
  | otherwise = doRightUntil eq b f $ f (Cursor ls rs us ds (Just (ML fl lb (x:xs) Left)) os ou)
doRightUntil eq b f (Cursor ls (r:rs) us ds s os ou)
  | eq r = Cursor ls (r:rs) us ds s os ou
  | otherwise = doRightUntil eq b f $ f (Cursor ls (r:rs) us ds s os ou)

doLeftUntil :: (a -> Bool) -> Bool -> (Cursor a -> Cursor a) -> Cursor a -> Cursor a
doLeftUntil eq untilLineStart f (Cursor [] rs [] ds s os ou) = (Cursor [] rs [] ds s os ou)
doLeftUntil eq untilLineStart f (Cursor [] rs us ds s os ou)
  | untilLineStart = Cursor [] rs us ds s os ou
  | otherwise = doLeftUntil eq untilLineStart f $ f (Cursor [] rs us ds s os ou)
doLeftUntil eq b f (Cursor ls rs us ds (Just (SL (x:xs) Right)) os ou)
  | eq x = Cursor ls rs us ds (Just (SL (x:xs) Right)) os ou
  | otherwise = doLeftUntil eq b f $ f (Cursor ls rs us ds (Just (SL (x:xs) Right)) os ou)
doLeftUntil eq b f (Cursor ls rs us ds (Just (ML fl lb (x:xs) Right)) os ou)
  | eq x = Cursor ls rs us ds (Just (ML fl lb (x:xs) Right)) os ou
  | otherwise = doLeftUntil eq b f $ f (Cursor ls rs us ds (Just (ML fl lb (x:xs) Right)) os ou)
doLeftUntil eq b f (Cursor (l:ls) rs us ds s os ou)
  | eq l = Cursor (l:ls) rs us ds s os ou
  | otherwise = doLeftUntil eq b f $ f (Cursor (l:ls) rs us ds s os ou)

-- Private

moveToSelectionStart :: Cursor a -> Cursor a
moveToSelectionStart (Cursor ls rs us ds (Just (SL ss Left)) os ou)           = Cursor ls (map ou ss ++ rs) us ds Nothing os ou
moveToSelectionStart (Cursor ls rs us ds (Just (SL ss Right)) os ou)          = Cursor ls ((reverse $ map ou ss) ++ rs) us ds Nothing os ou
moveToSelectionStart (Cursor ls rs us ds (Just (ML sus sls sds Left)) os ou)  = Cursor ls (map ou sus) us (sls' ++ (lastLineWithSelection : ds)) Nothing os ou
  where
    sls' = map (map ou) sls
    lastLineWithSelection = map ou sds ++ rs
moveToSelectionStart (Cursor ls rs us ds (Just (ML sus sls sds Right)) os ou) = Cursor ls (reverse $ map ou sus) us ((reverse (map reverse sls')) ++ (lastLineWithSelection : ds)) Nothing os ou
  where
    sls' = map (map ou) sls
    lastLineWithSelection = (reverse $ map ou sds) ++ rs
moveToSelectionStart c                                                  = c

moveToSelectionEnd :: Cursor a -> Cursor a
moveToSelectionEnd (Cursor ls rs us ds (Just (SL ss Left)) os ou)           = Cursor ((reverse $ map ou ss) ++ ls) rs us ds Nothing os ou
moveToSelectionEnd (Cursor ls rs us ds (Just (SL ss Right)) os ou)          = Cursor (map ou ss ++ ls) rs us ds Nothing os ou
moveToSelectionEnd (Cursor ls rs us ds (Just (ML sus sls sds Left)) os ou)  = Cursor (reverse $ map ou sds) rs ((reverse (map reverse sls')) ++ (firstLineWithSelection : us)) ds Nothing os ou
  where
    sls' = map (map ou) sls
    firstLineWithSelection = reverse (map ou sus) ++ ls
moveToSelectionEnd (Cursor ls rs us ds (Just (ML sus sls sds Right)) os ou) = Cursor (map ou sds) rs (sls' ++ (firstLineWithSelection : us)) ds Nothing os ou
  where
    sls' = map (map ou) sls
    firstLineWithSelection = map ou sus ++ ls
moveToSelectionEnd c                                                  = c

searchAndReplaceFromActualPosition :: Eq a => [a] -> [a] -> (Cursor a, [Position]) -> (Cursor a, [Position])
searchAndReplaceFromActualPosition _ _ (Cursor l [] u [] s os ou, p) = (Cursor l [] u [] s os ou, p)
searchAndReplaceFromActualPosition old new (c, ps) = searchAndReplaceFromActualPosition old new (moveDown $ c {right = replaced}, positions)
  where split'    = splitOn old $ right c
        replaced  = DLU.replace old new $ right c
        positions = calculatePositions split' ps 0
        calculatePositions (x:[]) ps _ = ps
        calculatePositions [] ps _     = ps
        calculatePositions (x:xs) ps a = calculatePositions xs (ps ++ [(length $ up c, a + (length x))]) (a + (length new) + (length x))

restOfLines :: Cursor a -> [[a]]
restOfLines (Cursor _ rs _ ds _ _ _) = rs:ds
