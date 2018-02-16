module Cursor
  ( Cursor (..),
    Selection,
    empty,
    insert,
    insertLine,
    delete,
    moveLeft,
    moveRight,
    moveUp,
    moveDown,
    moveToLineStart,
    moveToLineEnd,
    moveToScreenTop,
    moveToScreenStart,
    getLines,
    (-:)
  ) where

data Direction = Left | Right deriving (Show)

data Selection a = Selection
  { firstLine :: [a],
    completeLines :: [[a]],
    lastLine :: [a],
    direction :: Direction
  } deriving (Show)

data Cursor a = Cursor
  { left :: [a],
    right :: [a],
    up :: [[a]],
    down :: [[a]],
    selection :: Maybe (Selection a)
  } deriving (Show)

empty :: Cursor a
empty = Cursor [] [] [] [] Nothing

--TODO insert for Just
insert :: Cursor a -> a -> Cursor a
insert (Cursor ls rs us ds Nothing) e = Cursor (e:ls) rs us ds Nothing

-- TODO newLine for Just
insertLine :: Cursor a -> Cursor a
insertLine (Cursor ls rs us ds Nothing) = Cursor [] rs (ls:us) ds Nothing

-- TODO delete for Just
delete :: Cursor a -> Cursor a
delete (Cursor (l:ls) rs us ds Nothing) = Cursor ls rs us ds Nothing
delete (Cursor [] rs (u:us) ds Nothing) = Cursor u rs us ds Nothing
delete c                                = c

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

--TODO tener en cuenta tamaño de la terminal
moveUp :: Cursor a -> Cursor a
moveUp (Cursor ls rs [] ds s)           = moveToLineStart (Cursor ls rs [] ds s)
moveUp (Cursor ls rs (u:us) ds Nothing) = Cursor firstPart (reverse lastPart) us ((reverse ls ++ rs):ds) Nothing where
  (lastPart, firstPart) = splitAt (length u - length ls) u
moveUp (Cursor ls rs us ds (Just s))    = moveUp $ moveToSelectionStart (Cursor ls rs us ds (Just s))

--TODO tener en cuenta tamaño de la terminal
moveDown :: Cursor a -> Cursor a
moveDown (Cursor ls rs us [] Nothing)     = moveToLineEnd (Cursor ls rs us [] Nothing)
moveDown (Cursor ls rs us (d:ds) Nothing) = Cursor (reverse firstPart) lastPart ((reverse rs ++ ls):us) ds Nothing where
  (firstPart, lastPart) = splitAt (length ls) d
moveDown (Cursor ls rs us ds (Just s)) = moveDown $ moveToSelectionEnd (Cursor ls rs us ds (Just s))

moveToLineStart :: Cursor a -> Cursor a
moveToLineStart (Cursor ls rs us ds Nothing) = Cursor [] (reverse ls ++ rs) us ds Nothing
moveToLineStart c                            = moveToLineStart $ moveToSelectionStart c

moveToLineEnd :: Cursor a -> Cursor a
moveToLineEnd (Cursor ls rs us ds Nothing) = Cursor (reverse rs ++ ls) [] us ds Nothing
moveToLineEnd c                            = moveToLineEnd $ moveToSelectionEnd c

moveToScreenTop :: Cursor a -> Cursor a
moveToScreenTop (Cursor ls rs [] ds Nothing) = Cursor ls rs [] ds Nothing
moveToScreenTop (Cursor ls rs us ds (Just s))  = moveToLineStart $ moveToSelectionStart (Cursor ls rs us ds (Just s))
moveToScreenTop c                            = moveToScreenTop $ moveUp c

-- moveToScreenBottom :: Cursor a -> Cursor a
-- moveToScreenBottom (Cursor ls rs us [] Nothing) = Cursor ls rs us [] Nothing
-- moveToScreenBottom (Cursor ls rs us ds (Just s))  = moveToLineEnd $ moveToSelectionEnd (Cursor ls rs us ds Just s)
-- moveToScreenBottom c                            = moveToScreenBottom $ moveDown c

moveToScreenStart :: Cursor a -> Cursor a
moveToScreenStart c = moveToScreenTop $ moveToLineStart c

-- moveToScreenEnd :: Cursor a -> Cursor a
-- moveToScreenEnd c = moveToScreenBottom $ moveToLineEnd c

getLines :: Cursor a -> [[a]]
getLines c = restOfLines $ moveToScreenStart c



(-:) x f = f x -- TODO remove

-- Private

moveToSelectionStart :: Cursor a -> Cursor a
moveToSelectionStart (Cursor ls rs us ds (Just (Selection sus sls sds d))) = Cursor ls sus us (sls ++ (borderLine : ds)) Nothing where
  borderLine = sds ++ rs

moveToSelectionEnd :: Cursor a -> Cursor a
moveToSelectionEnd (Cursor ls rs us ds (Just (Selection sus sls sds d))) = Cursor (reverse sds) rs ((reverse (map reverse sls)) ++ (borderLine : us)) ds Nothing where
  borderLine = reverse sus ++ ls

restOfLines :: Cursor a -> [[a]]
restOfLines (Cursor _ rs _ ds _) = rs:ds

--
-- moveLeft :: Cursor -> Cursor
-- moveLeft ([], rs, u:us, ds, ([], [], []))  = (u, [], us, rs:ds, ([], [], []))
-- moveLeft (l:ls, rs, us, ds, ([], [], []))  = (ls, l:rs, us, ds, ([], [], []))
-- moveLeft (ls, rs, us, ds, (sus, sls, sds)) = (ls, sus, us, sls ++ ((reverse sds ++ rs) : ds), ([], [], []))
--
-- moveRight :: Cursor -> Cursor
-- moveRight (ls, [], us, d:ds, ([], [], []))  = ([], d, ls:us, ds, ([], [], []))
-- moveRight (ls, r:rs, us, ds, ([], [], []))  = (r:ls, rs, us, ds, ([], [], []))
-- moveRight (ls, rs, us, ds, (sus, sls, sds)) = (sds, rs, sls ++ ((ls ++ sus) : us), ds, ([], [], []))
--
-- moveUp :: Cursor -> Cursor
-- moveUp (ls, lr, [], ds, ([], [], []))      =
-- moveUp (ls, lr, u:us, ds, ([], [], []))    = (firstPart, reverse lastPart, us, (reverse ls ++ rs):ds) where
--     (lastPart, firstPart) = splitAt (length u - length ls) u
-- moveUp (ls, rs, us, ds, (sus, sls, sds)) = (ls, sus, us, , ([], [], []))
--
-- selectLeft :: Cursor -> Cursor
-- selectLeft ([], rs, u:us, ds, (sus, sls, sds)) = (u, [], us, rs:ds, ([], sus:sls, sds))
-- selectLeft (l:ls, rs, us, ds, (sus, sls, sds)) = (ls, rs, us, ds, (l:sus, sls, sds))
-- selectLeft cc                                  = cc
--
-- newLine :: CLursor -> Cursor
-- newLine (ls, rs, us, ds, (_, _, _)) = ([], rs, ls:us, ds, ([], [], []))
--
-- getLines :: Cursor -> [[StyleChar]]
-- getLines (ls, rs, us, ds, (sus, sls, sds)) = us ++ ((reverse ls ++ sus) : sls) ++ ((sds ++ rs) : ds)
--
