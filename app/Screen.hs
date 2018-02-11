module Screen
(   Line,
    Screen,
    ScreenZipper,
    emptyScreenZipper,
    moveLeft,
    moveRight,
    moveUp,
    moveDown,
    moveToLineStart,
    moveToLineEnd,
    moveToScreenStart,
    moveToScreenEnd,
    newChar,
    newStr,
    newLine,
    delete,
    screenToString,
    (-:)
) where

type Line = String
type Screen = [Line]

type ScreenZipper = (Line, Line, [Line], [Line])

emptyScreenZipper = ("", "", [], [])



moveLeft :: ScreenZipper -> ScreenZipper
moveLeft ("", liner, u:us, ds) = (u, "", us, liner:ds)
moveLeft (c:linel, liner, us, ds) = (linel, c:liner, us, ds)
moveLeft sz = sz

moveRight :: ScreenZipper -> ScreenZipper
moveRight (linel, "", us, d:ds) = ("", d, linel:us, ds)
moveRight (linel, c:liner, us, ds) = (c:linel, liner, us, ds)
moveRight sz = sz

moveUp :: ScreenZipper -> ScreenZipper
moveUp (linel, liner, u:us, ds) = (firstPart, reverse lastPart, us, (reverse linel ++ liner):ds) where
    (lastPart, firstPart) = splitAt (length u - length linel) u
moveUp sz = sz

moveDown :: ScreenZipper -> ScreenZipper
moveDown (linel, liner, us, d:ds) = (reverse firstPart, lastPart, (reverse liner ++ linel):us, ds) where
    (firstPart, lastPart) = splitAt (length linel) d
moveDown sz = sz

moveToLineStart :: ScreenZipper -> ScreenZipper
moveToLineStart (linel, liner, us, ds) = ("", reverse linel ++ liner, us, ds)

moveToLineEnd :: ScreenZipper -> ScreenZipper
moveToLineEnd (linel, liner, us, ds) = (reverse liner ++ linel, "", us, ds)

moveToScreenTop :: ScreenZipper -> ScreenZipper
moveToScreenTop (linel, liner, [], ds) = (linel, liner, [], ds)
moveToScreenTop sz = moveToScreenTop $ moveUp sz

moveToScreenBottom :: ScreenZipper -> ScreenZipper
moveToScreenBottom (linel, liner, us, []) = (linel, liner, us, [])
moveToScreenBottom sz = moveToScreenBottom $ moveDown sz

moveToScreenStart :: ScreenZipper -> ScreenZipper
moveToScreenStart sz = moveToScreenTop $ moveToLineStart sz

moveToScreenEnd :: ScreenZipper -> ScreenZipper
moveToScreenEnd sz = moveToScreenBottom $ moveToLineEnd sz

newChar :: Char -> ScreenZipper -> ScreenZipper
newChar c (linel, liner, us, ds) = (c:linel, liner, us, ds)

newStr :: String -> ScreenZipper -> ScreenZipper
newStr str sz = foldl (\h c-> newChar c h) sz str

newLine :: ScreenZipper -> ScreenZipper
newLine (linel, liner, us, ds) = ("", liner, linel:us, ds)

delete :: ScreenZipper -> ScreenZipper
delete ("", liner, u:us, ds) = (u, liner, us, ds)
delete (l:linel, liner, us, ds) = (linel, liner, us, ds)
delete sz = sz

restOfScreenToString :: ScreenZipper -> String
restOfScreenToString (_, liner, _, ds) = foldr (\s h -> s ++ "\n" ++ h) "" $ liner:ds

screenToString :: ScreenZipper -> String
screenToString sz = restOfScreenToString $ moveToScreenStart sz





(-:) x  f = f x

