module TextZipper
(   Line,
    Text,
    TextZipper,
    emptyTextZipper,
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
    toString,
    position,
    (-:)
) where

type Line = String
type Text = [Line]

type TextZipper = (Line, Line, [Line], [Line])

emptyTextZipper = ("", "", [], [])

moveLeft :: TextZipper -> TextZipper
moveLeft ("", liner, u:us, ds) = (u, "", us, liner:ds)
moveLeft (c:linel, liner, us, ds) = (linel, c:liner, us, ds)
moveLeft tz = tz

moveRight :: TextZipper -> TextZipper
moveRight (linel, "", us, d:ds) = ("", d, linel:us, ds)
moveRight (linel, c:liner, us, ds) = (c:linel, liner, us, ds)
moveRight tz = tz

moveUp :: TextZipper -> TextZipper
moveUp (linel, liner, u:us, ds) = (firstPart, reverse lastPart, us, (reverse linel ++ liner):ds) where
    (lastPart, firstPart) = splitAt (length u - length linel) u
moveUp tz = tz

moveDown :: TextZipper -> TextZipper
moveDown (linel, liner, us, d:ds) = (reverse firstPart, lastPart, (reverse liner ++ linel):us, ds) where
    (firstPart, lastPart) = splitAt (length linel) d
moveDown tz = tz

moveToLineStart :: TextZipper -> TextZipper
moveToLineStart (linel, liner, us, ds) = ("", reverse linel ++ liner, us, ds)

moveToLineEnd :: TextZipper -> TextZipper
moveToLineEnd (linel, liner, us, ds) = (reverse liner ++ linel, "", us, ds)

moveToScreenTop :: TextZipper -> TextZipper
moveToScreenTop (linel, liner, [], ds) = (linel, liner, [], ds)
moveToScreenTop tz = moveToScreenTop $ moveUp tz

moveToScreenBottom :: TextZipper -> TextZipper
moveToScreenBottom (linel, liner, us, []) = (linel, liner, us, [])
moveToScreenBottom tz = moveToScreenBottom $ moveDown tz

moveToScreenStart :: TextZipper -> TextZipper
moveToScreenStart tz = moveToScreenTop $ moveToLineStart tz

moveToScreenEnd :: TextZipper -> TextZipper
moveToScreenEnd tz = moveToScreenBottom $ moveToLineEnd tz

newChar :: Char -> TextZipper -> TextZipper
newChar c (linel, liner, us, ds) = (c:linel, liner, us, ds)
newChar '\n' tz = newLine tz

newStr :: String -> TextZipper -> TextZipper
newStr str tz = foldl (\h c-> newChar c h) tz str

newLine :: TextZipper -> TextZipper
newLine (linel, liner, us, ds) = ("", liner, linel:us, ds)

delete :: TextZipper -> TextZipper
delete ("", liner, u:us, ds) = (u, liner, us, ds)
delete (l:linel, liner, us, ds) = (linel, liner, us, ds)
delete tz = tz

restOfTextToString :: TextZipper -> String
restOfTextToString (_, liner, _, ds) = foldr (\s h -> s ++ "\n" ++ h) "" $ liner:ds

toString :: TextZipper -> String
toString tz = restOfTextToString $ moveToScreenStart tz

position :: TextZipper -> (Int,Int)
position (linel, _, us, _) = (length us, length linel)



(-:) x  f = f x

