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
moveLeft sz = sz

moveRight :: TextZipper -> TextZipper
moveRight (linel, "", us, d:ds) = ("", d, linel:us, ds)
moveRight (linel, c:liner, us, ds) = (c:linel, liner, us, ds)
moveRight sz = sz

moveUp :: TextZipper -> TextZipper
moveUp (linel, liner, u:us, ds) = (firstPart, reverse lastPart, us, (reverse linel ++ liner):ds) where
    (lastPart, firstPart) = splitAt (length u - length linel) u
moveUp sz = sz

moveDown :: TextZipper -> TextZipper
moveDown (linel, liner, us, d:ds) = (reverse firstPart, lastPart, (reverse liner ++ linel):us, ds) where
    (firstPart, lastPart) = splitAt (length linel) d
moveDown sz = sz

moveToLineStart :: TextZipper -> TextZipper
moveToLineStart (linel, liner, us, ds) = ("", reverse linel ++ liner, us, ds)

moveToLineEnd :: TextZipper -> TextZipper
moveToLineEnd (linel, liner, us, ds) = (reverse liner ++ linel, "", us, ds)

moveToScreenTop :: TextZipper -> TextZipper
moveToScreenTop (linel, liner, [], ds) = (linel, liner, [], ds)
moveToScreenTop sz = moveToScreenTop $ moveUp sz

moveToScreenBottom :: TextZipper -> TextZipper
moveToScreenBottom (linel, liner, us, []) = (linel, liner, us, [])
moveToScreenBottom sz = moveToScreenBottom $ moveDown sz

moveToScreenStart :: TextZipper -> TextZipper
moveToScreenStart sz = moveToScreenTop $ moveToLineStart sz

moveToScreenEnd :: TextZipper -> TextZipper
moveToScreenEnd sz = moveToScreenBottom $ moveToLineEnd sz

newChar :: Char -> TextZipper -> TextZipper
newChar c (linel, liner, us, ds) = (c:linel, liner, us, ds)

newStr :: String -> TextZipper -> TextZipper
newStr str sz = foldl (\h c-> newChar c h) sz str

newLine :: TextZipper -> TextZipper
newLine (linel, liner, us, ds) = ("", liner, linel:us, ds)

delete :: TextZipper -> TextZipper
delete ("", liner, u:us, ds) = (u, liner, us, ds)
delete (l:linel, liner, us, ds) = (linel, liner, us, ds)
delete sz = sz

restOfTextToString :: TextZipper -> String
restOfTextToString (_, liner, _, ds) = foldr (\s h -> s ++ "\n" ++ h) "" $ liner:ds

toString :: TextZipper -> String
toString sz = restOfTextToString $ moveToScreenStart sz

position :: TextZipper -> (Int,Int)
position (linel, _, us, _) = (length us, length linel)



(-:) x  f = f x

