-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 =  above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside (flipV knight) (flipV (invert knight))) (beside (invert knight) knight)


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

buildLeftBoardSide :: Picture
buildLeftBoardSide = beside (beside rook knight) bishop

buildRightBoardSide :: Picture
buildRightBoardSide = beside (beside bishop knight) rook

buildRow :: Picture
buildRow = beside (beside (beside buildLeftBoardSide queen) king) buildRightBoardSide

whiteRow :: Picture
whiteRow = over buildRow otherEmptyRow

blackRow :: Picture
blackRow = over (invert buildRow) emptyRow

-- e)

buildPawns :: Picture
buildPawns = repeatH 8 pawn

buildWhitePawnsRow :: Picture
buildWhitePawnsRow = over buildPawns emptyRow

buildBlackPawnsRow :: Picture
buildBlackPawnsRow = over (invert buildPawns) otherEmptyRow

populatedBoard :: Picture
populatedBoard = above (above (above (above blackRow buildBlackPawnsRow) middleBoard) buildWhitePawnsRow) whiteRow



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = above (twoBeside x) (invert (twoBeside x))