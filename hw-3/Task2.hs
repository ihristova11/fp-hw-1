module Task2 where 

import Data.Word

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

greyscaleF :: Rgb -> Rgb 
greyscaleF (Rgb r g b) = Rgb val val val
    where val = (0.30 * r) + (0.59 * g) + (0.11 * b)

grayscale :: Image -> Image
grayscale (Image w h c) = Image w h (map (map greyscaleF) c)

-- мап на всеки елемент, за всеки елемент пак мап и за всяко от стойностите прилагаме горната формула