module Task2 where 

import Data.Word
data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

-- A) grayscale
grayscaleF :: Rgb -> Rgb 
grayscaleF (Rgb r g b) = Rgb val val val
    where val = round ((0.30 * toRational r) + (0.59 * toRational g) + (0.11 * toRational b))

grayscale :: Image -> Image
grayscale (Image w h c) = Image w h (map (map grayscaleF) c)

