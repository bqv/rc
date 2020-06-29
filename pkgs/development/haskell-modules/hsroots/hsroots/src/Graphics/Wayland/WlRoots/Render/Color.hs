module Graphics.Wayland.WlRoots.Render.Color
    ( Color (..)
    , colorWhite
    , colorBlack
    , darkenBy
    )
where

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

data Color = Color
    { colorR :: Float
    , colorG :: Float
    , colorB :: Float
    , colorA :: Float
    } deriving (Show)

darkenBy :: Float -> Color -> Color
darkenBy d (Color r g b a) =
    Color (r * d) (g * d) (b * d) a

instance Storable Color where
    sizeOf _ = 4 * sizeOf (undefined :: Float)
    alignment _ = alignment (undefined :: Int)
    peek ptr = Color
        <$> peekElemOff (castPtr ptr) 0
        <*> peekElemOff (castPtr ptr) 1
        <*> peekElemOff (castPtr ptr) 2
        <*> peekElemOff (castPtr ptr) 3
    poke ptr c = do
        pokeElemOff (castPtr ptr) 0 $ colorR c
        pokeElemOff (castPtr ptr) 1 $ colorG c
        pokeElemOff (castPtr ptr) 2 $ colorB c
        pokeElemOff (castPtr ptr) 3 $ colorA c

colorWhite :: Color
colorWhite = Color 1 1 1 1

colorBlack :: Color
colorBlack = Color 0 0 0 1
