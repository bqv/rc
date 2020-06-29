{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr

    , KeyState (..)
    , EventKey (..)

    , getKeystate
    , getKeymap

    , setKeymap
    , keyStateToInt

    , KeyboardModifiers (..)
    , getModifierPtr
    , readModifiers

    , WlrModifier (..)
    , getModifiers
    , modifierToNum
    , modifiersToField

    , modifierInField
    , fieldToModifiers

    , keyStateToButtonState
    , keyStateFromButtonState

    , getKeyboardKeys
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard.h>

import Data.Bits ((.&.), (.|.), Bits)
import Foreign.C.Types (CInt(..), CSize)
import Foreign.Storable (Storable(..))
import Data.Word (Word32)
import Foreign.Ptr (Ptr, plusPtr)

import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Input.Buttons

import Text.XkbCommon.InternalTypes (CKeymap, CKeyboardState)

data WlrKeyboard

data KeyboardSignals = KeyboardSignals
    { keySignalKey :: Ptr (WlSignal EventKey)
    , keySignalModifiers :: Ptr (WlSignal ())
    }

getKeySignals :: Ptr WlrKeyboard -> KeyboardSignals
getKeySignals ptr = KeyboardSignals 
    { keySignalKey = #{ptr struct wlr_keyboard, events.key} ptr
    , keySignalModifiers = #{ptr struct wlr_keyboard, events.modifiers} ptr
    }

data KeyState
    = KeyReleased
    | KeyPressed
    deriving (Show, Eq)

keyStateToButtonState :: KeyState -> ButtonState
keyStateToButtonState KeyReleased = ButtonReleased
keyStateToButtonState KeyPressed = ButtonPressed

keyStateFromButtonState :: ButtonState -> KeyState
keyStateFromButtonState  ButtonReleased = KeyReleased
keyStateFromButtonState  ButtonPressed  = KeyPressed

getKeyDataPtr :: Ptr WlrKeyboard -> Ptr (Ptr a)
getKeyDataPtr = #{ptr struct wlr_keyboard, data}

keyStateFromInt :: CInt -> KeyState
keyStateFromInt #{const WLR_KEY_RELEASED} = KeyReleased
keyStateFromInt #{const WLR_KEY_PRESSED} = KeyPressed
keyStateFromInt x = error $ "Got invalid KeyState: " ++ show x

keyStateToInt :: Num a => KeyState -> a
keyStateToInt KeyReleased = #{const WLR_KEY_RELEASED}
keyStateToInt KeyPressed = #{const WLR_KEY_PRESSED}


data EventKey = EventKey
    { timeSec :: Word32
    , keyCode :: Word32
    , state :: KeyState
    }
    deriving (Show)

instance Storable EventKey where
    sizeOf _ = #{size struct wlr_event_keyboard_key}
    alignment _ = #{alignment struct wlr_event_keyboard_key}
    peek ptr = EventKey
        <$> #{peek struct wlr_event_keyboard_key, time_msec} ptr
        <*> #{peek struct wlr_event_keyboard_key, keycode} ptr
        <*> (fmap keyStateFromInt . #{peek struct wlr_event_keyboard_key, state}) ptr
    poke = error "We don't poke EventKeys"

foreign import ccall "wlr_keyboard_set_keymap" c_set_keymap :: Ptr WlrKeyboard -> Ptr CKeymap -> IO ()

setKeymap :: Ptr WlrKeyboard -> Ptr CKeymap -> IO ()
setKeymap = c_set_keymap

getKeystate :: Ptr WlrKeyboard -> IO (Ptr CKeyboardState)
getKeystate = #{peek struct wlr_keyboard, xkb_state}

getKeymap :: Ptr WlrKeyboard -> IO (Ptr CKeymap)
getKeymap = #{peek struct wlr_keyboard, keymap}


data KeyboardModifiers = Modifiers
    { modDepressed :: Word32
    , modLatched :: Word32
    , modLocked :: Word32
    , modGroup :: Word32
    }

getModifierPtr :: Ptr WlrKeyboard -> Ptr KeyboardModifiers
getModifierPtr = #{ptr struct wlr_keyboard, modifiers}

readModifiers :: Ptr WlrKeyboard -> IO KeyboardModifiers
readModifiers ptr = Modifiers
    <$> #{peek struct wlr_keyboard, modifiers.depressed} ptr
    <*> #{peek struct wlr_keyboard, modifiers.latched} ptr
    <*> #{peek struct wlr_keyboard, modifiers.locked} ptr
    <*> #{peek struct wlr_keyboard, modifiers.group} ptr

foreign import ccall unsafe "wlr_keyboard_get_modifiers" c_get_modifiers :: Ptr WlrKeyboard -> IO Word32

getModifiers :: Ptr WlrKeyboard -> IO Word32
getModifiers = c_get_modifiers

data WlrModifier
    = Shift
    | Caps
    | Ctrl
    | Alt
    | Mod2
    | Mod3
    | Logo
    | Mod5
    deriving (Show, Eq)

modifierToNum :: Num a => WlrModifier -> a
modifierToNum Shift = #{const WLR_MODIFIER_SHIFT}
modifierToNum Caps  = #{const WLR_MODIFIER_CAPS}
modifierToNum Ctrl  = #{const WLR_MODIFIER_CTRL}
modifierToNum Alt   = #{const WLR_MODIFIER_ALT}
modifierToNum Mod2  = #{const WLR_MODIFIER_MOD2}
modifierToNum Mod3  = #{const WLR_MODIFIER_MOD3}
modifierToNum Logo  = #{const WLR_MODIFIER_LOGO}
modifierToNum Mod5  = #{const WLR_MODIFIER_MOD5}

modifiersToField :: (Num a, Bits a, Foldable t) => t WlrModifier -> a
modifiersToField = foldr ((.|.) . modifierToNum) 0

modifierInField :: (Num a, Bits a) => WlrModifier -> a -> Bool
modifierInField modifier field = modifierToNum modifier .&. field /= 0

fieldToModifiers :: (Num a, Bits a) => a -> [WlrModifier]
fieldToModifiers field =
    foldr prependIf [] allMods
    where   prependIf :: WlrModifier -> [WlrModifier] -> [WlrModifier]
            prependIf modifier mods =
                if modifierInField modifier field
                    then (modifier:mods)
                    else mods
            allMods :: [WlrModifier]
            allMods = [Shift, Caps, Ctrl, Alt, Mod2, Mod3, Logo, Mod5]

getKeyboardKeys :: Ptr WlrKeyboard -> IO (Ptr Word32, CSize)
getKeyboardKeys ptr = do
    let ret = #{ptr struct wlr_keyboard, keycodes} ptr
    num <- #{peek struct wlr_keyboard, num_keycodes} ptr
    pure (ret, num)
