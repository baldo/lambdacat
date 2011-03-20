-- |
-- Module      : LambdaCat.Utils.InputBuffer
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides the 'InputBuffer' type.

module LambdaCat.Utils.InputBuffer
    (
      -- * The InputBuffer type
      InputBuffer

    , empty

    , before
    , after

      -- * Conversion
    , fromString
    , toString

      -- * Navigation
    , left
    , right

    , home
    , end

      -- * Insertion
    , insert
    , insertString

      -- * Deletion
    , delete
    , backspace

      -- * Completion
    , complete
    )
where

-- | Datatype holding an input buffer.
data InputBuffer = InputBuffer
    { _before :: String  -- ^ The String before the cursor in reversed order.
    , _after  :: String  -- ^ The String after the cursor.
    }

instance Show InputBuffer where
    showsPrec _ buffer = showChar '<'
                       . shows (before buffer)
                       . showChar '|'
                       . shows (after buffer)
                       . showChar '>'

-- | Gives the String before the cursor.
before :: InputBuffer -> String
before = reverse . _before

-- | Gives the String after the cursor.
after :: InputBuffer -> String
after = _after

-- | Creates an empty InputBuffer.
empty :: InputBuffer
empty = InputBuffer
    { _before = ""
    , _after  = ""
    }

-- | Generates an InputBuffer holding the given String. The cursor is placed
-- after the String.
fromString :: String -> InputBuffer
fromString cs =
    empty
        { _before = reverse cs
        }

-- | Gives the String hold by the given InputBuffer.
toString :: InputBuffer -> String
toString buffer = _after $ home buffer

-- | Moves the cursor one character to the left. If the cursor cannot move any
-- further, the InputBuffer stays unchanged.
left :: InputBuffer -> InputBuffer
left buffer@InputBuffer { _before = "" } =
    buffer
left InputBuffer { _before = c : bs, _after = as } =
    InputBuffer
        { _before = bs
        , _after  = c : as
        }

-- | Moves the cursor one character to the right. If the cursor cannot move
-- any further, the InputBuffer stays unchanged.
right :: InputBuffer -> InputBuffer
right buffer@InputBuffer { _after = "" } =
    buffer
right InputBuffer { _before = bs, _after = c : as } =
    InputBuffer
        { _before = c : bs
        , _after  = as
        }

-- | Places the cursor at the beginning of the InputBuffer.
home :: InputBuffer -> InputBuffer
home InputBuffer { _before = bs, _after = as } =
    InputBuffer
        { _before = ""
        , _after  = (reverse bs) ++ as
        }

-- | Places the cursor at the end of the InputBuffer.
end :: InputBuffer -> InputBuffer
end InputBuffer { _before = bs, _after = as } =
    InputBuffer
        { _before = (reverse as) ++ bs
        , _after = ""
        }

-- | Insert the given Char at the cursor's current position. The curosr is
-- placed after the inserted Char.
insert :: Char -> InputBuffer -> InputBuffer
insert c buffer@InputBuffer { _before = bs } =
    buffer
        { _before = c : bs
        }

-- | Insert the given String at the cursor's current position. The curosr is
-- placed after the inserted String.
insertString :: String -> InputBuffer -> InputBuffer
insertString cs buffer = foldl (flip insert) buffer cs

-- | Deletes the Char after the cursor if possible. Otherwise the InputBuffer
-- stays unchanged.
delete :: InputBuffer -> InputBuffer
delete buffer@InputBuffer { _after = "" } =
    buffer
delete buffer@InputBuffer { _after = _ : as } =
    buffer
        { _after = as
        }

-- | Deletes the Char before the cursor if possible. Otherwise the
-- InputBuffer stays unchanged.
backspace :: InputBuffer -> InputBuffer
backspace buffer@InputBuffer { _before = "" } =
    buffer
backspace buffer@InputBuffer { _before = _ : bs } =
    buffer
        { _before = bs
        }

-- | Completes the InputBuffer with the result of the given function. The
-- completion is done at the cursor's current position. The cursor is placed
-- after the completed part.
--
-- This function can be used e.g. for tab-completion.
complete
    :: (InputBuffer -> String)  -- ^ Function that gives the String to be
                                --   inserted depending on the contents of the
                                --   InputBuffer.
    -> InputBuffer              -- ^ InputBuffer to complete.
    -> InputBuffer              -- ^ Completed InputBuffer.
complete f buffer = insertString (f buffer) buffer

