-- |
-- Module      : LambdaCat.Utils.InputBuffer.Completion
-- Copyright   : Andreas Baldeau, Daniel Ehlers
-- License     : BSD3
-- Maintainer  : Andreas Baldeau <andreas@baldeau.net>,
--               Daniel Ehlers <danielehlers@mindeye.net>
-- Stability   : Alpha
--
-- This module provides completion functionality for 'InputBuffer's.

module LambdaCat.Utils.InputBuffer.Completion
    ( complete
    )
where

import LambdaCat.Utils.InputBuffer

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

