{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : LiveCoding.FRP.Curry
-- Description : Function uncurrying utilities
-- Copyright   : (c) 2022 Miguel Negrão
-- Maintainer  : miguel.negrao@friendlyvirus.org
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- Functions which are lifted into arrows have to be in completely curried form.
-- This module provides utility functions to automatically uncurry functions and
-- lift them to 'Arrow's using the tuple package's 'Curry' typeclass.
module LiveCoding.FRP.Curry where

import Data.Tuple.Curry (Curry (uncurryN))
import LiveCoding (Arrow (arr), Cell, Typeable, arrChangesM, arrM)
import LiveCoding.FRP (arrEM)

-- | Version of 'arr' which uncurries functions of arbitrary arity.
arrUncurry :: (Arrow a, Curry (b -> c) t) => t -> a b c
arrUncurry = arr . uncurryN

-- | Version of 'arrM' which uncurries functions of arbitrary arity.
arrUncurryM ::
  (Curry (input -> m output) b, Monad m) =>
  b ->
  Cell m input output
arrUncurryM = arrM . uncurryN

-- | Version of 'arrChangesM' which uncurries functions of arbitrary arity.
arrUncurryChangesM ::
  (Curry (input -> m output) b, Monad m, Typeable input, Typeable output, Eq input) =>
  b ->
  Cell m input output
arrUncurryChangesM = arrChangesM . uncurryN

-- | Version of 'arrEM' which uncurries functions of arbitrary arity.
arrUncurryEM ::
  (Curry (input -> m output) b, Monad m) =>
  b ->
  Cell m (Maybe input) (Maybe output)
arrUncurryEM = arrEM . uncurryN
