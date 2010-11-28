-----------------------------------------------------------------------------
-- |
-- Module      :  Rebindable.If
--
-- Copyright   :  (c) Nils Schweinsberg 2010
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Import this library to overload @if then else@ expressions. Requires the
-- @RebindableSyntax@ extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
             #-}

module Rebindable.If
    (
      RebindableIf (..)

      -- * Example usage
      -- $example
    ) where

class RebindableIf t a where
    ifThenElse :: t -> a -> a -> a

instance RebindableIf Bool a where
    ifThenElse True  t _ = t
    ifThenElse False _ e = e

instance (Monad m, RebindableIf t (m a)) => RebindableIf (m t) (m a) where
    ifThenElse m t e = do
        b <- m
        ifThenElse b t e

-- $example
-- 
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Rebindable.If
-- >
-- > import System.Environment
-- > import System.Directory
-- >
-- > main = do
-- >     [d] <- getArgs
-- >     h   <- getHomeDirectory
-- >
-- >     -- Test: IO Bool
-- >     if doesDirectoryExist d
-- >        then putStrLn "Found directory."
-- >        else putStrLn "No such directory."
-- >
-- >     -- Test: Bool
-- >     if d == h
-- >        then putStrLn $ d ++ " is your home directory."
-- >        else putStrLn $ d ++ " is not your home directory."
