-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IfThenElse
-- Copyright   :  (c) Nils Schweinsberg 2010
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Import this library to overload @if then else@ expressions. Requires the
-- RebindableSyntax extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances
             #-}

module Data.IfThenElse
    (
      IfThenElse (..)

      -- * Example usage
      -- $example
    ) where

class IfThenElse a b c d | a b c -> d where
    ifThenElse :: a -> b -> c -> d

instance (Monad m, IfThenElse (t a) (a -> m b) (m b) (m b))
      => IfThenElse (m (t a)) (a -> m b) (m b) (m b) where
    ifThenElse m t e = do
        b <- m
        ifThenElse b t e

instance (Monad m, IfThenElse a (m b) (m c) (m d))
      => IfThenElse (m a) (m b) (m c) (m d) where
    ifThenElse m t e = do
        b <- m
        ifThenElse b t e

instance IfThenElse Bool a a a where
    ifThenElse True  t _ = t
    ifThenElse False _ e = e

instance IfThenElse (Maybe a) (a -> b) b b where
    ifThenElse (Just a) t _ = t a
    ifThenElse Nothing  _ e = e


-- $example
-- 
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Data.IfThenElse
-- >
-- > import System.Environment
-- > import System.Directory
-- >
-- > main = do
-- >     [d] <- getArgs
-- >     h <- getHomeDirectory
-- >
-- >     -- Test: IO Bool
-- >     if doesDirectoryExist d
-- >        then putStrLn "Found directory."
-- >        else putStrLn "No such directory."
-- >
-- >     -- Test: IO (Maybe FilePath)
-- >     if findExecutable d
-- >        then \fp -> putStrLn $ "Found executable located at " ++ fp
-- >        else putStrLn "No such executable."
-- >
-- >     -- Test: Bool
-- >     if d == h
-- >        then putStrLn $ d ++ " is your home directory."
-- >        else putStrLn $ d ++ " is not your home directory."
