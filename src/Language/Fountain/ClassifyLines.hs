{-|
Module      : Language.Fountain.ClassifyLines
Description : Algorithms to classify text into different line types before line-level parsing
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fountain.ClassifyLines where

import Data.Text ( Text )
import qualified Data.Text as T ( empty )
import Language.Fountain.Preprocess


