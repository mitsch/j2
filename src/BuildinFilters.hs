{-# LANGUAGE FlexibleInstances #-}

module BuildinFilters ( buildin_abs
) where

import Buildin ( Buildin(..)
               , Parameter(..)
               , param
               , ret
               , retM
               , overload
               , mkBuildin
               )
import Data.List.NonEmpty
import Value ( Value(..)
             , FromValue
             , expectInteger
             , expectFloat)
import Failure ( MonadFailure, doFail )


buildin_abs :: Buildin Value Value
buildin_abs = overload
    $  (mkBuildin Prelude.abs
            `param` (RegularParameter "x" Nothing expectInteger)
            `ret`   IntegerVal)
    <| (mkBuildin Prelude.abs
            `param` (RegularParameter "x" Nothing expectFloat)
            `ret`   FloatVal)
    :| []

