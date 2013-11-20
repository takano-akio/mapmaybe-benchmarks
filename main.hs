import Criterion.Main

import Control.DeepSeq
import qualified Data.Maybe as B
import qualified MapMaybe as F

main = do
  rnf ints `seq` return ()
  defaultMain
    [ bgroup "simple"
      [ bench "base" $ nf (B.mapMaybe half) ints
      , bench "fold" $ nf (F.mapMaybe half) ints
      ]
    , bgroup "after map"
      [ bench "base" $ nf (B.mapMaybe half . map (+1)) ints
      , bench "fold" $ nf (F.mapMaybe half . map (+1)) ints
      ]
    , bgroup "after filter"
      [ bench "base" $ nf (B.mapMaybe half . filter seven) ints
      , bench "fold" $ nf (F.mapMaybe half . filter seven) ints
      ]
    , bgroup "before map"
      [ bench "base" $ nf (map (+1) . B.mapMaybe half) ints
      , bench "fold" $ nf (map (+1) . F.mapMaybe half) ints
      ]
    ]

ints :: [Int]
ints = [-2 .. 8000]

half :: Int -> Maybe Int
half n = if even n then Just $! div n 2 else Nothing

seven :: Int -> Bool
seven x = quot x 10 == 7
