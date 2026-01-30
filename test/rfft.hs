import Data.Complex
import Data.List

data Direction = O_O | X_X
    deriving Eq

infixl 4 <:
(<:) :: [Complex Double] -> Direction -> [Complex Double]
(<:) [] _   = []
(<:) [x] _  = [x]
(<:) preXs inv  = rs
    where
        preN    = length preXs
        n   = until (>= preN) (*2) 1
        xs  = take n $ preXs ++ repeat 0.0

        rotateSign  = if inv == X_X then (-1.0) else 1.0
        operXs  = [ exp (0 :+ (-rotateSign) * 2 * pi * fromIntegral k / fromIntegral n)
                  | k <- [0 .. n `div` 2 - 1]
                  ] :: [Complex Double]

        split = go [] []
            where
                go evens odds []    = (reverse evens, reverse odds)
                go evens odds [x]   = (reverse (x:evens), reverse odds)
                go evens odds (x:y:rest)    = go (x:evens) (y:odds) rest
        (evenXs, oddXs) = split xs

        evenRs  = evenXs <: O_O
        oddRs   = zipWith (*) operXs (oddXs <: O_O)
        concRs  = zipWith (+) evenRs oddRs ++ zipWith (-) evenRs oddRs
        rs  = if inv == X_X then map (\z -> z / (fromIntegral n :+ 0)) concRs else concRs

fastMul :: [Int] -> [Int] -> [Int]
fastMul as bs   = ret
  where
    len = length as + length bs - 1
    padLen  = until (>= len) (*2) 1
    pad xs  = take padLen (map ((:+ 0) . fromIntegral) xs ++ repeat 0)
    x   = pad as
    y   = pad bs
    z   = zipWith (*) (x <: O_O) (y <: O_O) <: X_X
    ret = take len $ map (round . realPart) z

slowMul :: [Int] -> [Int] -> [Int]
slowMul as bs   = ret
  where
    len = length as + length bs - 1
    pad zs  = take len (zs ++ repeat 0)
    terms   = [shift i (map (a*) bs) | (i, a) <- zip [0..] as]
    shift i xs  = replicate i 0 ++ xs
    addPoly xs ys   = zipWith (+) (pad xs) (pad ys)
    ret = foldl addPoly (replicate len 0) terms
