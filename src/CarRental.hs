module CarRental ( solve
                 , Policy
                 , Values
                 ) where

  import Debug.Trace as T (trace)
  import Data.Map.Strict as Map ( mapWithKey
                                , foldlWithKey
                                , (!)
                                , Map
                                , empty
                                , insert
                                )

  data Location = Cluj | Mures deriving (Eq)
  type Policy = Map (Int, Int) Int
  type Values = Map (Int, Int) Double
  type Distribution = Map Int ((Double, Double), (Double, Double))

  debug :: Show a => a -> a
  debug a | T.trace (show a) False = undefined
  debug a = a

  minMaybe :: Maybe Int -> Int -> Maybe Int
  minMaybe mj i = fmap (\j -> min i j) mj

  combinations :: [a] -> [[a]]
  combinations xs = flip mapM [0..1] $ const xs
  
  pairs :: Int -> [((Int, Int), (Int, Int))]
  pairs n = f (combinations $ f (combinations [0..n]))
    where 
      f :: [[a]] -> [(a,a)]
      f = foldl (\tl ls -> (ls!!0, ls!!1):tl) []

  initMap ::  [Int] -> ((Int,Int) -> a) -> Map (Int, Int) a
  initMap ls f = foldl (\m i -> foldl (\m' j -> insert (i, j) (f(i,j)) m') m ls) empty ls

  initValues :: Values
  initValues = initMap [0..20] $ const 0.0

  initPolicy :: Policy
  initPolicy = initMap [0..20] $ const 0

  poissonProb :: Int -> Int -> Double
  poissonProb lambda n = num/denom
    where 
      num :: Double
      num = fromIntegral (lambda^n)
      denom :: Double
      denom = 2.71828 * (fromIntegral lambda) * fromIntegral (product [1..n])

  initDist :: Distribution
  initDist = foldl (\d n -> insert n (reqRetPoisson Cluj n, reqRetPoisson Mures n) d) empty [0..11]
    where
      lreq l = if l == Cluj then 4 else 3
      lret l = if l == Cluj then 2 else 3
      reqRetPoisson l n = (poissonProb (lreq l) n, poissonProb (lret l) n)

  expectedReturn :: Values -> (Int, Int) -> Int -> [((Int, Int), (Int, Int))] -> Distribution -> Double
  expectedReturn vs s a reqsRets d = 
    if (c < 0 || c' < 0) then
      -1.0
    else
      maybe (negate 1.0) id $ foldl (\sum reqRet -> (+) <$> sum <*> singleReturn reqRet) (Just 0.0) reqsRets
    where
      cost = -2 * abs a
      c = min (fst s - a) 20
      c' = min (snd s + a) 20
      singleReturn :: ((Int, Int), (Int, Int)) -> Maybe Double
      singleReturn ((req, req'), (ret, ret'))
        | c - req < 0 || c' -req' < 0 = Nothing
        | otherwise                   = Just(probs*(r + 0.9*(vs ! (min (c - minreq + ret) 20, min (c' - minreq' + ret) 20))))
          where
            (minreq, minreq') = (min c req, min c' req')
            probs = (fst.fst $ d ! minreq)*(fst.snd $ d ! minreq')*(snd.fst $ d ! ret)*(snd.snd $ d ! ret')
            r = fromIntegral(cost + 10*(minreq + minreq'))

  policyImprove :: Policy -> Values -> [((Int, Int), (Int, Int))] -> Distribution -> Policy
  policyImprove p vs reqsRets d = debug(mapWithKey (\s a -> bestAction s) p)
    where
      bestAction s = fst (foldl compare (negate 5, expectedReturn vs s (negate 5) reqsRets d) [negate 4..5])
        where 
          compare :: (Int, Double) -> Int -> (Int, Double)
          compare (ba, er) a = let newEr = expectedReturn vs s a reqsRets d in if newEr > er then (a, newEr) else (ba, er)
      

  policyEvaluation :: Policy -> Values -> [((Int, Int), (Int, Int))] -> Distribution -> Values
  policyEvaluation p vs reqsRets d = mapWithKey (\s v -> expectedReturn vs s (p ! s) reqsRets d) vs
      
  learn :: (Policy, Values) -> [((Int, Int), (Int, Int))] -> Distribution -> (Policy, Values)
  learn (p, vs) reqsRets d = 
    if diff <= 0.0001 then
      (np, nvs)
    else
      learn (np, nvs) reqsRets d
    where 
      nvs = policyEvaluation p vs reqsRets d
      np = policyImprove p nvs reqsRets d
      diff = debug(foldlWithKey (\sum s v -> sum + abs (v - vs!s)) 0.0 nvs)

  solve :: (Policy, Values)
  solve = learn (initPolicy, initValues) (pairs 11) initDist