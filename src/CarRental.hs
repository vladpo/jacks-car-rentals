module CarRental ( solve
                 , Policy
                 , Values
                 , solveValueIteration
                 , ValueAction
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
  type ValueAction = Map (Int, Int) (Int, Double)

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
  expectedReturn vs s a reqsRets d = foldl (\sum reqRet -> sum + singleReturn reqRet) 0.0 reqsRets
    where
      cost = -2 * abs a
      c = min (fst s - a) 20
      c' = min (snd s + a) 20
      singleReturn :: ((Int, Int), (Int, Int)) -> Double
      singleReturn ((req, req'), (ret, ret'))
        | c - req < 0 || c' -req' < 0 = 0.0
        | otherwise                   = probs*(r + 0.9*(vs ! (min (c - minreq + ret) 20, min (c' - minreq' + ret) 20)))
          where
            (minreq, minreq') = (min c req, min c' req')
            probs = (fst.fst $ d ! minreq)*(snd.fst $ d ! minreq')*(fst.snd $ d ! ret)*(fst.snd $ d ! ret')
            r = fromIntegral(cost + 10*(minreq + minreq'))

  policyImprove :: Policy -> Values -> [((Int, Int), (Int, Int))] -> Distribution -> Policy
  policyImprove p vs reqsRets d = debug(mapWithKey (\s a -> bestAction s) p)
    where
      bestAction s = fst (foldl compare (0, 0.0) [negate 4..5])
        where 
          compare :: (Int, Double) -> Int -> (Int, Double)
          compare (ba, er) a = 
            let 
              c = min (fst s - a) 20
              c' = min (snd s + a) 20
            in 
              if (c < 0 || c' < 0) then
                (ba, er)
              else
                let newEr = expectedReturn vs s a reqsRets d in if newEr > er then (a, newEr) else (ba, er)
      

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

  initValueActions :: ValueAction 
  initValueActions = initMap [0..20] $ const (0, 0.0)

  valueIteration :: ValueAction -> [((Int, Int), (Int, Int))] -> Distribution -> ValueAction
  valueIteration vas reqsRets d = mapWithKey (\s (oa, v) -> foldl argmax (0, 0.0) $ map (\a -> fmap (\s' -> (a, sum $ map (\t -> let p = prob t in  p*(cost a + return s' t + 0.9* value s' t)) reqsRets)) $ move s a) [negate 5..5]) vas
    where 
      move :: (Int, Int) -> Int -> Maybe (Int, Int)
      move (c, c') a = if c - a >= 0 && c' + a >= 0 then Just (min (c-a) 20, min (c'+a) 20) else Nothing
      value :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Double
      value (c, c') ((req, req'), (ret, ret')) = if c - req + ret < 0 || c' - req' + ret' < 0 then 0.0 else snd $ vas ! (min (c - req + ret) 20, min(c' - req' + ret') 20)
      cost a = fromIntegral $ negate 2 * abs a
      return :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Double
      return (c, c') ((req, req'), (ret, ret')) = if c >= req && c' >= req' then fromIntegral(10 * (req + req')) else 0.0
      prob :: ((Int, Int), (Int, Int)) -> Double
      prob ((req, req'), (ret, ret')) = (fst.fst $ d ! req) * (snd.fst $ d ! req') * (fst.snd $ d ! ret) * (snd.snd $ d ! ret')
      argmax :: (Int, Double) -> Maybe (Int, Double) -> (Int, Double)
      argmax (a, er) (Just (a', er')) = if er' > er then (a', er') else (a, er)
      argmax t Nothing = t

  learnVi :: ValueAction -> [((Int, Int), (Int, Int))] -> Distribution -> ValueAction
  learnVi vas reqsRets d = if totalDiff <= 0.0001 || policyUnchanged then nvas else learnVi nvas reqsRets d
    where 
      nvas = valueIteration vas reqsRets d
      totalDiff = debug(foldlWithKey (\diff s t -> diff + abs(snd t - (snd $ vas!s))) 0.0 nvas)
      policyUnchanged = foldlWithKey (\b s t -> b && fst t == (fst $ vas!s)) True nvas

  solveValueIteration :: ValueAction
  solveValueIteration = learnVi initValueActions (pairs 11) initDist