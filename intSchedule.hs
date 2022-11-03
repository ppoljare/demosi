import Data.List

intSchedule :: [(Int, Int)] -> [Int]
intSchedule xs = reverse $ fst $ fold_res
  where
    fold_res = foldl (\(sol,t) (i, (s,f)) -> if s >= t then (i : sol, f) else (sol,t)) ([], 0) sortedxs
    sortedxs = sortBy (\(_,x) (_,y) -> compare (snd x) (snd y)) (zip [0..] xs)