{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import Data.List
import Data.Maybe

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = MyNode {state :: s
                , action :: Maybe a
                , parent :: Node s a
                , children :: [Node s a]} 
                | Nil
                deriving (Eq, Ord, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState = state

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

helper1 :: ProblemState b a => b -> a -> Node b a -> Node b a 
helper1 st act prnt = MyNode st (Just act) prnt [helper1 (snd pair) (fst pair) (helper1 st act prnt) | pair <- successors st]

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace initial = MyNode initial Nothing Nil [helper1 (snd pair) (fst pair) (createStateSpace initial) | pair <- successors initial]

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace (MyNode st act prnt kids) = (MyNode st act prnt (map orderStateSpace (sortBy f kids)))
    where f = \n1 n2 -> compare (heuristic (nodeState n1)) (heuristic (nodeState n2))

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

helper2 :: (ProblemState s a, Ord s) => Node s a -> Int -> [Node s a] -> [Node s a]
helper2 node depth nodeList =
    if depth == 0 
        then nodeList
        else foldl (\nL nod -> if find (== (nodeState nod)) (map nodeState nL) == Nothing
                                then helper2 nod (depth - 1) (nL ++ [nod])
                                else nL) 
                    nodeList (children node)

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs node dpth = helper2 node dpth [node]

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s, Eq a)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening node =
    let dfsTries = takeWhile (\x -> (find (isGoal . nodeState) x) == Nothing) [limitedDfs node d | d <- [0..]]
        lastDfs = limitedDfs node (length dfsTries) -- lista intoarsa la ultimul Dfs (cel in care gaseste nod final)
        nrStates = (foldl (+) 0 (map length dfsTries)) + (length (fst (break (isGoal . nodeState) lastDfs))) 
        finalNode = fromJust (find (isGoal . nodeState) lastDfs)
    in (finalNode, nrStates)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: (ProblemState s a, Ord s, Eq a) => Node s a -> [(a, s)]
extractPath node = map (\n -> (fromJust (action n), nodeState n)) $ reverse $ takeWhile ((Nil /=) . parent) (iterate parent node)

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve initial useH = if useH == False
                      then extractPath (fst (iterativeDeepening (createStateSpace initial)))
                      else extractPath (fst (iterativeDeepening (orderStateSpace (createStateSpace initial))))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))