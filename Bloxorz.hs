{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Bloxorz where

import ProblemState

import Data.List

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Tile {cellType :: Char} |
            Switch {positionList :: [Position]} 
            deriving (Eq, Read, Ord)

instance Show Cell where
    show (Tile t) = [t]
    show (Switch _) = [switch]
{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}
won :: Char
won = 'w'

lost :: Char
lost = 'l'

continue :: Char
continue = 'c'

data Level = Level {gameMap :: A.Array Position Cell, blockState :: [Position], gameState :: Char, finalTile :: Position}   
            deriving (Eq, Ord)            

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where 
    show (Level arr bl state _) = let arrToPrint = arr A.// [(p, (Tile block)) | p <- bl] in
                                        (filter (`notElem` ",[]") $ 
                                         foldl (\x y -> (x ++ "\n" ++ y)) "" 
                                         $ map (show . (map snd)) $ groupBy (\x y -> fst (fst x) == fst (fst y)) $ A.assocs arrToPrint)
                                            ++ "\n" ++ case state of
                                                'w' -> "Congrats! You won!\n"
                                                'l' ->"Game Over\n"
                                                _ -> ""

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel p1 p2 = Level (A.listArray ((0, 0), p1) $ replicate ((fst p1 + 1) * (snd p1 + 1)) (Tile emptySpace))
                         [p2]
                         continue
                         (0, 0)

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile c p (Level arr bl state fin) = if ch == winningTile
                                        then Level (arr A.// [(p, (Tile ch))]) bl state p
                                        else Level (arr A.// [(p, (Tile ch))]) bl state fin
                                     where ch = case c of
                                                'H' -> hardTile
                                                'S' -> softTile
                                                _ -> winningTile

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch p l (Level arr bl state fin) = Level (arr A.// [(p, (Switch l))]) bl state fin

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate (Switch l) (Level arr bl state fin) = if (cellType (arr A.! (head l))) == emptySpace
                                                then (Level (arr A.// [(p, (Tile hardTile)) | p <- l]) bl state fin)
                                                else (Level (arr A.// [(p, (Tile emptySpace)) | p <- l]) bl state fin)

activate (Tile t) (Level arr bl state fin)
    | t == emptySpace = (Level arr bl lost fin)
    | t == softTile = if (length bl) == 1
                            then (Level arr bl lost fin)
                            else (Level arr bl state fin)
    | t == winningTile = if (length bl) == 1 
                            then (Level arr bl won fin)
                            else (Level arr bl state fin)
    | otherwise = (Level arr bl state fin)

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level 
move d (Level arr bl state fin)
        | state /= continue = (Level arr bl state fin) -- joc castigat sau pierdut
        | (length bl) == 1 =                       -- is vertical
                let p = head bl in case d of
                        North -> let  p1 = (fst p - 2, snd p)
                                      p2 = (fst p - 1, snd p)
                                      cell1 = arr A.! p1
                                      cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        South -> let  p1 = (fst p + 1, snd p)
                                      p2 = (fst p + 2, snd p)
                                      cell1 = arr A.! p1
                                      cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        West -> let  p1 = (fst p, snd p - 2)
                                     p2 = (fst p, snd p - 1)
                                     cell1 = arr A.! p1
                                     cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        _ -> let  p1 = (fst p, snd p + 1)
                                  p2 = (fst p, snd p + 2)
                                  cell1 = arr A.! p1
                                  cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

        | fst (head bl) == fst (head (tail bl)) =  -- orizontal pe o singura linie
                let bl1 = head bl
                    bl2 = head (tail bl) in case d of
                        North -> let p1 = (fst bl1 - 1, snd bl1)
                                     p2 = (fst bl2 - 1, snd bl2)
                                     cell1 = arr A.! p1
                                     cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        South -> let p1 = (fst bl1 + 1, snd bl1)
                                     p2 = (fst bl2 + 1, snd bl2)
                                     cell1 = arr A.! p1
                                     cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        West -> let p1 = (fst bl1, snd bl1 - 1)
                                    cell1 = arr A.! p1 in
                                        activate cell1 (Level arr [p1] state fin)

                        _ -> let p1 = (fst bl2, snd bl2 + 1)
                                 cell1 = arr A.! p1 in
                                        activate cell1 (Level arr [p1] state fin)


        | otherwise = -- orizontal pe o singura coloana
                let bl1 = head bl
                    bl2 = head (tail bl) in case d of
                        North -> let p1 = (fst bl1 - 1, snd bl1)
                                     cell1 = arr A.! p1 in
                                        activate cell1 (Level arr [p1] state fin)

                        South -> let p1 = (fst bl2 + 1, snd bl2)
                                     cell1 = arr A.! p1 in
                                        activate cell1 (Level arr [p1] state fin)

                        West -> let p1 = (fst bl1, snd bl1 - 1)
                                    p2 = (fst bl2, snd bl2 - 1)
                                    cell1 = arr A.! p1
                                    cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

                        _ -> let p1 = (fst bl1, snd bl1 + 1)
                                 p2 = (fst bl2, snd bl2 + 1)
                                 cell1 = arr A.! p1
                                 cell2 = arr A.! p2 in
                                        activate cell2 (activate cell1 (Level arr [p1, p2] state fin))

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level _ _ state _) = state == continue

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors lvl = if (gameState lvl) == won
                        then []
                        else filter (\p -> gameState (snd p) /= lost) 
                        [(North, move North lvl), (South, move South lvl), (West, move West lvl), (East, move East lvl)]

    isGoal lvl = (gameState lvl) == won

    -- Doar petru BONUS
    heuristic (Level _ bl _ fin) 
        | length bl == 1 = abs (fst fin - fst (head bl)) + abs (snd fin - snd (head bl))
        | otherwise = let d1 = abs (fst fin - fst (head bl)) + abs (snd fin - snd (head bl))
                          d2 = abs (fst fin - fst (bl !! 1)) + abs (snd fin - snd (bl !! 1)) in
                          min d1 d2
