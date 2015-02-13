import Test.QuickCheck

data Arbre coul val = Feuille  
                    | Noeud coul val (Arbre coul val) (Arbre coul val)
                    deriving Show

monArbre = Noeud 0 1 ( Noeud 0 2 (Noeud 0 3 Feuille Feuille) (Noeud 0 4 Feuille (Noeud 0 5 Feuille Feuille))) (Noeud 0 2 Feuille Feuille) 

mapArbre :: ( a -> b ) -> Arbre coul a -> Arbre coul b
mapArbre _ Feuille                = Feuille 
mapArbre f (Noeud coul val a1 a2) = Noeud coul (f val) (mapArbre f a1) (mapArbre f a2 )                  

foldArbre :: ( a -> a -> a ) -> a -> Arbre coul a -> a
foldArbre op x Feuille             = x
foldArbre op x (Noeud _ val a1 a2) = (val `op` (foldArbre op x a1)) `op` (foldArbre op x a2)

hauteur :: Arbre coul a -> Int
hauteur Feuille           = 0
hauteur (Noeud _ _ a1 a2) = 1 + (max (hauteur a1) (hauteur a2))

hauteur' :: Arbre coul a -> Int
hauteur' arb = foldArbre (\x l -> max (x+1) l ) 0 (mapArbre (\x -> 0) arb)

taille :: Arbre coul a -> Int
taille Feuille           = 0
taille (Noeud _ _ a1 a2) = 1 + (taille a1) + (taille a2) 

taille' :: Arbre coul a -> Int
taille' arb = foldArbre (+) 0 (mapArbre (\x -> 1) arb)

peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche []         = Feuille
peigneGauche ((c,a):xs) = Noeud c a (peigneGauche xs) Feuille

estComplet :: Arbre c a -> Bool 
estComplet Feuille           = True
estComplet (Noeud _ _ a1 a2) = (hauteur a1 == hauteur a2) && (estComplet a1) && (estComplet a2)

--estComplet' :: Arbre c a -> Bool
--estComplet' a = foldArbre (\x y ->  ) True a

complet :: Int -> [(c, a)] -> Arbre c a 
complet 0 _          = Feuille
complet _ []         = Feuille
complet n xs         = Noeud (fst (xs !! mil)) (snd(xs !! mil)) (complet (n-1) (take mil xs)) (complet (n-1) (drop (mil+1) xs)) 
                      where mil = ((length xs) `div` 2) 

repeat' x = iterate id x

arbreinfini = map (\x-> ((),x)) ['a' ..] 

aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille           = []
aplatit (Noeud c a e1 e2) = (aplatit e1) ++ [(c,a)] ++ (aplatit e2)

element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element a (Noeud _ b e1 e2) | a == b    = True
                            | otherwise = element a e1 || element a e2  
                            
noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud fc fa (c,a) = (fa a) ++ " " ++ (fc c) ++ "\n" 

--colorToString :: String -> String
colorToString c = "[color=" ++ c ++ ", fontcolor=" ++ c ++"]"

--valToString :: Char -> String
valToString x = x : " "

valNoeud :: Noeud c a n1 n2 -> a
valNoeud (Noeud _ a _ _) = a

arcs :: Arbre c a -> [(a,a)]
arcs Feuille                      = []
arcs (Noeud _ _ Feuille Feuille)  = []
arcs (Noeud _ a e1 e2)            = (a, valNoeud e1) : (a, valNoeud e2) : ((arcs e1) ++ (arcs e2))

--Propriétés
--Propriété vérifiant si la hauteur du peigne gauche est bien celle de la liste qu'on a passé en paramètre
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)

prop_hauteur'Peigne xs = length xs == hauteur' (peigneGauche xs)

prop_taillePeigne xs = length xs == taille (peigneGauche xs)

prop_taille'Peigne xs = length xs == taille' (peigneGauche xs)

prop_foldPeigne xs = (foldr (\(x,y) xs'-> y+xs')  0 xs) == (foldArbre (+) 0 (peigneGauche xs))

prop_foldmapPeigne xs = (foldr (+) 0 (map (\x ->1) xs)) == (foldArbre (+) 0 (mapArbre (\x -> 1) (peigneGauche xs)))

--Nous profitons que la fonction s'arrêtre si on a une liste vide
prop_aplatitgardeordre xs = xs == aplatit ( complet (length xs) xs)
