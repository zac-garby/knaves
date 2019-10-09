module Naives where
    
import Data.List
import Data.Maybe

type ID = String
type Problem = [(ID, Prop)]

data Prop
    = V ID
    | And Prop Prop
    | Or Prop Prop
    | Not Prop
    | If Prop Prop
    | Eq Prop Prop
    | F
    | T

(&&&) = And
(|||) = Or
(==>) = If
(<=>) = Eq

instance Show Prop where
    show (V i) = i
    show (And a b) = "(" ++ show a ++ " ∩ " ++ show b ++ ")"
    show (Or a b) = "(" ++ show a ++ " ∪ " ++ show b ++ ")"
    show (Not a) = "¬" ++ show a
    show (If a b) = "(" ++ show a ++ " ⇒ " ++ show b ++ ")"
    show (Eq a b) = "(" ++ show a ++ " ⇔ " ++ show b ++ ")"
    show F = "⊥"
    show T = "⊤"

evaluate :: [(ID, Bool)] -> Prop -> Maybe Bool
evaluate e (V i) = lookup i e
evaluate e (And a b) = (&&) <$> evaluate e a <*> evaluate e b
evaluate e (Or a b) = (||) <$> evaluate e a <*> evaluate e b
evaluate e (Not a) = not <$> evaluate e a
evaluate e (If a b) = (\x y -> y || not x) <$> evaluate e a <*> evaluate e b
evaluate e (Eq a b) = (==) <$> evaluate e a <*> evaluate e b
evaluate _ F = Just False
evaluate _ T = Just True

variables :: Prop -> [ID]
variables (V i) = [i]
variables (And a b) = nub $ variables a ++ variables b
variables (Or a b) = nub $ variables a ++ variables b
variables (Not a) = variables a
variables (If a b) = nub $ variables a ++ variables b
variables (Eq a b) = nub $ variables a ++ variables b
variables _ = []

cases :: Int -> [[Bool]]
cases 0 = []
cases 1 = [[False], [True]]
cases n = ((False:) <$> prev) ++ ((True:) <$> prev)
    where prev = cases (n - 1)

tautology :: Prop -> Bool
tautology p = all (truthy . (`evaluate` p)) $ map (zip vars) (cases $ length vars)
    where vars = variables p
          truthy (Just b) = b
          truthy Nothing = False

truthTable :: Prop -> [([(ID, Bool)], Bool)]
truthTable p = map (\e -> (e, fromJust $ evaluate e p)) (map (zip vars) (cases $ length vars))
    where vars = variables p

sat :: Prop -> [[(ID, Bool)]]
sat = map fst . filter snd . truthTable

solve :: Problem -> [[(ID, Bool)]]
solve [] = []
solve pr = sat expr
  where props = map (\(i, p) -> V i <=> p) pr
        expr = foldr1 (&&&) props

{-
 Alice: If Bob lies, then so does Carol.
 Bob: Alice and Carol are not both lying.
 Carol: Neither Alice nor Bob are truthful.
-}
prob1 =
  [ ("A", Not (V "B") ==> Not (V "C"))
  , ("B", V "A" ||| V "C")
  , ("C", Not (V "A") &&& Not (V "B"))
  ]

{-
 Troll 1: If I am a knave, then there are exactly two knights here.
 Troll 2: Troll 1 is lying.
 Troll 3: Either we are all knaves, or at least one of us is a knight.
-}
prob2 =
  [ ("1", Not (V "1") ==> (V "2" &&& V "3"))
  , ("2", Not (V "1"))
  , ("3", (Not (V "1") &&& Not (V "2") &&& Not (V "3")) ||| (V "1" ||| V "2" ||| V "3"))
  ]
