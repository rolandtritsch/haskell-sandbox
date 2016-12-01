-- Guards.hs
module Guards where

myAbs x
  | x < 0 = (-x)
  | otherwise = x

data BloodPressure =
  HIGH |
  NORMAL |
  LOW
  deriving (Eq, Ord, Show)

measureBloodPressure p
  | p < 135 = LOW
  | p > 145 = HIGH
  | otherwise = NORMAL

isRightTriangle a b c
  | a^2 + b^2 == c^2 = True
  | otherwise = False

dogYrs age
  | age <= 0 = 0
  | age <= 1 = age * 15
  | age <= 2 = age * 12
  | age <= 4 = age * 8
  | otherwise = age * 6

data Grades = A | B | C | D | E | F deriving (Eq, Ord, Show)

grading rightAnswers numberOfQuestions
  | percentage >= 0.9 = A
  | percentage >= 0.8 = B
  | percentage >= 0.7 = C
  | percentage >= 0.6 = D
  | percentage >= 0.5 = E
  | percentage <  0.5 = F
  where percentage = rightAnswers / numberOfQuestions
