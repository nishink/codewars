-- https://www.codewars.com/kata/5a5f9f80f5dc3f942b002309
-- Schrödinger's Boolean

module Solution where

data OmniBool = Schrodinger Bool

(==) :: OmniBool -> Bool -> Bool
(==) (Schrodinger _) _ = True

omniBool = Schrodinger True

