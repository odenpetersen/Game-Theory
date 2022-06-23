import Control.Monad.Fix

{-
Stochastic Nim:

In front of the players is a pile of tokens.

On their turn, each player takes either one or two tokens from the pile, then flips a coin. If it is heads, they take one additional token.

The player to take the last token wins.
-}

-- p(n) = 1 - min(p(n-1)+p(n-2), p(n-2)+p(n-3))/2
recurrence :: [Float] -> [Float]
recurrence xs = map (\x -> 1 - x) complement
			where
					complement = zipWith min takeOne takeTwo
					takeOne = map (/2) $ zipWith (+) minusOne minusTwo
					takeTwo = map (/2) $ zipWith (+) minusTwo minusThree
					minusOne = 0:xs
					minusTwo = 0:0:xs
					minusThree = 0:0:0:xs

getMoves :: [Float] -> [Bool]
getMoves xs = zipWith (<) minusOne minusThree
			where
					minusOne = 0:xs
					minusThree = 0:0:0:xs

moves = take 30 $ getMoves $ fix recurrence

main = print $ take 40 $ fix recurrence
