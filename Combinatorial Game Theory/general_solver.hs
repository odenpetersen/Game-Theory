{-
Solves a general combinatorial game.
-}


--module CGTSolve (solveGame) where

import Prelude hiding (filter)
import Data.Set

-- Returns the set of all optimal moves
solveGame :: Eq a => (a -> Set a) -> a -> a -> Set a
solveGame reachable winState curState =
		 filter optimal $ reachable curState
		 where
		 		optimal s = (s == winState) || losingState s
				losingState s = isEmpty $ solveGame reachable winState s
				isEmpty = (==empty)

{-
-- One-pile Nim where either one or two tokens can be taken
nimReachable :: Int -> Set Int
nimReachable x 	| x <= 2
									= singleton 0
								| otherwise
									= fromList [x-1,x-2]

main	=	do
					print $ solveGame nimReachable 0 8
-}
