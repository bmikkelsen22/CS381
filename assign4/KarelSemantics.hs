module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) w (rp rc _) = rc == c
test (Clear d) w r = isClear (relativePos d r) w
test Beeper w r = hasBeeper (getPos r) w
test Empty w (_ _ c) = c == 0

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w (rp rc ri) = let newPos = neighbor rc rp in
      if (isClear (newPos) w)
            then OK w (newPos rc ri)
            else Error (show newPos ++ " is out of bounds")
stmt PutBeeper _ w r = let p = getPos r in
      if not (isEmpty r)
            then OK (incBeeper p w) (decBag r)
            else Error "No beepers in bag to place"
stmt (Turn d) _ w r = let c = getFacing r in
      OK w (setFacing (cardTurn d c) r)
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r