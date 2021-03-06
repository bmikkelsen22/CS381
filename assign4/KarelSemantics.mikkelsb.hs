--- mikkelsb (Brooks Mikkelsen)
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) w r = (getFacing r) == c
test (Clear d) w r = isClear (relativePos d r) w
test Beeper w r = hasBeeper (getPos r) w
test Empty w r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = let newPos = neighbor (getFacing r) (getPos r) in
      if (isClear (newPos) w)
            then OK w (newPos, (getFacing r), (getBag r))
            else Error ("Blocked at: " ++ show newPos)
stmt PutBeeper _ w r = let p = getPos r in
      if not (isEmpty r)
            then OK (incBeeper p w) (decBag r)
            else Error "No beeper to put."
stmt (Turn d) _ w r = let c = getFacing r in
      OK w (setFacing (cardTurn d c) r)
stmt (Block (h : t)) d w r = case (stmt h d w r) of
      (OK (w1) (r1)) -> stmt (Block t) d w1 r1
      (Error e) -> Error e
      (Done r) -> Done r
stmt (Block []) d w r = OK w r
stmt (If q t f) d w r = if (test q w r)
                        then stmt t d w r
                        else stmt f d w r
stmt (Call m) d w r = let optS = (lookup m d) in
      case optS of 
            Just s -> stmt s d w r
            Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) d w r = if i > 0
      then case (stmt s d w r) of
            (OK (w1) (r1)) ->  stmt (Iterate (i - 1) s) d w1 r1
            (Error e) -> Error e
            (Done r) -> Done r 
      else (OK (w) (r))
stmt (While t s) d w r = if (test t w r)
      then case (stmt s d w r) of
            (OK (w1) (r1)) -> stmt (While t s) d w1 r1
            (Error e) -> Error e
            (Done r) -> Done r
      else OK (w) (r)
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
