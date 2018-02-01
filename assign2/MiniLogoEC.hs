-- Brooks Mikkelsen (mikkelsb)
import Data.List
import Prelude hiding (Num)

-- Part 1
type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down | Up

data Expr = V Var
          | N Num
          | Add Expr Expr

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]

-- Part 2 --

-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
--     pen up;
-- }

line = Define "line" ["x1", "x2", "x3", "x4"] [Pen Up, Move (V "x1") (V "y1"), Pen Down, Move (V "x2") (V "y2"), Pen Up]

-- Part 3 --

-- define nix (x, y, w, h) {
--     call line (x, y, x + w, y + h);
--     call line (x, y + h, x + w, y);
-- }

nix = Define "nix" ["x", "y", "w", "h"] [Call "line" [V "x", V "y", Add (V "x") (V "w"), Add (V "y") (V "h")], Call "line" [V "x", Add (V "y") (V "h"), Add (V "x") (V "w"), (V "y")]]

-- Part 4 --
steps :: Int -> Prog
steps 0 = [Pen Down]
steps n = steps (n - 1) ++ [Move (N (n - 1)) (N n), Move (N n) (N n)]

-- Part 5 --
macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ _) : t) = m : (macros t)

-- Part 6 --
pretty :: Prog -> String
pretty [] = ""
pretty ((Pen m) : t) = "pen " ++ (prettyMode m) ++ ";\n" ++ (pretty t)
pretty ((Move e1 e2) : t) = "move (" ++ (prettyExpr e1) ++ ", " ++ (prettyExpr e2) ++ ");\n" ++ (pretty t)
pretty ((Define m vars prog) : t) = "define " ++ m ++ " (" ++ (intercalate ", " vars) ++ ") {\n" ++ (pretty prog) ++ "}\n" ++ (pretty t)
pretty ((Call m exprs) : t) = "call " ++ m ++ " (" ++ (intercalate ", " (map prettyExpr exprs)) ++ ");\n" ++ (pretty t)

prettyMode :: Mode -> String
prettyMode Up = "up"
prettyMode Down = "down"

prettyExpr :: Expr -> String
prettyExpr (V var) = var
prettyExpr (N num) = show num
prettyExpr (Add e1 e2) = (prettyExpr e1) ++ "+" ++ (prettyExpr e2)

optE :: Expr -> Expr
optE (Add (N n1) (N n2)) = N (n1 + n2)
optE (Add e1 e2) = Add (optE e1) (optE e2)
optE e = e

optP :: Prog -> Prog
optP [] = []
optP ((Move e1 e2) : t) = Move ((optE e1) (optE e2)) : (optP t)
optP ((Call m exprs) : t) = Call (m (map optE exprs)) : (optP t)
optP (h : t) = h : (optP t)
