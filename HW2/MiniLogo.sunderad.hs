--    Adam Sunderman   sunderad
module MiniLogo where

import Prelude hiding (Num)
import Data.List

{-
    Part 1 

    Define the abstract syntax of MiniLogo as a set of Haskell data types. 
    You should use built-in types for num, var, and macro. (If you want to 
    define a type Num, you will have to hide that name from the Prelude).
-}
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up 
          | Down
          deriving(Show, Eq)

data Expr =  Ref Var
           | Num Int
           | Add Expr Expr
           deriving(Show, Eq)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving(Show, Eq)

{-
    Part 2

    Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere 
    on the canvas) draws a line segment from (x1,y1) to (x2,y2). Write the macro 
    in MiniLogo concrete syntax (i.e. the notation defined by the grammar and 
    used in the example programs above). Include this definition in a comment 
    in your submission. Encode the macro definition as a Haskell value using 
    the data types defined in Task 1. This corresponds to the abstract syntax of 
    MiniLogo. Your Haskell definition should start with something like line = 
    Define "line" ...

    MiniLogo Concrete Syntax:

    define line (x1,y1,x2,y2){
	   pen up; move (x1,y1);
	   pen down; move (x2,y2);
    }

-}

line = Define "line" ["x1","y1","x2","y2"] [Pen Up, Move (Ref "x1",Ref "y1"), Pen Down, Move (Ref "x2", Ref "y2")] 

{-
    Part 3

    Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h)
    that draws a big “X” of width w and height h, starting from position (x,y). Your 
    definition should not contain any move commands. Write the macro in MiniLogo 
    concrete syntax and include this definition in a comment in your submission.
    Encode the macro definition as a Haskell value, representing the abstract syntax 
    of the definition.

    MiniLogo Concrete Syntax:

    define nix (x,y,w,h){
       line (x,y,x+w,y+h);
       line (x+w, y, x, y+h);
	   
    }
-}

nix = Define "nix" ["x","y","w","h"] [ 
           Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], 
           Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]]

{-
	Part 4

    Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program 
    that draws a staircase of n steps starting from (0,0).
-}

steps :: Int -> Prog
steps x
    | x <= 0 = []
    | otherwise = [Call "line" [Num x, Num x, Num (x - 1), Num x],
                   Call "line" [Num (x - 1), Num x, Num (x - 1), Num (x - 1)]] ++ steps (x - 1)

{-
    Part 5

    Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names 
    of all of the macros that are defined anywhere in a given MiniLogo program. Don’t worry 
    about duplicates—if a macro is defined more than once, the resulting list may include 
    multiple copies of its name.
-}

macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
  Define m _ _ -> m:macros xs
  otherwise -> macros xs


{-
    Part 6

    Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program. 
    That is, it transforms the abstract syntax (a Haskell value) into nicely formatted concrete 
    syntax (a string of characters). Your pretty-printed program should look similar to the 
    example programs given above; however, for simplicity you will probably want to print just 
    one command per line.
-}

pretty :: Prog -> String
pretty [] = ""
pretty (Pen Up:xs) = "pen up; " ++ pretty xs
pretty (Pen Down:xs) = "pen down; " ++ pretty xs
pretty (Move (x, y):xs) = "move (" ++ parseExpr x ++ ", " ++ parseExpr y ++ "); " ++ pretty xs
pretty (Call m xs:xss) = m ++ "(" ++ intercalate ", " (map parseExpr xs) ++ "); " ++ pretty xss
pretty (Define m vs x:xs) = "define " ++ m ++ "(" ++ intercalate ", " vs ++ ") {" ++ pretty x ++ "}; " ++ pretty xs

parseExpr :: Expr -> String
parseExpr (Num n) = show n
parseExpr (Ref r) = r
parseExpr (Add x y) = parseExpr x ++ " + " ++ parseExpr y 

{-
    Opt E

    Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions 
    by replacing any additions of literals with the result. For example, given the 
    expression (2+3)+x, optE should return the expression 5+x.
-}

-- Note: I think I may have misunderstood how to handle additions in the program above.
-- Therfore the function below is probably wrong but it works with expressions such as ...
-- optE Add(Num 5 Num 6) -> Num 11 

optE :: Expr -> Expr
optE (Add (Num x) (Num y)) = Num (x + y)
optE otherwise = otherwise

{-
    Opt F

    Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions 
    contained in a given program using optE.
-}









