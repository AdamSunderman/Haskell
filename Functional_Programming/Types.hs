Given:

  data Maybe a = Nothing | Just a

  gt   :: Int -> Int -> Bool
  not  :: Bool -> Bool
  even :: Int -> Bool
  map  :: (a -> b) -> [a] -> [b]
  (.)  :: (b -> c) -> (a -> b) -> a -> c

What are the types of the following expressions?
 
 * Just
   :: a -> Maybe a


 * not even 3   -- (not even) 3
   not even :: ??
     Targ = Bool, Tres = Bool
     Bool =? Int -> Bool  -- TYPE ERROR!


 * not (even 3)
   even 3 :: Bool
     Targ = Int, Tres = Bool
     Int =? Int
   not (even 3) :: ** Bool **
     Targ = Bool, Tres Bool
     Bool =? Bool

 * gt 3 :: Int -> Bool
     Targ = Int, Tres = Int -> Bool
     Int =? Int


 * map 3


 * map even

 
 * not . even


 * even . not


 * map (Just . even)
