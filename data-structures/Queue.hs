{-# Language RecordWildCards #-}
{-# Language NamedFieldPuns #-}

data Queue a = Queue {
    forward :: [a]
  , reversed :: [a]
  } deriving Show

empty :: Queue a
empty = Queue {forward = [], reversed = []}

isEmpty :: Queue a -> Bool
isEmpty Queue{..} = null forward && null reversed

push :: a -> Queue a -> Queue a
push e Queue{..} = Queue{reversed=e:reversed, ..}

pop :: Queue a -> (a, Queue a)
pop Queue{forward=[], reversed=[]} = error "Empty"
pop Queue{forward=x:xs, ..} = (x, Queue{forward=xs, ..})
pop Queue{forward=[], ..} = pop Queue{forward=reverse reversed, reversed = []}

main = do
  let q1 = empty :: Queue String
  let q2 = push "Test #1" q1
  let q3 = push "Test #2" q2
  print q3
  let (e, q4) = pop q3
  print $ "pop:" ++ e
  let (e, q5) = pop q4
  print $ "pop:" ++ e
  return ()
