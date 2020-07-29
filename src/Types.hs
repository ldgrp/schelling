module Types where 

data State s a =
    State { runState :: (s -> (a, s)) }

instance Functor (State s) where
    fmap f (State state) = 
        State (\s ->
            let (a, s') = state s
             in (f a, s')
        )

instance Applicative (State s) where
    pure a = State (\s -> (a, s))

    State f <*> State state = 
        State (\s ->
            let (f', s') = f s
                (a, s'') = state s'
             in (f' a, s'')
        )

evalState state s = fst (runState state s)


