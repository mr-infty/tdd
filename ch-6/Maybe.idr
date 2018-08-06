maybeAdd1: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd1 x y = case x of
                     Nothing => Nothing
                     Just x' => (case y of
                                       Nothing => Nothing
                                       Just y' => Just (x' + y'))

maybeAdd2: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd2 x y = x >>= \x' =>
                y >>= \y' =>
                      Just (x' + y')

maybeAdd3: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd3 x y = do x' <- x
                   y' <- y
                   Just (x' + y')


