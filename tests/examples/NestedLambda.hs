{-# LANGUAGE ImplicitParams, NamedFieldPuns, ParallelListComp, PatternGuards #-}


getPath :: [String] -> Filter
getPath names elms =
  let follow = foldl (\f n -> \els-> subElems n $ f els) id' names :: Filter
      id' = id :: Filter
  in  follow elms

match a =
  case ?pat of
    POr ps -> choice flips $ map (\p -> \b -> let ?pat = p in match s{ flips = b }) ps
