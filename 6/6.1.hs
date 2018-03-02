data DList a = DEmpty | DCons (DList a) a (DList a)

rotateList :: DList a -> DList a
rotateList DEmpty = DEmpty
rotateList res@(DCons DEmpty _ _) = res
rotateList (DCons (DCons ll la lr) a r) = DCons ll la (DCons lr a r)

(!!) :: DList a -> Int -> a
list !! n = findNth (rotateList list) n where
              findNth DEmpty _ = error "Too short list"
              findNth (DCons _ a _) 0 = a
              findNth (DCons _ _ r) n | n < 0  = error "Incorrect parameter"
                                      | otherwise = findNth r (n-1)

ins :: DList a -> Int -> a -> DList a
ins list n x = insNth (rotateList list) n x where
                insNth DEmpty n x  = if n == 0 then DCons DEmpty x DEmpty 
                                               else error "Too short list"
                insNth (DCons l a (DCons rl ra rr)) 0 x = newNode where
                             newNode = DCons (DCons l a newNode) x (DCons newNode ra rr)
                insNth (DCons _ _ r) n x = if n < 0 then error "Incorrect parameter"
                                                    else insNth r (n-1) x

del :: DList a -> Int -> DList a
del list n = delNth (rotateList list) n where
              delNth DEmpty _ = error "Too short list"
              delNth (DCons (DCons ll la lr) a (DCons rl ra rr)) 0 = newNode where
                     newNode = DCons ll la (DCons newNode ra rr)
              delNth (DCons _ _ r) n = if n < 0 then error "Incorrect parameter"
                                                else delNth r (n-1)