---  Jose Manuel Beauregard Mendez     A01021716
--factorialTail x = ft (x-1) x
--    where ft n res
--            | n==0      = res
--            | otherwise = ft (n-1) (res*n)
--

--reverseList list = rl list []
--    where rl list rev
--            | list==[] =rev
--            | otherwise = rl (init list) (rev ++ [last list])
--

maxMin list = mm list (head list) (head list) (length list)
    where mm list mini maxi cont
            | cont==0 =(mini,maxi)
            | (head list) > maxi =mm list mini (head list) (cont)
            | (head list) < mini =mm list (head list) maxi (cont)
            | otherwise = mm (tail list) mini maxi (cont-1)
