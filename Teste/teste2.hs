qsort [] = []
qsort list = qsort(filter(< head list) list) ++ filter (== head list) list ++ qsort(filter(>head list) list)
isSorted list = list == qsort list

isSorted' [x] = True
isSorted' list = 
    if head list <= list !! 1
    then isSorted' (tail list)
    else False