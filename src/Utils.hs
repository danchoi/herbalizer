{- arch-tag: List utilities main file

Dan Choi: This is extracted from MissingH because the library as a whole has
compile problems on Ubuntu

Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : Data.List.Utils - renamed Utils by Dan Choi for herbalizer
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with lists.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Utils(-- * Merging
                     merge, mergeBy,
                     -- * Tests
                     startswith, endswith, contains, hasAny,
                     -- * Association List Utilities
                     {- | These functions are designed to augment the
                     association list functions in "Data.List" and
                     provide an interface similar to "Data.FiniteMap" or
                     "Data.Map"
                     for association lists. -}
                     addToAL, delFromAL, flipAL, keysAL, valuesAL,
                     hasKeyAL,
                     -- ** Association List Conversions
                     strFromAL,
                     strToAL,
                     -- * Conversions
                     split, join, replace, genericJoin, takeWhileList,
                     dropWhileList, spanList, breakList,
                     -- ** Advanced Conversions
                     WholeFunc(..), wholeMap, fixedWidth,
                     -- * Fixed-Width and State Monad Utilities
                     grab,
                     -- * Miscellaneous
                     countElem, elemRIndex, alwaysElemRIndex, seqList,
                     subIndex, uniq
                     -- -- * Sub-List Selection
                     -- sub,
                    ) where
import Data.List(intersperse, concat, isPrefixOf, isSuffixOf, elemIndices,
                elemIndex, elemIndices, tails, find, findIndex, isInfixOf, nub)
import Control.Monad.State(State, get, put)
import Data.Maybe(isJust)


{- | Merge two sorted lists into a single, sorted whole.

Example:

> merge [1,3,5] [1,2,4,6] -> [1,1,2,3,4,5,6]

QuickCheck test property:

prop_merge xs ys =
    merge (sort xs) (sort ys) == sort (xs ++ ys)
          where types = xs :: [Int]
-}
merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy (compare)

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp [] ys = ys
mergeBy cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

{- | Returns true if the given list starts with the specified elements;
false otherwise.  (This is an alias for "Data.List.isPrefixOf".)

Example:

> startswith "He" "Hello" -> True

-}

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

{- | Returns true if the given list ends with the specified elements;
false otherwise.  (This is an alias for "Data.List.isSuffixOf".)

Example:

> endswith "lo" "Hello" -> True

-}
endswith :: Eq a => [a] -> [a] -> Bool
endswith = isSuffixOf

{- | Returns true if the given list contains any of the elements in the search
list. -}
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _ = False             -- An empty search list: always false
hasAny _ [] = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

{- | Similar to Data.List.takeWhile, takes elements while the func is true.
The function is given the remainder of the list to examine. -}
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList func list@(x:xs) =
    if func list
       then x : takeWhileList func xs
       else []

{- | Similar to Data.List.dropWhile, drops elements while the func is true.
The function is given the remainder of the list to examine. -}
dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList func list@(x:xs) =
    if func list
       then dropWhileList func xs
       else list

{- | Similar to Data.List.span, but performs the test on the entire remaining
list instead of just one element.

@spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@
-}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a delimiter and a list (or string), split into components.

Example:

> split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]

> split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
-}
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)


{- | Given a list and a replacement list, replaces each occurance of the search
list with the replacement list in the operation list.

Example:

>replace "," "." "127,0,0,1" -> "127.0.0.1"

This could logically be thought of as:

>replace old new l = join new . split old $ l
-}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

{- | Like 'join', but works with a list of anything showable, converting
it to a String.

Examples:

> genericJoin ", " [1, 2, 3, 4] -> "1, 2, 3, 4"
> genericJoin "|" ["foo", "bar", "baz"] -> "\"foo\"|\"bar\"|\"baz\""

-}
genericJoin :: Show a => String -> [a] -> String
genericJoin delim l = join delim (map show l)

{-# DEPRECATED contains "Use Data.List.isInfixOf, will be removed in MissingH 1.1.0" #-}
{- | Returns true if the given parameter is a sublist of the given list;
false otherwise.

Example:

> contains "Haskell" "I really like Haskell." -> True
> contains "Haskell" "OCaml is great." -> False

This function was submitted to GHC and was applied as
'Data.List.isInfixOf'.  This function therefore is deprecated and will
be removed in future versions.
-}

contains :: Eq a => [a] -> [a] -> Bool
contains = isInfixOf

-- above function submitted to GHC as Data.List.isInfixOf on 8/31/2006

{- | Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{- | Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

{- | Returns the keys that comprise the (key, value) pairs of the given AL.

Same as:

>map fst
-}
keysAL :: [(key, a)] -> [key]
keysAL = map fst

{- | Returns the values the comprise the (key, value) pairs of the given
AL.

Same as:

>map snd
-}
valuesAL :: [(a, value)] -> [value]
valuesAL = map snd

{- | Indicates whether or not the given key is in the AL. -}
hasKeyAL :: Eq a => a -> [(a, b)] -> Bool
hasKeyAL key list =
    elem key (keysAL list)

{- | Flips an association list.  Converts (key1, val), (key2, val) pairs
to (val, [key1, key2]). -}
flipAL :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])]
flipAL oldl =
    let worker :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])] -> [(val, [key])]
        worker [] accum = accum
        worker ((k, v):xs) accum =
            case lookup v accum of
                                Nothing -> worker xs ((v, [k]) : accum)
                                Just y -> worker xs (addToAL accum v (k:y))
        in
        worker oldl []

{- | Converts an association list to a string.  The string will have
one pair per line, with the key and value both represented as a Haskell string.

This function is designed to work with [(String, String)] association lists,
but may work with other types as well. -}

strFromAL :: (Show a, Show b) => [(a, b)] -> String
strFromAL inp =
    let worker (key, val) = show key ++ "," ++ show val
        in unlines . map worker $ inp

{- | The inverse of 'strFromAL', this function reads a string and outputs the
appropriate association list.

Like 'strFromAL', this is designed to work with [(String, String)] association
lists but may also work with other objects with simple representations.
-}
strToAL :: (Read a, Read b) => String -> [(a, b)]
strToAL inp =
    let worker line =
            case reads line of
               [(key, remainder)] -> case remainder of
                     ',':valstr -> (key, read valstr)
                     _ -> error "Data.List.Utils.strToAL: Parse error on value"
               _ -> error "Data.List.Utils.strToAL: Parse error on key"
        in map worker (lines inp)


{- FIXME TODO: sub -}

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

{- | Returns the rightmost index of the given element in the
given list. -}
elemRIndex :: Eq a => a -> [a] -> Maybe Int
elemRIndex item l =
    case reverse $ elemIndices item l of
                                   [] -> Nothing
                                   (x:_) -> Just x
{- | Like elemRIndex, but returns -1 if there is nothing
found. -}
alwaysElemRIndex :: Eq a => a -> [a] -> Int
alwaysElemRIndex item list =
    case elemRIndex item list of
                              Nothing -> -1
                              Just x -> x

{- | Forces the evaluation of the entire list. -}
seqList :: [a] -> [a]
seqList [] = []
seqList list@(x:xs) = seq (seqList xs) list

--------------------------------------------------
-- Advanced Conversions
--------------------------------------------------

{- | The type used for functions for 'wholeMap'.  See 'wholeMap' for details.
-}
newtype WholeFunc a b = WholeFunc ([a] -> (WholeFunc a b, [a], [b]))

{- | This is an enhanced version of the concatMap or map functions in
Data.List.

Unlike those functions, this one:

 * Can consume a varying number of elements from the input list during
   each iteration

 * Can arbitrarily decide when to stop processing data

 * Can return a varying number of elements to insert into the output list

 * Can actually switch processing functions mid-stream

 * Is not even restricted to processing the input list intact

The function used by wholeMap, of type 'WholeFunc', is repeatedly called
with the input list.  The function returns three things: the function
to call for the next iteration (if any), what remains of the input list,
and the list of output elements generated during this iteration.  The return
value of 'wholeMap' is the concatenation of the output element lists from
all iterations.

Processing stops when the remaining input list is empty.  An example
of a 'WholeFunc' is 'fixedWidth'. -}
wholeMap :: WholeFunc a b -> [a] -> [b]
wholeMap _ [] = []              -- Empty input, empty output.
wholeMap (WholeFunc func) inplist =
    let (nextfunc, nextlist, output) = func inplist
        in
        output ++ wholeMap nextfunc nextlist

{- | A parser designed to process fixed-width input fields.  Use it with
'wholeMap'.

The Int list passed to this function is the list of the field widths desired
from the input.  The result is a list of those widths, if possible.  If any
of the input remains after processing this list, it is added on as the final
element in the result list.  If the input is less than the sum of the requested
widths, then the result list will be short the appropriate number of elements,
and its final element may be shorter than requested.

Examples:

>wholeMap (fixedWidth [1, 2, 3]) "1234567890"
> --> ["1","23","456","7890"]
>wholeMap (fixedWidth (repeat 2)) "123456789"
> --> ["12","34","56","78","9"]
>wholeMap (fixedWidth []) "123456789"
> --> ["123456789"]
>wholeMap (fixedWidth [5, 3, 6, 1]) "Hello, This is a test."
> --> ["Hello",", T","his is"," ","a test."]
-}
fixedWidth :: [Int] -> WholeFunc a [a]
fixedWidth len =
    WholeFunc (fixedWidthFunc len)
    where -- Empty input: Empty output, stop
          fixedWidthFunc _ [] = ((fixedWidth []), [], [])
          -- Empty length: Stop here.
          fixedWidthFunc [] x = ((fixedWidth []), [], [x])
          -- Stuff to process: Do it.
          fixedWidthFunc (len:lenxs) input =
              (fixedWidth lenxs, next, [this])
              where (this, next) = splitAt len input

{- | Helps you pick out fixed-width components from a list.

Example:

>conv :: String -> (String,String)
>conv = runState $
>        do f3 <- grab 3
>           n2 <- grab 2
>           return $ f3 ++ "," ++ n2
>
>main = print $ conv "TestIng"

Prints:

>("Tes,tI","ng")
-}

grab :: Int -> State [a] [a]
grab count =
    do g <- get
       (x, g') <- return $ splitAt count g
       put g'
       return x

{- | Similar to Data.List.elemIndex.  Instead of looking for one element in a
list, this function looks for the first occurance of a sublist in the list,
and returns the index of the first element of that occurance.  If there is no
such list, returns Nothing.

If the list to look for is the empty list, will return Just 0 regardless
of the content of the list to search.

Examples:

>subIndex "foo" "asdfoobar" -> Just 3
>subIndex "foo" [] -> Nothing
>subIndex "" [] -> Just 0
>subIndex "" "asdf" -> Just 0
>subIndex "test" "asdftestbartest" -> Just 4
>subIndex [(1::Int), 2] [0, 5, 3, 2, 1, 2, 4] -> Just 4
 -}
subIndex :: Eq a => [a] -> [a] -> Maybe Int
subIndex substr str = findIndex (isPrefixOf substr) (tails str)

{- | Given a list, returns a new list with all duplicate elements removed.
For example:

>uniq "Mississippi" -> "Misp"

You should not rely on this function necessarily preserving order, though
the current implementation happens to.

This function is not compatible with infinite lists.

This is presently an alias for Data.List.nub
 -}
uniq :: Eq a => [a] -> [a]
uniq = nub

----- same as
--uniq (x:xs) = x : [y | y <- uniq xs, y /= x]
