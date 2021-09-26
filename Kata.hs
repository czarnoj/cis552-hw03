{-
Return of the Data Munging Kata
===============================
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kata where

{-
This problem asks you to revisit the data munging Kata from the first homework
assignment, to see how type classes can help structure programs to separate
library routines from business logic.

In this problem, you can use any function in the Haskell
[Prelude](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html)
or the following libraries:
-}

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
{-
The code below also uses the following functions that we'll import explicitly.
-}

{-

We'll also use the following library for `NonEmpty` lists
-}

import Data.List.NonEmpty (NonEmpty (..)) -- Lists that are NonEmpty, see below
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import System.IO
import Test.HUnit (Test (TestList), (~?=))
import Text.Read (readMaybe)

{-
Prelude: Working with `NonEmpty` lists
--------------------------------------

This time around, you need to be more careful about the use of functions that
could trigger a runtime error. For example, calling the `maximum` function with
an empty list triggers a runtime error.

      *Kata> maximum []
      maximum []
      *** Exception: Prelude.maximum: empty list

Furthermore, this kind of exeception is difficult to recover from in Haskell!
Because it can be difficult to know for certain that a list is guaranteed to not
be null, a good approach is to get the type system to help out.

The Haskell base library includes the [following
   module](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List-NonEmpty.html)
that defines the parameterized type `NonEmpty a` to represent non-empty
lists. This type is represented using standard lists and supports many of
the usual operations that lists do.

For example, we can construct a nonempty list out of a regular list and a
distinguished first element. For example,

        *Kata> let x = 'a' :| ['b', 'c', 'd']
        let x = 'a' :| ['b', 'c', 'd']
        *Kata> :type x
        :type x
        x :: NonEmpty Char

Because we know that there is a first element, the `head` operation
is guaranteed to succeed. This function is defined in the `Data.List.NonEmpty`
module, which we have imported using the shorter qualifier `NE`.

        *Kata> NE.head x
        NE.head x
        'a'

Furthermore, many of the usual list functions, such as `maximum`
and `length` are overloaded, so they are available for both regular and
`NonEmpty` lists.

        *Kata> length x
        length x
        4
        *Kata> maximum x
        maximum x
        'd'

Kata revisited
--------------

The goal of this problem is to develop a *library* for working with ad hoc
data files like we saw in the first homework assignment.

This library should support good design: we want to cleanly separate the
job of parsing the data file from any sort of processing that we want to
do with that data. Furthermore, because we are creating a reusable library,
we'll work with a simplified form of a standard data format---data files
that separate values by commas.
(The design of the library is *loosely* inspired by
[Cassava](https://hackage.haskell.org/package/cassava), a Haskell library for
working with CSV data.)

A key idea of this library is that it should support working with a
*data model* that represents only the data from the file that is relevant to
our application.

For example, for the weather data, we might want to define a type that
includes only the information from the file that we care about: the day
number and the minimum and maximum temperatures.
-}

-- | Weather model
data Weather = Weather
  { day :: Day,
    maxTemp :: Int,
    minTemp :: Int
  }
  deriving (Eq, Show)

{-
Furthermore, we will use a special purpose type to represent the day
of the month. (A newtype is a restricted form of datatype that allows
only one variant and one field.)
-}

-- | Type for Day of the month information: an Int in the range 1-31
newtype Day = Day Int deriving (Eq, Show)

{-
Using the library
-----------------

The main operation of this library is the function `parse`, defined below, of type

      parse :: ParseRecord a => String -> Maybe (NonEmpty a)

This function parses the input file into a non-empty list data records. In the case of
weather files, these records will be of type `Weather`, representing the weather data
in each row of the input. The parse function needs extra information to figure out how
to interpret the input string. The first line of input should be a *header row* that
specifies the columns of the file, separated by commas. Then each row
of the file can be split up into columns and parsed into the data model, using the
overloaded `parseRecord` function for that type.

For example, for the `Weather` type, the definition looks like this:
-}

-- | Parse weather information from rows of tabulated strings
instance ParseRecord Weather where
  parseRecord :: [(String, String)] -> Maybe Weather
  parseRecord row = Just Weather <**> row !? "DY" <**> row !? "MAX" <**> row !? "MIN"

{-
Above, the `parseRecord` function expects a data row that has been converted
into a simple dictionary of type `[(String,String)]`. The first component
of each pair in the list is the name of the column (from the header row) and the second
component should be a string with the value in that row. For example
the data from the first line of `jul21.csv` would look like:

      [("DY"," 1"),("MAX"," 86"),("MIN"," 70"), ...]

To take this input and create our data model concisely, we use two infix
operators. The first `(!?)` is just a wrapper for the library function `lookup`
from `Data.List`. The second `(<**>)` is a function that we'll
define below in our library.
-}

(!?) :: [(String, String)] -> String -> Maybe String
row !? name = List.lookup name row

{-
The `(<**>)` function has to know how to convert each data string into the appropriate
type for the data model. For this, it relies on another overloaded function, `parseField`.
(See below for the declaration of the `ParseField` class.)

For example, to parse a `Day` field, we create the following instance:
-}

-- | Parse day information, returns Nothing when the int is out of range
instance ParseField Day where
  parseField :: String -> Maybe Day
  parseField str = case readMaybe str of
    Nothing -> Nothing
    Just x -> if x < 0 || x > 31 then Nothing else Just (Day x)

{-
This function converts a given string (such as " 1") into a `Day` while validating that it
is in range.  This parsing works in two steps: first we use the (overloaded)
`readMaybe` function from  the `Text.Read` library to ensure that the String represents
an `Int` value. Then we validate the range of the `Int` before constructing a `Day`.

The library contains instances of `ParseField` for built-in types, like `Int` (see below).
So this is all that we need to parse weather files.
-}

parseWeather :: String -> Maybe (NonEmpty Weather)
parseWeather = parse

{-
Business logic
--------------

Once we have defined our data model, the main program is short and sweet. For example,
the program below computes the day of the month with the minimum temperature difference.
-}

-- | Compare two weather records based on the difference between their
-- minimum and maximum temperatures
compareTempDiff :: Weather -> Weather -> Ordering
compareTempDiff w1 w2 = compare (diff w1) (diff w2)
  where
    diff w = maxTemp w - minTemp w

-- | A program that computes information about the weather
weatherProgram :: IO ()
weatherProgram = do
  chars <- readFile "jul20.csv"
  case parse chars of
    Nothing ->
      putStrLn "Cannot parse weather file."
    Just ws -> do
      let ans = day (Foldable.minimumBy compareTempDiff ws)
      putStrLn $ "The day with minimum temperature change was " ++ show ans

{-
The punchline of this problem is that all we need to do to calculate the
minimum temperature spread for weather files is the code to this point.
Everything after this line is part of the implementation of a reusable library.

The Library
-----------

Now, we develop our general purpose library for processing data files.  Your
job for this section is to get `weatherProgram` above to work.

Unlike the first assignment, this library will assume that the data is
stored in a CSV file.  Notice that in both [`jul21.csv`](jul21.csv) and
[`soccer20.csv`](soccer20.csv) the first line of the file is a
*header row*.

Your job is to implement the following two functions that divide up the header
row by commas and divide up each data row, matching it up with the headers.
If there are not enough header or data values, `splitRow` should produce
as much output as it can.

-}

-- >>>  splitHeader "A,B,C"
-- ["A","B","C"]
splitHeader :: String -> [String]
splitHeader = undefined

-- >>> splitRow ["A","B","C"] "1,2,3"
-- [("A","1"),("B","2"),("C","3")]
-- >>> splitRow ["A", "B"] "1,2,3"
-- [("A","1"),("B","2")]
-- >>> splitRow ["A","B","C"] "1,2"
-- [("A","1"),("B","2")]
splitRow :: [String] -> String -> [(String, String)]
splitRow header row = undefined

{-
Note that a real library for CSV parsing would ignore `,`s that appear
inside quotes and treat them as part of the data (or header name). You do not
need to do so for this exercise.

With these these functions, we can now define a general purpose parser for
data files. This parser can produce any sort of result data, as long as there
is an instance of the `ParseRecord` class (such as the one for `Weather`
records above).
-}

class ParseRecord a where
  -- Convert a list of string elements into row
  parseRecord :: [(String, String)] -> Maybe a

-- | Parse a data file into a list of data rows
-- First line of the file must be a header line
-- any rows in the data file that are unparseable are ignored
parse :: ParseRecord a => String -> Maybe (NonEmpty a)
parse str = NE.nonEmpty $ Maybe.mapMaybe parseRecord (tabulate str)
  where
    tabulate :: String -> [[(String, String)]]
    tabulate s = case lines s of
      [] -> []
      hd : rows -> map (splitRow header) rows
        where
          header = splitHeader hd

{-
There is nothing more for you to do for this part, other than make sure that you
understand how `parse` works.

Field parsing
--------------

The library also includes an auxiliary class that we can use to
tell the library how to parse each field of a record (such as `Weather`).
-}

class ParseField a where
  parseField :: String -> Maybe a

{-
For example, many of the fields of the `Weather` record are `Int`s.  Therefore,
we need to define an instance of this class for the `Int` type.
-}

instance ParseField Int where
  parseField = readMaybe

{-
In fact, many of Haskell's types can be parsed using Haskell's `readMaybe`
library function, which also ignores any spaces before or after the value.
-}

instance ParseField Bool where
  parseField = readMaybe

instance ParseField Double where
  parseField = readMaybe

{-
We also can add an instance for `String`s that doesn't do any parsing at
all --- just passes the value through.
-}

instance ParseField String where
  parseField = Just

{-
Field chaining
--------------

The implementation of `ParseRecord` uses `ParseField` implicitly through the
use of the binary operator `<**>`. This operator, defined below, combines
together the various fields in the row. It uses `ParseField` to allow
the type of each field in the structure to determine how it should be
parsed as a Haskell value.

For example, suppose we have this example data model
-}

data Example = Example Int Day deriving (Show)

{-
Then we can build/validate input field by field, returning
`Nothing` if any of them fail to parse.
-}

-- >>> Just Example <**> Just "12 " <**> Just " 12"
-- Just (Example 12 (Day 12))
-- >>> Just Example <**> Just "47 " <**> Just " 47"
-- Nothing

{-
Your job is to fix the implementation of this operation, using
`parseField`.  The `infixl` tells the Haskell parser what precedence
to use for this infix operator.
-}

infixl 4 <**>

(<**>) :: ParseField a => Maybe (a -> b) -> Maybe String -> Maybe b
_ <**> _ = Nothing

{-
At this point you should be able to run the `weatherProgram` above. Make
sure that it works correctly before continuing.

Parsing different types of fields
---------------------------------

The `Weather` data constructor only takes `Day` and `Int` arguments, but
`parseRecord` is more general than that. All we need is an instance of
`ParseField` to parse other types of data.

For example, `String` fields need no conversion, so they can be returned
immediately.

A more full featured library would include additional instances for the basic
Haskell types.
-}

-----------------------------------------------------------------
-- Weather events

{-
However, let's do more domain-specific parsing.

For example, the weather file includes a column (marked `WX`) for weather events. We
can represent these events with a datatype. If multiple events happen on the same day,
there will be multiple entries in the column.
-}

data Event
  = Fog
  | IntenseFog
  | Thunder
  | Ice
  | Hail
  | FreezingRain
  | Duststorm
  | Smoke
  | BlowingSnow
  | Tornado
  deriving (Eq, Ord, Enum, Show)

{-
Your next job is to parse single events according to the legend shown in the
data file. More specifically, your implementation should
look at a single character ('1'-'9' or 'X') and produce the appropriate
event above.
-}

parseEventChar :: Char -> Maybe Event
parseEventChar _ = Nothing

testParseEventChar :: Test
testParseEventChar =
  TestList
    [ parseEventChar '1' ~?= Just Fog,
      parseEventChar 'X' ~?= Just Tornado,
      parseEventChar 'Y' ~?= (Nothing :: Maybe Event)
    ]

{-
We can use this parser for events to parse the sequence of events in the WX
 column. We'll start by defining a new type for a list of events.
-}

newtype Events = Events {getEvents :: [Event]} deriving (Eq, Show)

{-
This newtype allows us to define a special purpose parser that parses each
character individually and ignores any invalid events.
-}

-- >>> parseField "12 " :: Maybe Events
-- Just (Events {getEvents = [Fog,IntenseFog]})
-- >>> parseField "1D3 " :: Maybe Events
-- Just (Events {getEvents = [Fog,Thunder]})

instance ParseField Events where
  parseField = undefined

{-
Above, you might find functions in the `Data.Maybe` library useful.

Working with the Library
------------------------

In the weather data file, the column marked `DEP` indicates the difference
between the day's average temperature and the usual average temperature for
that day.  Modify the types above (or define new ones!) and fill in the
definitions below so that we can also calculate the day where the temperature
is the most unseasonable... i.e. the day with the greatest departure from
normal.
-}

-- | return the day of the month with the largest departure from normal temperature
mostUnseasonable :: NonEmpty Weather -> Day
mostUnseasonable = undefined

{-
Next, write a function that returns how many days of the month had
some sort of precipitation. The column marked `WTR` contains this information,
when that column has a `T`, that indicates a trace amount, which should be
included in the result.
-}

-- | return how many days had some sort of precipitation
numPrecip :: NonEmpty Weather -> Int
numPrecip = undefined

{-
Finally, write a function that returns the list of all foggy days. This
function should count all days that include `Fog` or `IntenseFog` as a
weather event.
-}

-- | return a list of all dates with fog events
foggyDays :: NonEmpty Weather -> [Day]
foggyDays = undefined

{-
Make sure that you write some tests for these functions! This time we've given you
two weather data files to play with: jul20.csv and jul21.csv. Make sure
that your implementation works for both of them.

-----------------------------------------------------------------

Premier League data
--------------------

Finally, to make sure that you understand how this library works, use it to
process Premier League tables, calculating the place `#` of the team with the smallest
absolute difference between the number of wins `W` and number of losses `L`. If there
are two teams with the same difference, the program should return the first one.
-}

soccer :: String -> Maybe Int
soccer = undefined

{-
>
>
>
-}

soccerProgram :: IO ()
soccerProgram = do
  chars <- readFile "soccer20.csv"
  case soccer chars of
    Just answer ->
      putStrLn $ "The team with the smallest difference placed " ++ show answer ++ "."
    Nothing ->
      putStrLn "Couldn't read input file"
