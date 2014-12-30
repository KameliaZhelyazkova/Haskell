import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char
import Data.List

-- Type synonyms - these are only used for better readablity of our code, they mostly have no real function
-- Example. We can say that the type Name is a synonym of a String
type Name = String

-- So the following
sayName:: Name -> String
sayName name = name

-- Is the same as
sayName1:: String -> String
sayName1 name = name


-- algebriac data types are used to describe a spesific set of values. For example we can describe gender

data Gender = Male | Female | Unknown

sayGender:: Gender -> String
sayGender Male = "I'm a boy"
sayGender Female = "I'm a girl"
sayGender Unknown = "?"

-- try doing the same but for all the seasons of the year


-- Also algebraic type value can have type paramethers (more values) in them. Lets look at an example

-- Multiply all the components of a construct
data Construct = Point Int Int | Vector Int Int Int

doStuffWithConstruct:: Construct -> Int
doStuffWithConstruct (Point x y) = x * y
doStuffWithConstruct (Vector a b c) = a * b * c

-- So here we can store Ints in the Point value or the Vector value (they are both from type Construct)
-- TODO: see infix operators