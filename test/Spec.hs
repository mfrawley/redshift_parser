import SQLParser
import Text.ParserCombinators.Parsec (parseTest, parse)
import Test.HUnit (Test(..), assertEqual, assertFailure, runTestTT)

-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

testParsingSQLName = TestCase $ assertEqual
    "should parse sql identifier"
    "bad_test"
    (do
      case parse sqlName "" "bad_test" of
        Left e -> ""
        Right s -> s
      )

-- testDefaultsParser = do
--   parseTest defaultsParser "not null"
--   parseTest defaultsParser "default null"
--
-- testColWithNoSize = do
--   parseTest colWithSize " , activity_id     int           not null \n"
--
-- testColWithSize = do
--   parseTest colWithSize "   activity_name   varchar(255)  not null\n"
--
-- testPrimaryKeyLine = do
--   parseTest primaryKeyLineParser ", primary key(activity_id)\n"
--
-- distStyleTest = do
--   parseTest distStyleParser "diststyle key"

tests = TestList [TestLabel "testParsingSQLName" testParsingSQLName]

main = do
  runTestTT tests
  -- testParsingSQLName
  -- testDefaultsParser
  -- testColWithNoSize
  -- testColWithSize
  -- testPrimaryKeyLine
