import Lib
import Types
import SQLParser
import Text.ParserCombinators.Parsec (parseTest, parse)
import Test.HUnit (Test(..), assertEqual, assertFailure, runTestTT)
import Data.Either.Utils (forceEither)
import Data.Maybe

testParsingSQLName = TestCase $ assertEqual
    "should parse sql identifier"
    "bad_test"
    (forceEither $ parse sqlName "" "bad_test")

testDefaultsParserNotNull = TestCase $ assertEqual
  "should parse a not null defaults string"
  "not null"
  (forceEither $ parse defaultsParser "" "not null")

testDefaultsParserDefaultNull = TestCase $ assertEqual
  "should parse a default null defaults string"
  "default null"
  (forceEither $ parse defaultsParser "" "default null")

testColWithNoSize = TestCase $ assertEqual
  "should a line defining a column with no size defined"
  (ColumnDefinition {
      colName = "activity_id"
      , colType = "int"
      , colDataLen = Nothing
      , colDefaults = "not null"
      })
  (forceEither $ parse colWithSize  "" " , activity_id     int           not null \n")

testVarcharColWithSize = TestCase $ assertEqual
  "should a line defining a column with no size defined"
  (ColumnDefinition {
      colName = "activity_name"
      , colType = "varchar"
      , colDataLen = Just ColumnLength {colLen = 255, colPrecision = Nothing}
      , colDefaults = "not null"
      })
  (forceEither $ parse colWithSize  "" "  activity_name   varchar(255)  not null\n")

testPrimaryKeyLine = TestCase $ assertEqual
  "should parse a line defining a primary key field"
  "activity_id"
  (forceEither $ parse primaryKeyLineParser "" ", primary key(activity_id)\n")

testDistStyle = TestCase $ assertEqual
  "should parse a line defining a distribution style for the table"
  "key"
  (forceEither $ parse distStyleParser "" "diststyle key")

testSortKey = TestCase $ assertEqual
  "should parse the sortkey list"
  ["website_id", "queued_at"]
  (forceEither $ parse sortKeyParser "" "sortkey(website_id, queued_at)")

testIntPairInParens = TestCase $ assertEqual
  "should parse a pair of ints in parentheses"
  ColumnLength { colLen = 12, colPrecision = Just 2}
  (forceEither $ parse intPairInParens "" "(12,2)")

testCreateWithNoExistentialCheck = TestCase $ assertEqual
  "should parse a pair of ints in parentheses"
  ("mapping", "activity")
  (forceEither $ parse createStm "" "create table mapping.activity(")

tests = TestList [
           testParsingSQLName
          , testDefaultsParserNotNull
          , testDefaultsParserDefaultNull
          , testColWithNoSize
          , testVarcharColWithSize
          , testIntPairInParens
          , testPrimaryKeyLine
          , testDistStyle
          , testSortKey
          , testCreateWithNoExistentialCheck

        ]

main = do
  runTestTT tests
