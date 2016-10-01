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
      , colDataLen = Just "255"
      , colDefaults = "not null"
      })
  (forceEither $ parse colWithSize  "" "   activity_name   varchar(255)  not null\n")

testPrimaryKeyLine = TestCase $ assertEqual
  "should parse a line defining a primary key field"
  "activity_id"
  (forceEither $ parse primaryKeyLineParser "" ", primary key(activity_id)\n")

testDistStyle = TestCase $ assertEqual
  "should parse a line defining a distribution style for the table"
  "key"
  (forceEither $ parse distStyleParser "" "diststyle key")

tests = TestList [
           TestLabel "testParsingSQLName" testParsingSQLName
          , TestLabel "testDefaultsParserNotNull" testDefaultsParserNotNull
          , TestLabel "testDefaultsParserDefaultNull" testDefaultsParserDefaultNull
          , TestLabel "testColWithNoSize" testColWithNoSize
          , TestLabel "testVarcharColWithSize" testVarcharColWithSize
          , TestLabel "testPrimaryKeyLine" testPrimaryKeyLine
          , TestLabel "testDistStyle" testDistStyle
        ]

main = do
  runTestTT tests
