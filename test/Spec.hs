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
  "should parse a column with no size defined"
  (ColumnDefinition {
      colName = "activity_id"
      , colType = "int"
      , colDataLen = Nothing
      , colDefaults = "not null"
      , colEncoding = Nothing

      })
  (forceEither $ parse colWithSize  "" " , activity_id     int           not null \n")

testVarcharColWithSize = TestCase $ assertEqual
  "should parse a column with no size defined"
  (ColumnDefinition {
      colName = "activity_name"
      , colType = "varchar"
      , colDataLen = Just ColumnLength {colLen = 255, colPrecision = Nothing}
      , colDefaults = "not null"
      , colEncoding = Nothing
      })
  (forceEither $ parse colWithSize  "" "  activity_name   varchar(255)  not null\n")


testVarcharColWithMaxSize = TestCase $ assertEqual
  "should parse a varchar column with max size defined"
  (ColumnDefinition {
      colName = "activity_name"
      , colType = "varchar"
      , colDataLen = Just ColumnLength {colLen = 65535, colPrecision = Nothing}
      , colDefaults = "not null"
      , colEncoding = Nothing
      })
  (forceEither $ parse colWithSize  "" "  activity_name   varchar(max)  not null\n")

testDecimalCol = TestCase $ assertEqual
  "should parse a column with type decimal"
  (ColumnDefinition {
      colName = "sum_click_costs"
      , colType = "decimal"
      , colDataLen = Just ColumnLength {colLen = 12, colPrecision = Just 2}
      , colDefaults = "not null"
      , colEncoding = Nothing
      })
  (forceEither $ parse colWithSize  "" ", sum_click_costs decimal(12, 2)      not null")


testBooleanCol = TestCase $ assertEqual
  "should parse a column with type boolean"
  (ColumnDefinition {
      colName = "is_notification_newsletter_active"
      , colType = "boolean"
      , colDataLen = Nothing
      , colDefaults = "not null"
      , colEncoding = Nothing
      })
  (forceEither $ parse colWithSize  "" ", is_notification_newsletter_active boolean not null")

testEncodingCol = TestCase $ assertEqual
  "should parse a column with an explicit encoding"
  (ColumnDefinition {
      colName = "id"
      , colType = "bigint"
      , colDataLen = Nothing
      , colDefaults = "not null"
      , colEncoding = Just "delta"
      })
  (forceEither $ parse colWithSize  "" "id                      bigint    not null     encode delta")

testPrimaryKeyLine = TestCase $ assertEqual
  "should parse a line defining a primary key field"
  "activity_id"
  (forceEither $ parse primaryKeyLineParser "" ", primary key(activity_id)\n")

testDistStyle = TestCase $ assertEqual
  "should parse a line defining a distribution style for the table"
  "key"
  (forceEither $ parse distStyleParser "" "diststyle key")

testInvalidDistStyle = TestCase $ assertEqual
  "should NOT parse a line defining an invalid distribution style"
  "(line 1, column 11):\nunexpected \"f\"\nexpecting space, \"all\", \"even\" or \"key\""
  (case (parse distStyleParser "" "diststyle foo") of
    Left e -> show e
    Right p -> "")

testSortKey = TestCase $ assertEqual
  "should parse the sortkey list"
  ["website_id", "queued_at"]
  (forceEither $ parse sortKeyParser "" "sortkey(website_id, queued_at)")

testIntPairInParens = TestCase $ assertEqual
  "should parse a pair of ints in parentheses"
  ColumnLength { colLen = 12, colPrecision = Just 2}
  (forceEither $ parse intPairInParens "" "(12,2)")

testCreateTable = TestCase $ assertEqual
  "should parse a create statement"
  ("logs", "freetrial")
  (forceEither $ parse createStm "" "create table logs.freetrial (")

testCreateTableWithExistentialCheck = TestCase $ assertEqual
  "should parse a create statement with existential check"
  ("logs", "freetrial")
  (forceEither $ parse createStm "" "create table if not exists logs.freetrial (")

testDropTable = TestCase $ assertEqual
  "should parse a simple drop table statement"
  "freetrial"
  (forceEither $ parse dropTableQuery "" "drop table logs.freetrial;")

testDropTableWithExistentialCheck = TestCase $ assertEqual
  "should parse a drop table with an 'if exists' statement"
  "freetrial"
  (forceEither $ parse dropTableQuery "" "drop table if exists logs.freetrial;")

testComment = TestCase $ assertEqual
  "should parse a comment"
  ()
  (forceEither $ parse spacesOrComment "" " /** //ssdfdsf */")

testEncodeParser = TestCase $ assertEqual
  "should parse a col encoding"
  ("lzo")
  (forceEither $ parse encodeParser "" "encode lzo")

tests = TestList [
           testParsingSQLName
          , testDefaultsParserNotNull
          , testDefaultsParserDefaultNull
          , testColWithNoSize
          , testDecimalCol
          , testVarcharColWithSize
          , testVarcharColWithMaxSize
          , testBooleanCol
          , testIntPairInParens
          , testPrimaryKeyLine
          , testDistStyle
          , testInvalidDistStyle
          , testSortKey
          , testCreateTable
          , testCreateTableWithExistentialCheck
          , testDropTable
          , testDropTableWithExistentialCheck
          , testComment
          , testEncodeParser
          , testEncodingCol
        ]

main = do
  runTestTT tests
