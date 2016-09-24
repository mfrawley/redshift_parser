import SQLParser
import Text.ParserCombinators.Parsec (parseTest)

testParsingSQLName =
  parseTest sqlName "bad_test"

testDefaultsParser = do
  parseTest defaultsParser "not null"
  parseTest defaultsParser "default null"

testColWithNoSize = do
  parseTest colWithNoSize " , activity_id     int           not null \n"

testColWithSize = do
  parseTest colWithSize "   activity_name   varchar(255)  not null\n"

main :: IO ()
main = do
  testParsingSQLName
  testDefaultsParser
  testColWithNoSize
  testColWithSize
