
import Criterion.Main
import Text.Parsec

import Data.Dates.Formats

formatStr1 = "YYYY/MM/DD"
formatStr2 = "YY.MM.DD[, HH:mm:SS]"

main = do
  format1 <- case parseFormat formatStr1 of
              Left err -> fail (show err)
              Right f -> return f
  format2 <- case parseFormat formatStr2 of
              Left err -> fail (show err)
              Right f -> return f
  let parser1 = formatParser format1
  let parser2 = formatParser format2
      parse1 str = case runParser parser1 () str str of
                    Left err -> error (show err)
                    Right x -> x
      parse2 str = case runParser parser2 () str str of
                    Left err -> error (show err)
                    Right x -> x
  defaultMain [
      bench formatStr1 $ whnf parse1 "2012/09/12",
      bench formatStr2 $ whnf parse2 "13.05.01, 11:47:15",
      bench formatStr2 $ whnf parse2 "13.05.01",
      bench formatStr2 $ whnf parse2 "13.05.01,    12:15:00"
    ]
