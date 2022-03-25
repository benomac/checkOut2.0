package CheckOut

import CheckOut.Main.{args, splitArgs}
import CheckOut.Store.{Discount, StockKeepingUnit}
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser

object Parsers {
  val skuItemParser: Parser[StockKeepingUnit] = {
    val multiDigitStop = digit.rep.string.map(_.toInt) <* char('.')
    val doubleDigit = digit ~ digit
    val none = ignoreCase("none")
    (
      (alpha <* sp)
        ~ (multiDigitStop ~ doubleDigit)
        ~ sp
        ~ ((digit <* sp) ~ (multiDigitStop ~ doubleDigit) | none)
      )
      .string.map(_.split(" ").toList match {
      case List(name, price, specialPrice, amountForSpecialPrice) =>
        (name, price, specialPrice, amountForSpecialPrice)
        StockKeepingUnit(
          name,
          BigDecimal.valueOf(price.toDouble),
          Some(Discount(specialPrice.toInt, BigDecimal.valueOf(amountForSpecialPrice.toDouble))))
      case List(name, price, none) =>
        (name, price, none.toLowerCase)
        StockKeepingUnit(
          name,
          BigDecimal.valueOf(price.toDouble),
          None)
    }
    )
  }

  def splitCommandLineArgs(args: Array[String]): List[String] = args(0).split(",").toList

  def getListOfProducts(list: List[String]) = {
      list.map(x => skuItemParser.parse(x) match {
        case Left(v) => v
        case Right(v) => v._2
      }
    )
  }

}
