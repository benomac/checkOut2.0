package CheckOut

import CheckOut.Main.{args, splitArgs}
import CheckOut.Store.{Discount, StockKeepingUnit}
import Parsers.skuItemParser
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Numbers.bigInt
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser

object Parsers {
  private val multiDigit = digit.rep.string.map(_.toInt)
  private val none = ignoreCase("none")

  def splitCommandLineArgs(args: Array[String]): List[String] = args(0).split(",").toList

  def getListOfProducts(list: List[String]): List[StockKeepingUnit] = {
    list.map(item => skuItemParser.parse(item) match {
      case Left(v) =>
        println(s"Incorrect format for $item")
        Left(v)
      case Right(v) => Right(v._2)
    })
      .filter(_.isRight).collect {
      case Right(v) => v
    }
  }

  def skuItemParser: Parser[StockKeepingUnit] =
    ((alpha <* sp)
      ~ ((multiDigit <* char('.')) ~ bigInt)
      ~ sp
      ~ ((digit <* sp) ~ ((multiDigit <* char('.')) ~ multiDigit) | none))
      .string
      .map(_.split(" ").toList match {
        case List(name, price, specialPrice, amountForSpecialPrice) =>
          StockKeepingUnit(
            name = name,
            unitPrice = BigDecimal.valueOf(price.toDouble),
            discount = Some(Discount(specialPrice.toInt, BigDecimal.valueOf(amountForSpecialPrice.toDouble))))
        case List(name, price, _) =>
          StockKeepingUnit(
            name = name,
            unitPrice = BigDecimal.valueOf(price.toDouble),
            discount = None)
        case _ => "error".asInstanceOf[StockKeepingUnit]
      }
      )


}
