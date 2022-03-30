package CheckOut

import CheckOut.Main.args
import CheckOut.Store.{Discount, StockKeepingUnit}
import Parsers.{mainParser, skuItemParser}
import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser, Parser0}
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser

object Parsers {
  private val multiDigit = digit.rep.string.map(_.toInt)
  private val none = ignoreCase("none")
  private val mainParser =
    ((alpha <* sp)
      ~ ((multiDigit <* char('.')) ~ multiDigit)
      ~ sp
      ~ ((digit <* sp) ~ ((multiDigit <* char('.')) ~ multiDigit) | none))

  def commandLineArgsParser: Parser0[Equals with Serializable] =
    (mainParser | mainParser <* char(',')) | mainParser.rep

  def skuItemParser: Parser[StockKeepingUnit] =
    mainParser
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

  def splitCommandLineArgs(args: Array[String]): List[String] =
    args(0).split(",").toList


  def getListOfProducts(products: Array[String]): List[StockKeepingUnit] = {
    if (products.length == 0) Nil
    else
      splitCommandLineArgs(products)
      .map(product => skuItemParser.parse(product) match {
        case Left(v) =>
          println(s"Incorrect format for product $product")
          Left(v)
        case Right(v) => Right(v._2)
      }
      )
      .filter(_.isRight).collect {
      case Right(v) => v
    }
  }

}