import CheckOut.Main.{args, splitArgs}

import CheckOut.Store.{Discount, StockKeepingUnit}

import cats.parse.Numbers._
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser

object Parsers {
  private val none = ignoreCase("none")

  def skuItemParser: Parser[StockKeepingUnit] =
    ((alpha <* sp)
      ~ ((bigInt <* char('.')) ~ bigInt)
      ~ sp
      ~ ((digit <* sp) ~ ((bigInt <* char('.')) ~ bigInt) | none))
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
      }
      )

  def splitCommandLineArgs(args: Array[String]): List[String] = args(0).split(",").toList

  def getListOfProducts(list: List[String]): List[StockKeepingUnit] = { //how to get the values out??
    list.map(item => skuItemParser.parse(item) match {
      case Left(v) => {
        println(s"${list.head}")
        Left(v)
      }
      case Right(v) => Right(v._2)
    }
    ).filter(_.isRight).collect {
      case Right(v) => v
    }
  }
}
import Parsers._

skuItemParser.parse("e 0.40 3 1.00,f 00.4 none,fail")
val split = splitCommandLineArgs(Array("e 0.40 3 1.00,f 00.4 none,fail"))
//
val foo: Seq[StockKeepingUnit] = getListOfProducts(split)
