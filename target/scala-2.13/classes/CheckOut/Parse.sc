import CheckOut.CurrentlyAvailableProducts.currentlyAvailableProducts
import CheckOut.Parsers.multiDigit
import CheckOut.Store.{Discount, StockKeepingUnit}
import cats.data.NonEmptyList
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{char, ignoreCase}
import cats.parse.Rfc5234.{alpha, digit, sp}

import scala.annotation.tailrec

object Parsers {
  private val multiDigit = digit.rep.string.map(_.toInt)
  private val none = ignoreCase("none")

  def skuItemParser: Parser[StockKeepingUnit] =
    ((alpha <* sp)
      ~ ((multiDigit <* char('.')) ~ multiDigit)
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

  def splitCommandLineArgs(args: Array[String]): List[String] = args(0).split(",").toList

  def getListOfProducts(products: Array[String]): List[StockKeepingUnit] = {
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

  def commandLineArgsParser: Parser0[NonEmptyList[String]] =
    ((alpha <* sp)
      ~ ((multiDigit <* char('.')) ~ multiDigit)
      ~ sp
      ~ ((digit <* sp) ~ ((multiDigit <* char('.')) ~ multiDigit) | none)).string.rep

}

@tailrec
def createNewSKUMapIfThereAreNewSKUsAvailable(inputs: List[StockKeepingUnit], acc: Map[String, StockKeepingUnit] = Map.empty): Map[String, StockKeepingUnit] = {
  if (inputs.isEmpty) {
    println("this" + inputs)
    currentlyAvailableProducts
  }
  else
    inputs match {
      case ::(head, next) =>
        if (head.isInstanceOf[StockKeepingUnit]) {
          createNewSKUMapIfThereAreNewSKUsAvailable(next, acc + (head.name -> head))
        } else {
          createNewSKUMapIfThereAreNewSKUsAvailable(next, acc)
        }
      case Nil => {
        println("here" + acc ++ currentlyAvailableProducts)
        acc ++ currentlyAvailableProducts
      }
    }

}


Parsers.commandLineArgsParser.parse("jn")


