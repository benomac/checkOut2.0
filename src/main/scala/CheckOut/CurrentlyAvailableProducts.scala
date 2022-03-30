package CheckOut


import CheckOut.Parsers.{commandLineArgsParser, getListOfProducts}
import CheckOut.Store.{Discount, StockKeepingUnit}
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Parser._
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.Parser

import scala.annotation.tailrec

object CurrentlyAvailableProducts {
  val currentlyAvailableProducts: Map[String, StockKeepingUnit] = Map(
    "a" -> StockKeepingUnit("a", 0.5, Some(Discount(3, 1.3))),
    "b" -> StockKeepingUnit("b", 0.3, Some(Discount(2, 0.45))),
    "c" -> StockKeepingUnit("c", 0.2, None),
    "d" -> StockKeepingUnit("d", 0.15, None))

  def checkForExtraProducts(potentialArgs: Array[String]): Map[String, StockKeepingUnit] =
    commandLineArgsParser.parse(potentialArgs(0)) match {
      case Left(_) => currentlyAvailableProducts
      case _ =>
        val potentialProducts = getListOfProducts(potentialArgs)
        createNewSKUMapIfThereAreNewSKUsAvailable(potentialProducts)
    }

  @tailrec
  def createNewSKUMapIfThereAreNewSKUsAvailable(inputs: List[StockKeepingUnit], acc: Map[String, StockKeepingUnit] = currentlyAvailableProducts): Map[String, StockKeepingUnit] = {
    inputs match {
        case ::(head, next) =>
          head match {
            case StockKeepingUnit(_, _, _) => createNewSKUMapIfThereAreNewSKUsAvailable(next, acc + (head.name -> head))
            case _ => createNewSKUMapIfThereAreNewSKUsAvailable(next, acc)
          }
        case Nil =>
          acc ++ currentlyAvailableProducts
      }
  }

}


