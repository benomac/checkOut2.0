package CheckOut


import CheckOut.Store.{Discount, StockKeepingUnit}

import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.Parser._
import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

import scala.annotation.tailrec

object CurrentlyAvailableProducts {
  val currentlyAvailableProducts: Map[String, StockKeepingUnit] = Map(
    "a" -> StockKeepingUnit("a", 0.5, Some(Discount(3, 1.3))),
    "b" -> StockKeepingUnit("b", 0.3, Some(Discount(2, 0.45))),
    "c" -> StockKeepingUnit("c", 0.2, None),
    "d" -> StockKeepingUnit("d", 0.15, None))

  @tailrec
  def createNewSKUMapIfThereAreNewSKUsAvailable(inputs: List[StockKeepingUnit], acc: Map[String, StockKeepingUnit] = Map.empty): Map[String, StockKeepingUnit] = {
    inputs match {
      case ::(head, next) =>
        if (head.isInstanceOf[StockKeepingUnit])
          createNewSKUMapIfThereAreNewSKUsAvailable(next, acc + (head.name -> head))
        else {
          println(s"Error $head invalid product")
          createNewSKUMapIfThereAreNewSKUsAvailable(next, acc)
        }
      case Nil => acc ++ currentlyAvailableProducts
    }
  }

}


