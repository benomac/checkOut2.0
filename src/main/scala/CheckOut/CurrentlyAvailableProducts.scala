package CheckOut


import CheckOut.Store.{Discount, StockKeepingUnit}

import scala.annotation.tailrec

object CurrentlyAvailableProducts {
  val currentlyAvailableProducts: Map[String, StockKeepingUnit] = Map(
    "a" -> StockKeepingUnit("a", 0.5, Some(Discount(3, 1.3))),
    "b" -> StockKeepingUnit("b", 0.3, Some(Discount(2, 0.45))),
    "c" -> StockKeepingUnit("c", 0.2, None),
    "d" -> StockKeepingUnit("d", 0.15, None))

  def createNewSKUMapIfThereAreNewSKUsAvailable(inputs: String): Map[String, StockKeepingUnit] = {
    val newItemsList: List[List[String]] =
      inputs
        .toLowerCase
        .split(",")
        .toList
        .map(newItem => List(newItem))
        .map(newItemList => newItemList
          .head.split(" ")
          .toList)
    val formatOk = checkFormat(newItemsList)
    if (formatOk.flatten.flatten.isEmpty)
      currentlyAvailableProducts
    else currentlyAvailableProducts ++ createSkuFromInputs(formatOk)
  }

  def incorrectFormat(head: List[String], next: List[List[String]], acc: List[List[String]]): List[List[String]] = {
    println(s"$head's format is incorrect'")
    checkFormat(next, acc)
  }


  @tailrec
  def checkFormat(list: List[List[String]], acc: List[List[String]] = Nil): List[List[String]] =
    list match {
      case ::(head, next) =>
        head match {
          case a :: b :: c :: Nil =>
            if (!b.charAt(0).isDigit || b.toInt == 0 || c.toInt == 0) {
              incorrectFormat(head, next, acc)
            }
            else if (a.getClass.toString != "class java.lang.String" ||
              BigDecimal.valueOf(b.toDouble).getClass.toString != "class scala.math.BigDecimal" ||
              c.toLowerCase != "none") {
              incorrectFormat(head, next, acc)
            }
            else checkFormat(next, head +: acc)
          case a :: b :: c :: d :: Nil =>
            if (!b.charAt(0).isDigit || !d.charAt(0).isDigit || b.toInt == 0 || c.toInt == 0 || d.toInt == 0) {
              incorrectFormat(head, next, acc)
            }
            else if (a.getClass.toString != "class java.lang.String" ||
              BigDecimal.valueOf(b.toDouble).getClass.toString != "class scala.math.BigDecimal" ||
              c.toInt.getClass.toString != "int" ||
              BigDecimal.valueOf(d.toDouble).getClass.toString != "class scala.math.BigDecimal") {
              incorrectFormat(head, next, acc)
            }
            else checkFormat(next, head +: acc)
          case _ => incorrectFormat(head, next, acc)

        }
      case Nil => acc
    }

  @tailrec
  def createSkuFromInputs(list: List[List[String]], acc: List[StockKeepingUnit] = Nil): Map[String, StockKeepingUnit] = list match {
    case ::(head, next) =>
      if (head.last == "none")
        createSkuFromInputs(
          next,
          acc :+ StockKeepingUnit(head.head,
            BigDecimal.valueOf(head(1).toDouble),
            None)
        )
      else createSkuFromInputs(
        next,
        acc :+ StockKeepingUnit(head.head,
          BigDecimal.valueOf(head(1).toDouble),
          Option(Discount(head(2).toInt, BigDecimal.valueOf(head.last.toDouble)))))

    case Nil => acc.groupMap(k => k.name)(f =>
      StockKeepingUnit(f.name, f.unitPrice, f.discount)).map {
      case (key, value) => key -> value.head
    }
  }
}


