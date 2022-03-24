package CheckOut

import scala.annotation.tailrec

object Store {


  case class StockKeepingUnit(name: String, unitPrice: BigDecimal, discount: Option[Discount])

  case class Discount(NumberOfItems: Int, forThePrice: BigDecimal)

  case class Basket(productsToBePaidFor: List[StockKeepingUnit]) {

    def transaction: BigDecimal = {
      val prdToSkuMap: Map[String, List[StockKeepingUnit]] = productsToBePaidFor.groupBy(_.name)

      prdToSkuMap.map {
        case (_, Nil) => BigDecimal.valueOf(0)
        case (skuName: String, skus: List[StockKeepingUnit]) => calculate(skus.head, skus.size)
      }.sum
    }

    def calculate(item: StockKeepingUnit, numberOfSku: Int): BigDecimal = item match {
      case StockKeepingUnit(_, price, None) => price * numberOfSku
      case StockKeepingUnit(_, price, discount) =>
        val d: Discount = discount.get
        ((numberOfSku / d.NumberOfItems) * d.forThePrice) + (numberOfSku % d.NumberOfItems) * price
    }
  }

  @tailrec
  def makeBasket(items: String, acc: List[StockKeepingUnit] = Nil, currentlyAvailableProducts: Map[String, StockKeepingUnit]): Basket =
    if (items == "") Basket(acc)
    else {
      if (!currentlyAvailableProducts.contains(items.head.toString)) {
        println(s"${items.head} is not an valid product.")
        makeBasket(items.tail, acc, currentlyAvailableProducts)
      }
      else
        makeBasket(items.tail, acc :+ currentlyAvailableProducts(items.head.toString), currentlyAvailableProducts)
    }
}
