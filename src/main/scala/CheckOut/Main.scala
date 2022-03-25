package CheckOut

import CheckOut.CurrentlyAvailableProducts.{createNewSKUMapIfThereAreNewSKUsAvailable, currentlyAvailableProducts}
import CheckOut.Parsers.{getListOfProducts, splitCommandLineArgs}
import CheckOut.Store._

object Main extends App {

  val splitArgs: List[String] = splitCommandLineArgs(args)
  val justStockKeepingUnits = getListOfProducts(splitArgs)
  val availableProducts: Map[String, StockKeepingUnit] = createNewSKUMapIfThereAreNewSKUsAvailable(justStockKeepingUnits)
  val basket = makeBasket("aaaebqc", currentlyAvailableProducts = availableProducts)
  println(basket.transaction)
  println(createNewSKUMapIfThereAreNewSKUsAvailable(justStockKeepingUnits))

}




