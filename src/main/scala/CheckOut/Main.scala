package CheckOut

import CheckOut.CurrentlyAvailableProducts.{createNewSKUMapIfThereAreNewSKUsAvailable, currentlyAvailableProducts}
import CheckOut.Store._

import scala.annotation.tailrec

object Main extends App {
  val newListOfProducts: Map[String, StockKeepingUnit] = {
    if(args.length == 0)
      currentlyAvailableProducts
    else
      println(args(0))
    createNewSKUMapIfThereAreNewSKUsAvailable(args(0))
  }

  @tailrec
  def newShopper(): Unit = {

    val basketContents: String = scala.io.StdIn.readLine("What's in your basket? :-")

    val bas = makeBasket(basketContents, currentlyAvailableProducts = newListOfProducts)

    println(bas.transaction)

    newShopper()
  }
  newShopper()

}




