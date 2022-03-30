package CheckOut

import CheckOut.CurrentlyAvailableProducts.{checkForExtraProducts, createNewSKUMapIfThereAreNewSKUsAvailable, currentlyAvailableProducts}
import CheckOut.Parsers.{commandLineArgsParser, getListOfProducts, splitCommandLineArgs}
import CheckOut.Store._

object Main extends App {

//  println("here" + args(0))
  val availableProducts = {
    if(args.length == 0)
      currentlyAvailableProducts
    else
      checkForExtraProducts(args)
  }
  val basketContents = scala.io.StdIn.readLine("Enter items list eg: abcd :-")
  val basket = makeBasket(basketContents, currentlyAvailableProducts = availableProducts)
  println(basket.transaction)

}




