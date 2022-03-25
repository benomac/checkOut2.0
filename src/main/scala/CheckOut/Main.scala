package CheckOut

import CheckOut.CurrentlyAvailableProducts.currentlyAvailableProducts
import CheckOut.Parsers.{getListOfProducts, skuItemParser, splitCommandLineArgs}
import CheckOut.Store._

import scala.annotation.tailrec

object Main extends App {


  val splitArgs: List[String] = splitCommandLineArgs(args)
  println(s"these are you args $splitArgs")


  println(getListOfProducts(splitArgs))
}




