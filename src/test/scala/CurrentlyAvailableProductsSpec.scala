import CheckOut.CurrentlyAvailableProducts.{checkForExtraProducts, createNewSKUMapIfThereAreNewSKUsAvailable, currentlyAvailableProducts}
import CheckOut.Parsers.{getListOfProducts, splitCommandLineArgs}
import CheckOut.Store.{Discount, StockKeepingUnit}

class CurrentlyAvailableProductsSpec extends munit.FunSuite {
  val expectedMap = Map(
    "e" -> StockKeepingUnit("e",0.4,Some(Discount(3,1.0))),
    "f" -> StockKeepingUnit("f",0.4,None),
    "a" -> StockKeepingUnit("a",0.5,Some(Discount(3,1.3))),
    "b" -> StockKeepingUnit("b",0.3,Some(Discount(2,0.45))),
    "c" -> StockKeepingUnit("c",0.2,None),
    "d" -> StockKeepingUnit("d",0.15,None))

  val testListOfPotentialProducts: Array[String] = Array("e 0.40 3 1.00,f 00.4 none,fail")

  val testListOfSkus = List(StockKeepingUnit("e",0.4,Some(Discount(3,1.0))), StockKeepingUnit("f",0.4,None))

  test("splitCommandLineArgs") {
    assertEquals(splitCommandLineArgs(Array("e 0.40 3 1.00,f 00.4 none,fail")
    ), List("e 0.40 3 1.00", "f 00.4 none", "fail"))
  }

  test("checkForExtraProducts") {
    assertEquals(checkForExtraProducts(Array("e 0.40 3 1.00,f 00.4 none,fail")
    ), expectedMap)
  }

  test("checkForExtraProducts") {
    assertEquals(checkForExtraProducts(Array.empty
    ), expectedMap)
  }

  test("getListOfProducts") {
    assertEquals(getListOfProducts(testListOfPotentialProducts), testListOfSkus)
  }

  test("createNewSKUMapIfThereAreNewSKUsAvailable") {
    assertEquals(createNewSKUMapIfThereAreNewSKUsAvailable(testListOfSkus), expectedMap)
  }

  test("createNewSKUMapIfThereAreNewSKUsAvailable") {
    assertEquals(createNewSKUMapIfThereAreNewSKUsAvailable(Nil), currentlyAvailableProducts)
  }



}
