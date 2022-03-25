import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, char, sp}

val p: Parser[Char] = Parser.anyChar

val g: Serializable = p.parse("t") match {
  case Left(value) => value
  case Right(value) => value._2
}
// res0: Either[Error, Tuple2[String, Char]] = Right((,t))
p.parse("")
// res1: Either[Error, Tuple2[String, Char]] = Left(Error(0,NonEmptyList(InRange(0,,))))
p.parse("two")
// res2: Either[Error, Tuple2[String, Char]] = Right((wo,t))

case class CharWrapper(value: Char)

val p1: Parser[CharWrapper] = Parser.anyChar.map(char => CharWrapper(char))

val foo = p1.parse("ww") match {
  case Left(value) => println(s"$value does not exist")
  case Right(value) => value._2
}

val p2: Parser[(Char, Char, BigDecimal, Char, Int, Char, BigDecimal)] =
  alpha ~ char ~