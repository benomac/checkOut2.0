def leftOrRight(int: Int): Either[String, Int] =
  if(int % 2 == 0) Right(10)
  else Left("ten")

leftOrRight(8)
leftOrRight(7) match {
  case Right(v) => v
  case Left(v) => v
}