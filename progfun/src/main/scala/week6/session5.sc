val words = List("rat", "sat", "test", "hit", "git", "Java", "lava")

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

val charCode: Map[Char, Char] =
  mnem flatMap  {case (digit, str) => str.map(_ -> digit)}

val charCode2: Map[Char, Char] =
  for {
    (digit, str) <- mnem
    ltr <- str
  } yield ltr -> digit

def wordCode(word: String): String =
  word.filter(_.isLetter).toUpperCase map charCode.withDefaultValue('\0')

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(Nil)
  else
    (for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest).toSet

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

wordCode("Java")
wordCode("Ñe")

encode("7288378")
translate("7288378")