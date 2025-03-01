package object Multiplicacion{
  
  def PeasantAlgorithm(x: Int, y: Int): Int = {
    if (x == 0) 0
    else if (x % 2 == 0) PeasantAlgorithm(x/2, y+y)
    else PeasantAlgorithm(x/2, y+y) + y
  }

  def PeasantAlgorithmIt(a: Int, b: Int): Int = {
    def iterativeAlgorithm(a: Int, b: Int, result: Int): Int = {
      def isOdd: Int = iterativeAlgorithm(a / 2, b + b, result)
      def isOven: Int = iterativeAlgorithm(a / 2, b + b, result + b)
      if (a != 0) if (a%2==0) isOdd else isOven else result
    }
    iterativeAlgorithm(a, b, 0)
  }

  def splitMultiply(a: Int, b: Int): Int = {

    def numberDigits(c: Int, nDigits: Int): Int = if(c == 0)  nDigits  else numberDigits(c / 10, nDigits + 1);

    def abs(x: Int) = if(x >= 0) a else -x

    if (abs(a) < 10 && abs(b) < 10) return a * b

    val numberDigitA: Int = numberDigits(a, 0) / 2
    val numberDigitB: Int = numberDigits(b, 0) / 2
    val nFinal: Int = if (numberDigitA >= numberDigitB) numberDigitA else numberDigitB //PREGUNTAR AL PROFESOR...

    def defTopPart(d: Int, nDigits: Int): Int = d / Math.pow(10, nDigits).toInt
    def defBottomPart(e: Int, numberDigit: Int): Int = e % math.pow(10, numberDigit).toInt

    val topPartA = defTopPart(a,nFinal)
    val bottomPartA = defBottomPart(a,nFinal)
    val topPartB = defTopPart(b, nFinal)
    val bottomPartB = defBottomPart(b, nFinal)

    def firstStep: Int = Math.pow(10, nFinal*2).toInt * splitMultiply(topPartA,topPartB)
    def secondStep: Int = Math.pow(10, nFinal).toInt * ( splitMultiply(bottomPartA,topPartB) + splitMultiply(bottomPartB,topPartA) )
    def thirdStep: Int = splitMultiply(bottomPartA,bottomPartB)

    def algorithmMethod(): Int = firstStep + secondStep + thirdStep

    algorithmMethod();
  }


  def fastMultiply(a: Int, b: Int): Int = {
    def numberDigits(c: Int, nDigits: Int): Int = if (c == 0) nDigits else numberDigits(c / 10, nDigits + 1)

    def abs(x: Int) = if(x >= 0) a else -x

    if (abs(a) < 10 && abs(b) < 10) return a * b

    val numberDigitA: Int = numberDigits(a, 0) / 2
    val numberDigitB: Int = numberDigits(b, 0) / 2
    val nFinal: Int = if (numberDigitA >= numberDigitB) numberDigitA else numberDigitB

    def defTopPart(d: Int, nDigits: Int): Int = d / Math.pow(10, nDigits).toInt

    def defBottomPart(e: Int, numberDigit: Int): Int = e % math.pow(10, numberDigit).toInt

    val topPartA = defTopPart(a, nFinal)
    val bottomPartA = defBottomPart(a, nFinal)
    val topPartB = defTopPart(b, nFinal)
    val bottomPartB = defBottomPart(b, nFinal)

    val topRecycled = fastMultiply(topPartA,topPartB)
    val bottomRecycled = fastMultiply(bottomPartA, bottomPartB)

    def firstStep: Int = Math.pow(10, nFinal * 2).toInt * topRecycled

    def secondStep: Int = Math.pow(10, nFinal).toInt * (topRecycled + bottomRecycled - fastMultiply((topPartA - bottomPartA),(topPartB - bottomPartB)))

    def thirdStep: Int = bottomRecycled

    def algorithmMethod(): Int = firstStep + secondStep + thirdStep

    algorithmMethod()

  }
}
