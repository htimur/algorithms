package de.khamrakulov.algorithms

object Chapter1 {

  def mergeSort(input: List[Int]) = {

    def merge(left: List[Int], right: List[Int]): Stream[Int] = (left, right) match {
      case (x :: xs, y :: ys) if x <= y => x #:: merge(xs, right)
      case (x :: xs, y :: ys) => y #:: merge(left, ys)
      case _ => if (left.isEmpty) right.toStream else left.toStream
    }

    def sort(input: List[Int], length: Int): List[Int] = input match {
      case Nil | List(_) => input
      case _ =>
        val middle = length / 2
        val (left, right) = input splitAt middle
        merge(sort(left, middle), sort(right, middle + length % 2)).toList
    }

    sort(input, input.length)
  }

  def multiplyKhwarizmi(x: Int, y: Int): Int = {
    def isEven(num: Int): Boolean = num % 2 == 0
    def calc(x1:Int, y1: Int): Int = y1 match {
      case 0 => 0
      case _ if isEven(y1) => 2 * multiplyKhwarizmi(x1, y1 / 2)
      case _ if !isEven(y1) => x + 2 * multiplyKhwarizmi(x1, y1 / 2)
    }

    calc(x, y)
  }

  def multiplyKaratsuba(x: String, y: String):String = (x, y) match {
    case _ if x.toInt < 10 || y.toInt < 10 => (x.toInt * y.toInt).toString
    case _ =>
      val m = List(x.length, y.length).max
      val m2 = m/2
      val (num11, num12) = x splitAt m2
      val (num21, num22) = y splitAt m2
      val z0 = multiplyKaratsuba(num12, num22).toInt
      val z1 = multiplyKaratsuba((num11.toInt + num12.toInt).toString, (num21.toInt + num22.toInt).toString).toInt
      val z2 = multiplyKaratsuba(num11, num21).toInt

      ((z2 * math.pow(10, 2 * m2).toInt) + ((z1 - z2 - z0) * math.pow(10, m2).toInt) + z0).toString
  }
}
