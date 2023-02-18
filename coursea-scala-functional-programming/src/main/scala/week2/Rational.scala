package week2

class Rational (x : Int, y: Int){//constructor
  require(y!=0,"denominator must be non 0")
  def this(x: Int) = this(x,1) //secondar constructor

  private def gdc(a: Int, b: Int): Int = if (b==0) a else gdc(b, a % b)
  private def abs(a: Int)=if (a>=0) a else -a
  private val g = abs(gdc(x,y))
  val numer = x / g
  val denom = y / g
  //ADDITION
  def add(that: Rational): Rational = {
      new Rational(
        this.numer*that.denom+that.numer*this.denom,
        this.denom*that.denom
      )
  }
  def + (that: Rational): Rational = {
    new Rational(
      this.numer*that.denom + that.numer*this.denom,
      this.denom*that.denom
    )
  }
  //Negate
  def neg: Rational = new Rational(-numer,denom)
  def unary_- : Rational = new Rational(-numer,denom) //prefix operation

  //substraction
  def sub(that:Rational): Rational = add(that.neg)
  def - (that: Rational): Rational = this + -that

}
