package us.elifk

import cats._
import cats.implicits._

object Semigroups {

  implicit val expenseSemiGroup: Semigroup[Expense] = Semigroup.instance(
    (a, b) => new Expense(Math.max(a.id, b.id), a.amount+b.amount)
  )

  case class Expense(id: Long, amount: Double)

  def reduceThings2[T : Semigroup](list: List[T]): T =
    list.reduce(_ |+| _)

  def combineFold[T : Monoid](list: List[T]): T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)
  import cats.instances.map._

  val phoneBooks = List(
    Map("Alice" -> 234,
    "Bob" -> 345),
    Map("Ali" -> 455,
    "Ayşe" -> 32),
    Map("Fatma" -> 98)
  )
  import cats.instances.double._
  import cats.instances.list._
  import cats.instances.string._
  case class ShoppingCart(list: List[String], total: Double)
  implicit val scCombiner: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(),0),
    (a, b) => ShoppingCart(a.list |+| b.list, a.total |+| b.total)
  )

  def checkout(list: List[ShoppingCart]): ShoppingCart = combineFold(list)



  def main(args: Array[String]): Unit = {
    val combination = Semigroup.combine(Expense(3, 10), Expense(1,20))
    println(combination)

    println(combineFold(phoneBooks))
  }
}
