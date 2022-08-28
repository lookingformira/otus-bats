package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)

  implicit val intShow: Show[Int] = new Show[Int] {
    override def show(a: Int): String = s"$a"
  }

  implicit val stringShow: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  implicit val booleanShow: Show[Boolean] = new Show[Boolean] {
    override def show(a: Boolean): String = if (a) "true" else "false"
  }


  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = new Show[List[A]] {
    override def show(a: List[A]): String = a.foldLeft("") {
      case (a1, a2) => s"$a1 ${a2.show}"
    }
  }

  implicit def setShow[A](implicit ev: Show[A]): Show[Set[A]] = new Show[Set[A]] {
    override def show(a: Set[A]): String = a.foldLeft(""){
      case (a1, a2) => s"$a1 ${a2.show}"
    }
  }


  // 2. Summoner (apply)

  def apply[A](implicit show: Show[A]): Show[A] = show

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String =
      ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String = {
    val listString = list.foldLeft(""){
      case (a1, a2) => s"$a1$separator ${a2.show}"
    }
    s"$begin$listString$end"
  }


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = new Show[A] {
    override def show(a: A): String = a.toString
  }
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = new Show[A] {
    override def show(a: A): String = f(a)
  }

}
