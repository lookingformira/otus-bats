package me.chuwy.otusbats

import scala.util.Try

trait Monad[F[_]] extends Functor[F] { self =>

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(self.map(fa)(f))

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A]

}

object Monad {

  implicit val optionMonad: Monad[Option] = new Monad[Option] {


    override def point[A](a: A): Option[A] = Option(a)

    override def flatten[A](fa: Option[Option[A]]): Option[A] = fa.flatten

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {


    override def point[A](a: A): List[A] = List(a)

    override def flatten[A](fa: List[List[A]]): List[A] = fa.flatten

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val tryMonad: Monad[Try] = new Monad[Try] {


    override def point[A](a: A): Try[A] = Try(a)

    override def flatten[A](fa: Try[Try[A]]): Try[A] = fa.flatten

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
  }

  implicit class FlatMapOps[F[_], A](fa: F[A])(implicit ev: Monad[F]) {
    def flatMap_[B](f: A => F[B]): F[B] = ev.flatMap(fa)(f)

  }

  implicit class FlattenOps[F[_], A](fa: F[F[A]])(implicit ev: Monad[F]) {
    def flatten_ : F[A] = ev.flatten(fa)
  }

}
