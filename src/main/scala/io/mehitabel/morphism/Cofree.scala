package io.mehitabel.morphism

case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
