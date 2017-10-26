package io.mehitabel.morphism

case class Fix[F[_]](unFix: F[Fix[F]])
