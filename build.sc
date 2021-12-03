import mill._, scalalib._

object advent extends ScalaModule {
  def scalaVersion = "2.13.7"
  override def ivyDeps = Agg(
    ivy"org.tpolecat::atto-core:0.9.5",
    ivy"org.typelevel::cats-effect:3.3.0",
    ivy"org.typelevel::cats-core:2.7.0",
    ivy"co.fs2::fs2-core:3.2.2",
    ivy"co.fs2::fs2-io:3.2.2",
  )
}
