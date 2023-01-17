import mill._
import mill.scalalib._

object Dependency {
  val Scalatest = ivy"org.scalatest::scalatest:3.2.15"
}

object unstatic extends ScalaModule {
  override def scalaVersion = "3.2.1"

  // supports Scala 3.2.1
  override def ammoniteVersion = "2.5.6"

  object test extends Tests with TestModule.ScalaTest {
    override def scalaVersion = "3.2.1"

    override def ivyDeps = T{ super.ivyDeps() ++ Agg(Dependency.Scalatest) }
  }
}

