import mill._
import mill.scalalib._
import mill.scalalib.publish._

object Dependency {
  object Test {
    //val Scalatest = ivy"org.scalatest::scalatest:3.2.15"
    val Scalacheck = ivy"org.scalacheck::scalacheck:1.17.0"
    val Utest = ivy"com.lihaoyi::utest:0.8.1"
  }
}

object unstatic extends ScalaModule with PublishModule {
  override def scalaVersion = "3.2.1"

  // supports Scala 3.2.1
  override def ammoniteVersion = "2.5.6"

  // override def scalacOptions = T{ Seq("-explain") }

  object test extends Tests with TestModule.Utest {
    override def scalaVersion = "3.2.1"

    override def ivyDeps = T{ super.ivyDeps() ++ Agg(Dependency.Test.Scalacheck, Dependency.Test.Utest) }
  }

  // publishing stuff
  //
  // modified from https://com-lihaoyi.github.io/mill/mill/Common_Project_Layouts.html#_publishing

  def publishVersion = "0.0.1-SNAPSHOT"

  def pomSettings = PomSettings(
    description = "Towards a static site generator generator (and dynamic sites too)",
    organization = "com.mchange",
    url = "https://github.com/swaldman/unstatic",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("swaldman", "unstatic"),
    developers = Seq(
      Developer("swaldman", "Steve Waldman", "https://github.com/swaldman")
    )
  )
}

