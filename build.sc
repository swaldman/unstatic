import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.define.Target

trait UnstaticBuildModule extends ScalaModule with PublishModule {
  val UnstaticVersion = "0.0.1-SNAPSHOT"

  val TapirVersion = "1.2.6"

  object Dependency {
    object ZTapir {
      val Untemplate = ivy"com.mchange::untemplate:0.0.2"
      val TapirZio = ivy"com.softwaremill.sttp.tapir::tapir-zio:${TapirVersion}"
      val TapirZioHttpServer = ivy"com.softwaremill.sttp.tapir::tapir-zio-http-server:${TapirVersion}"
    }
    object Test {
      //val Scalatest = ivy"org.scalatest::scalatest:3.2.15"
      val Scalacheck = ivy"org.scalacheck::scalacheck:1.17.0"
      val Utest = ivy"com.lihaoyi::utest:0.8.1"
    }
  }

  override def scalaVersion = "3.2.1"

  // supports Scala 3.2.1
  override def ammoniteVersion = "2.5.6"

  // override def scalacOptions = T{ Seq("-explain") }

  private def pomSettings(name: String) = PomSettings(
    description = "Towards a static site generator generator (and dynamic sites too)",
    organization = "com.mchange",
    url = "https://github.com/swaldman/unstatic",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("swaldman", name),
    developers = Seq(
      Developer("swaldman", "Steve Waldman", "https://github.com/swaldman")
    )
  )

  def publishName: Target[String]

  override def publishVersion = T{UnstaticVersion}

  override def pomSettings = T{pomSettings(publishName())}
}

object unstatic extends UnstaticBuildModule {

  override def publishName = T{"unstatic"}

  object test extends Tests with TestModule.Utest {
    override def scalaVersion = T{unstatic.scalaVersion()}
    override def ivyDeps = T{ super.ivyDeps() ++ Agg(Dependency.Test.Scalacheck, Dependency.Test.Utest) }
  }

  object ztapir extends UnstaticBuildModule {

    override def moduleDeps = Seq(unstatic)

    override def publishName = T{"unstatic-ztapir"}

    override def ivyDeps = T {
      super.ivyDeps() ++
        Agg(
          Dependency.ZTapir.TapirZio,
          Dependency.ZTapir.TapirZioHttpServer,
          Dependency.ZTapir.Untemplate,
        )
    }
  }
}

