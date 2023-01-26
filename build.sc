import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.define.Target

trait UnstaticBuildModule extends ScalaModule with PublishModule {
  val UnstaticVersion   = "0.0.1-SNAPSHOT"

  object Dependency {
    val UntemplateVersion = "0.0.2"
    val TapirVersion      = "1.2.6"
    val MillVersion       = "0.10.10"

    // it'd save some repetition if these could be Products. maybe a fun macro project?
    object ZTapir {
      val Untemplate         = ivy"com.mchange::untemplate:${UntemplateVersion}" // mill.scalalib.Dep
      val TapirZio           = ivy"com.softwaremill.sttp.tapir::tapir-zio:${TapirVersion}"
      val TapirZioHttpServer = ivy"com.softwaremill.sttp.tapir::tapir-zio-http-server:${TapirVersion}"
    }
    object Test {
      val Scalacheck = ivy"org.scalacheck::scalacheck:1.17.0"
      val Utest      = ivy"com.lihaoyi::utest:0.8.1"
    }
    object Mill {
      val MillMain       = ivy"com.lihaoyi::mill-main:${MillVersion}".withDottyCompat("3.2.1")
      val MillScalalib   = ivy"com.lihaoyi::mill-scalalib:${MillVersion}".withDottyCompat("3.2.1")
      val UntemplateMill = ivy"com.mchange::untemplate-mill:${UntemplateVersion}".withDottyCompat("3.2.1")
    }
  }

  override def scalaVersion = "3.2.1"
  override def ammoniteVersion = "2.5.6" // supports Scala 3.2.1
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
  override def pomSettings    = T{pomSettings(publishName())}
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
  /*
  object mill extends UnstaticBuildModule {
    override def moduleDeps = Seq(ztapir)

    override def scalaVersion = T {"2.13.10"}
    override def scalacOptions = T {super.scalacOptions() :+ "-Ytasty-reader"}
    override def publishName = T {"unstatic-mill"}

    override def ivyDeps = T {
      super.ivyDeps() ++
        Agg(
          Dependency.Mill.MillMain,
          Dependency.Mill.MillScalalib,
          Dependency.Mill.UntemplateMill,
        )
    }
  }

  override def transitiveCompileIvyDeps = T {
    val out = super.transitiveIvyDeps()
    println(out)
    Agg(ivy"org.scala-lang:scala3-library_3.2.1:3.2.1")
    println( s"out.size: ${out.size}")
    out // a crude way to get rid of bad dependencies, wherever they are from
      .map(dep => {
        println(dep);
        dep
      })
      .filter(dep => dep.toString.indexOf("scala3-library_2.13") < 0)
      .filter(dep => dep.toString.indexOf("untemplate_2.13") < 0)
  }
  */
}

