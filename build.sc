import $meta._

import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.define.Target

trait UnstaticBuildModule extends ScalaModule with PublishModule {
  val UnstaticVersion   = "0.3.0-SNAPSHOT"

  object Dependency {
    val UntemplateVersion = "0.1.3-SNAPSHOT"
    val TapirVersion      = "1.10.7"
    val FlexmarkVersion   = "0.64.8"

    val Untemplate = ivy"com.mchange::untemplate:${UntemplateVersion}" // mill.scalalib.Dep
    val Scribe     = ivy"com.outr::scribe:3.13.5"

    // it'd save some repetition if these could be Products. maybe a fun macro project?
    object ZTapir {
      val TapirFiles         = ivy"com.softwaremill.sttp.tapir::tapir-files:${TapirVersion}"
      val TapirZio           = ivy"com.softwaremill.sttp.tapir::tapir-zio:${TapirVersion}"
      val TapirZioHttpServer = ivy"com.softwaremill.sttp.tapir::tapir-zio-http-server:${TapirVersion}"
      val AudiofluidityRss   = ivy"com.mchange::audiofluidity-rss:0.0.7"
      val Jsoup              = ivy"org.jsoup:jsoup:1.17.2"
      val FlexmarkSeq        = Seq (
        ivy"com.vladsch.flexmark:flexmark:${FlexmarkVersion}",
        ivy"com.vladsch.flexmark:flexmark-ext-footnotes:${FlexmarkVersion}",
        ivy"com.vladsch.flexmark:flexmark-ext-tables:${FlexmarkVersion}",
        ivy"com.vladsch.flexmark:flexmark-ext-gfm-strikethrough:${FlexmarkVersion}",
        ivy"com.vladsch.flexmark:flexmark-ext-anchorlink:${FlexmarkVersion}",
      )
    }
    object Test {
      val Scalacheck = ivy"org.scalacheck::scalacheck:1.18.0"
      val Utest      = ivy"com.lihaoyi::utest:0.8.3"
    }
  }

  override def scalaVersion = "3.3.3"
  // override def scalacOptions = T{ Seq("-explain") }
  override def scalacOptions = T{ Seq("-deprecation") }

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

  override def publishVersion = T{UnstaticVersion}
  override def pomSettings    = T{pomSettings(artifactName())}
}

object unstatic extends RootModule with UnstaticBuildModule {
  override def artifactName = T{"unstatic"}
  override def ivyDeps = T{ super.ivyDeps() ++ Agg(Dependency.Untemplate, Dependency.Scribe) }

  object test extends ScalaTests {
    override def scalaVersion = T{unstatic.scalaVersion()}
    override def ivyDeps = T{ super.ivyDeps() ++ Agg(Dependency.Test.Scalacheck, Dependency.Test.Utest) }
    override def testFramework = "utest.runner.Framework"
  }
  object ztapir extends UnstaticBuildModule {
    override def moduleDeps = Seq(unstatic)
    override def artifactName = T{"unstatic-ztapir"}
    override def ivyDeps = T {
      super.ivyDeps() ++
        Agg(
          Dependency.ZTapir.TapirFiles,
          Dependency.ZTapir.TapirZio,
          Dependency.ZTapir.TapirZioHttpServer,
          Dependency.ZTapir.AudiofluidityRss,
          Dependency.ZTapir.Jsoup,
        ) ++ Dependency.ZTapir.FlexmarkSeq
    }
  }
}

