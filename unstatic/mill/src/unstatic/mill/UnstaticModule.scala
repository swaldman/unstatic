// Temporary (I hope) scala-cli build info

//> using publish.organization "com.mchange"
//> using publish.name "unstatic-mill"
//> using publish.repository "central"
//> using publish.license "Apache-2.0"
//> using publish.developer "swaldman|Steve Waldman|https://github.com/swaldman"

//> using scala "2.13.10"





package unstatic.mill

import mill._
import mill.define._
import mill.scalalib._

import untemplate.mill.UntemplateModule
import unstatic.ztapir.{ZTSite,ZTStaticGen}

// Expect compilation as 2.13!

trait UnstaticModule extends UntemplateModule {
  def ztSite() : ZTSite

  def unstaticGenerateStatic = T {
    ZTStaticGen.generateZTSite(ztSite(), T.dest.toNIO)
    PathRef(T.dest)
  }

}