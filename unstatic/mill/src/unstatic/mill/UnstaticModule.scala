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