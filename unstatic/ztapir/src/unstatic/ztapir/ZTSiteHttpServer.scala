package unstatic.ztapir

import sttp.tapir.ztapir.*
import zio.*

class ZTSiteHttpServer(site : ZTSite)(using ZTSiteDynamic.Config) extends ZIOAppDefault:
  override def run = ZTSiteDynamic.serve(site).debug.exitCode

