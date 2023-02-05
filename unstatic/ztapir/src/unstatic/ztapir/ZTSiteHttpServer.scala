package unstatic.ztapir

import sttp.tapir.ztapir.*
import zio.*

class ZTSiteHttpServer(site : ZTSite)(using ZTSite.Config.Dynamic) extends ZIOAppDefault:
  override def run = ZTSite.serve(site).debug.exitCode

