package unstatic.ztapir

import sttp.tapir.ztapir.*
import zio.*

class ZTSiteHttpServer(site : ZTSite)(using ZTMain.Config.Dynamic) extends ZIOAppDefault:
  override def run = ZTMain.serve(site).debug.exitCode

