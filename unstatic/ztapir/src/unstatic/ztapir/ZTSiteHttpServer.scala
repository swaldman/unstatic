package unstatic.ztapir

import sttp.tapir.ztapir.*
import zio.*

class ZTSiteHttpServer(site : ZTSite)(using ZTSite.Dynamic.Config) extends ZIOAppDefault:
  override def run = ZTSite.Dynamic.serve(site).debug.exitCode

