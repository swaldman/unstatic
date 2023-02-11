package unstatic.ztapir

import unstatic.*

import scala.collection.immutable

trait ZTStaticResources[S <: ZTSite] extends StaticResources[S] with ZTEndpointBinding.Source with StaticLocationBinding.Source:
  // Keys are site-rooted, but endpoints are server rooted!
  def endpointBindings: immutable.Seq[ZTEndpointBinding] =
    locationBindings.map { case StaticLocationBinding(siteRootedPath, source) => ZTEndpointBinding.staticDirectoryServing( siteRootedPath, site, source, NoIdentifiers ) }

