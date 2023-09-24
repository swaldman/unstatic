package unstatic.ztapir

import scala.collection.*

import unstatic.*, UrlPath.*

import java.nio.file.{Files, Path as JPath}

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

object ZTSite:
  trait Composite extends ZTSite:
    def endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]
    def endpointBindings : immutable.Seq[ZTEndpointBinding] = endpointBindingSources.flatMap( _.endpointBindings )
  end Composite

  trait SingleStaticRootComposite( staticRootDir : JPath ) extends Composite:
    override val enforceUserContentFrom = Some(staticRootDir)

    private lazy val rootBinding = ZTEndpointBinding.staticDirectoryServing(Rooted.root, this, staticRootDir, immutable.Set("staticRoot"))
    override def endpointBindings : immutable.Seq[ZTEndpointBinding] = super.endpointBindings :+ rootBinding
  end SingleStaticRootComposite

trait ZTSite extends Site, ZTEndpointBinding.Source, ExposesDuplicateIdentifiers:
  override def allBindings : immutable.Seq[AnyBinding] = this.endpointBindings

  val enforceUserContentFrom : Option[JPath]

  lazy val siteRootedPathByIdentifier =
    allBindings.reverse.flatMap( b => b.identifiers.toSeq.map(id => (id, b.siteRootedPath)) ).toMap

  private def siteRootedPathIsDefined( siteRootedPath : Rooted, binding : ZTEndpointBinding ) : Boolean =
    binding match
      case fsd : ZTEndpointBinding.FromStaticDirectory =>
        val file = fsd.dir.resolve(siteRootedPath.unroot.toString)
        Files.exists(file)
      case other =>
        siteRootedPath == other.siteRootedPath

  def siteRootedPathIsDefined( siteRootedPath : Rooted ) : Boolean =
    this.endpointBindings.exists( binding => siteRootedPathIsDefined(siteRootedPath, binding) )

  /**
   * a "master switch", should not generally be used (html outputs determine their own hash-special resolution policy),
   * but implemented so we can compare generation times, understand just how performance-costly the current Jsoup-based
   * resolution approach is.
   *
   * override this to true to disable all hash special resolution
   */
  val disableAllResolveHashSpecials = false

  def publicReadOnlyHtml(siteLocation: SiteLocation, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String], resolveHashSpecials : Boolean, memoize : Boolean ) : ZTEndpointBinding =
    publicReadOnlyHtml(siteLocation.siteRootedPath, task, mediaDirSiteRooted, identifiers, resolveHashSpecials, memoize )

  def publicReadOnlyHtml(siteRootedLocation: Rooted, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String], resolveHashSpecials : Boolean, memoize : Boolean ) : ZTEndpointBinding =
    def resolvingTask : zio.Task[String] =
      task.map( rawHtml => htmlResolveHashSpecials(siteRootedLocation.toString(), siteRootedLocation, rawHtml, mediaDirSiteRooted, true ) )
    val base        : zio.Task[String] = if resolveHashSpecials then resolvingTask else task
    val mbMemoizing : zio.Task[String] = if memoize then base.memoize.flatten else base
    ZTEndpointBinding.publicReadOnlyHtml(siteRootedLocation, this, mbMemoizing, mediaDirSiteRooted, identifiers )

  def htmlResolveHashSpecials( sourceId : String, sourceSiteRooted : Rooted, unresolvedHtml : String, mbMediaDirSiteRooted : Option[Rooted], resolveEscapes : Boolean ) : String =
    if disableAllResolveHashSpecials then
      unresolvedHtml
    else
      val jsoupDoc = org.jsoup.Jsoup.parse(unresolvedHtml)
      mutateHtmlResolveHashSpecials( jsoupDoc, sourceId, sourceSiteRooted, mbMediaDirSiteRooted, resolveEscapes )
      jsoupDoc.outerHtml()

  def htmlFragmentResolveHashSpecials( sourceId : String, sourceSiteRooted : Rooted, unresolvedHtml : String, mbMediaDirSiteRooted : Option[Rooted], resolveEscapes : Boolean ) : String =
    if disableAllResolveHashSpecials then
      unresolvedHtml
    else
      val jsoupDoc = org.jsoup.Jsoup.parseBodyFragment(unresolvedHtml)
      mutateHtmlResolveHashSpecials( jsoupDoc, sourceId, sourceSiteRooted, mbMediaDirSiteRooted, resolveEscapes )
      jsoupDoc.body().html

  private def mutateHtmlResolveHashSpecials( parentElem : Element, sourceId : String, sourceSiteRooted : Rooted, mbMediaDirSiteRooted : Option[Rooted], resolveEscapes : Boolean ) : Unit =
    def mutateReplace(cssQuery : String, refAttr : String) : Unit =
      import scala.jdk.CollectionConverters._
      parentElem.select(cssQuery).asScala.foreach { elem =>
        val rawHref = elem.attr(refAttr)
        val shinyHref = replaceMaybeHashSpecial(sourceId, sourceSiteRooted, rawHref, mbMediaDirSiteRooted, resolveEscapes)
        elem.attr(refAttr, shinyHref)
      }

    mutateReplace("a","href")
    mutateReplace("img","src")
    mutateReplace("link","href")

  private def replaceMaybeHashSpecial( sourceId : String, sourceSiteRooted : Rooted, href : String, mbMediaDirSiteRooted : Option[Rooted], resolveEscapes : Boolean ) : String =
    if href.startsWith("##") then
      val content = href.drop(2)
      if content.startsWith("/") then
        val destSiteRooted = Rooted(content)
        if !siteRootedPathIsDefined(destSiteRooted) then
          throw new UnresolvedReference(sourceId, href, s"For internal site reference, no endpoint handles destination path '${destSiteRooted}'")
        sourceSiteRooted.relativizeSibling(destSiteRooted).toString()
      else if content.startsWith("./") then
        mbMediaDirSiteRooted match
          case Some( mediaDirSiteRooted ) =>
            val relpath = Rel(content.drop(2))
            val destSiteRooted = mediaDirSiteRooted.resolve(relpath)
            enforceUserContentFrom.foreach { jpath =>
              val expectedFile = jpath.resolve(destSiteRooted.unroot.toString())
              if (!Files.exists(expectedFile))
                throw new UnresolvedReference(sourceId, href, s"For media-dir reference, expected destination file does not exist: '${expectedFile}'", Some(expectedFile.toAbsolutePath.toString))
            }
            sourceSiteRooted.relativizeSibling(destSiteRooted).toString()
          case None =>
            throw new UnresolvedReference(sourceId, href, s"Special hash reference '${href}' is relative to a mediaDir, but no mediaDir is available in this context.")
            //scribe.warn(s"${sourceId}: Special hash reference '${href}' is relative to a mediaDir, but no mediaDir is available in this context. Left as-is.")
            //href
      else if content(0) == '\\' then
        if resolveEscapes then // we only want to unescape at once, at a page's last pass, otherwise we might accidentally unescape to something later resolves
          "##" + content.drop(1) // lose one backslash
        else
          href
      else
        val id = content
        siteRootedPathByIdentifier.get(id) match
          case Some(path) => sourceSiteRooted.relativizeSibling(path).toString()
          case None =>
            throw new UnresolvedReference(sourceId, href, s"Refers to identifier '${id}', but that identifier is unknown to this site.")
            // scribe.warn(s"${sourceId}: Special hash reference '${href}' could not be interpreted or resolved to an identifier, left as-is.")
            // href
    else
      href
