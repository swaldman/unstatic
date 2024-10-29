package unstatic.ztapir.simple

import unstatic.*
import unstatic.UrlPath.*

import scala.collection.immutable

import unstatic.ztapir.ZTSite

object Related:
  private def defaultRenderList( site : ZTSite, renderedOnSiteRootedPath : Rooted, related : Related, relatedListClass : String = "related-list" ) : String =
    def defaultRenderItem( item : Related.Item ) : String =
      val (resolvedLinkUrl, absoluteUrl) =
        val fromId =
          site.siteRootedPathByUniqueIdentifier( item.linkUrlOrUid.dropWhile( _ == '#' ) ).map( srp => (srp, renderedOnSiteRootedPath.relativizeSibling(srp)) )
        fromId match
          case Some( siteRootedUrl, relativeUrl ) => (relativeUrl.toString(), site.absFromSiteRooted(siteRootedUrl).toString)
          case None                => (item.linkUrlOrUid, item.linkUrlOrUid)
      val text =
        ( item.author, item.title ) match
          case ( Some(author), Some(title) ) => s"${author} &mdash; ${title}"
          case ( Some(author), None        ) => author
          case ( None,         Some(title) ) => title
          case ( None,         None        ) => absoluteUrl
      s"""  <li><a href="${resolvedLinkUrl}">${text}</a></li>"""
    s"""<ol class="${relatedListClass}">""" + LINESEP + related.items.map( defaultRenderItem ).mkString( LINESEP ) + LINESEP + "</ol>"

  def defaultRender(
    site                     : ZTSite,
    renderedOnSiteRootedPath : Rooted,
    related                  : Related,
    relatedDivClass          : String = "related",
    relatedTitleClass        : String = "related-title",
    relatedListClass         : String = "related-list"
  ) : String =
    val renderRelatedTitle = related.category.fold(related.base)(c => s"${related.base} &mdash; ${c}")
    val sb = new java.lang.StringBuilder
    sb.append(s"""<div class="${relatedDivClass}">""")
    sb.append(LINESEP)
    sb.append(s"""  <div class="${relatedTitleClass}">""")
    sb.append(LINESEP)
    sb.append(defaultRenderList(site,renderedOnSiteRootedPath,related, relatedListClass))
    sb.append(LINESEP)
    sb.append( """  </div>""")
    sb.append(LINESEP)
    sb.append( """</div>""")
    sb.toString

  def defaultRenderMulti(
    site                     : ZTSite,
    renderedOnSiteRootedPath : Rooted,
    relatedMulti             : Related.Multi,
    relatedMultiClass        : String = "related-multi",
    relatedMultiTitleClass   : String = "related-multi-title",
    relatedDivClass          : String = "related",
    relatedTitleClass        : String = "related-title",
    relatedListClass         : String = "related-list"
  ) : String =
    val sb = new java.lang.StringBuilder
    sb.append(s"""<div class="${relatedMultiClass}">""")
    sb.append(LINESEP)
    relatedMulti.title.foreach( title => sb.append(s"""  <div class="${relatedMultiTitleClass}">${title}</div>""") )
    sb.append(LINESEP)
    relatedMulti.relateds.foreach(related => defaultRender(site, renderedOnSiteRootedPath, related, relatedDivClass, relatedTitleClass, relatedListClass))
    sb.append("</div>")
    sb.toString

  def defaultRenderSingleOrMulti(
    site                     : ZTSite,
    renderedOnSiteRootedPath : Rooted,
    relatedOrMulti           : Related | Related.Multi,
    relatedMultiClass        : String = "related-multi",
    relatedMultiTitleClass   : String = "related-multi-title",
    relatedDivClass          : String = "related",
    relatedTitleClass        : String = "related-title",
    relatedListClass         : String = "related-list"
  ) : String =
    relatedOrMulti match
      case related : Related       => defaultRender(site,renderedOnSiteRootedPath,related,relatedDivClass,relatedTitleClass,relatedListClass)
      case multi   : Related.Multi => defaultRenderMulti(site,renderedOnSiteRootedPath,multi,relatedMultiClass,relatedMultiTitleClass,relatedDivClass,relatedTitleClass,relatedListClass)

  case class Multi( relateds : immutable.Seq[Related], title : Option[String] )
  case class Item( linkUrlOrUid : String, author : Option[String], title : Option[String] )
case class Related( base : String = "Related", category : Option[String], items : immutable.Seq[Related.Item] )

