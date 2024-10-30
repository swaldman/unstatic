package unstatic.ztapir.simple

import unstatic.*
import unstatic.UrlPath.*

import scala.collection.immutable

import unstatic.ztapir.ZTSite

object Related:
  private def defaultRenderList( site : ZTSite, renderedOnSiteRooted : Rooted, mbMediaDirSiteRooted : Option[Rooted], related : Related, relatedListClass : String = "related-list" ) : String =
    def defaultRenderItem( item : Related.Item ) : String =
      val (resolvedLinkUrl, absoluteUrl) =
        val maybeReplacedHashSpecial = site.replaceMaybeHashSpecial(renderedOnSiteRooted.toString(),renderedOnSiteRooted,item.href,mbMediaDirSiteRooted,false)
        if maybeReplacedHashSpecial == item.href then        // if unreplaced, just the URL
          ( maybeReplacedHashSpecial, maybeReplacedHashSpecial )
        else                                                         // if replaced
          val absUrl =
            UrlPath.parse(maybeReplacedHashSpecial) match
              case rel    : Rel    => site.absFromSiteRooted( renderedOnSiteRooted.resolveSibling(rel) ).toString()
              case rooted : Rooted => site.absFromSiteRooted( rooted ).toString()
              case abs    : Abs    => abs.toString()
          ( maybeReplacedHashSpecial, absUrl )
      val text =
        ( item.author, item.title ) match
          case ( Some(author), Some(title) ) => s"${author} &mdash; ${title}"
          case ( Some(author), None        ) => author
          case ( None,         Some(title) ) => title
          case ( None,         None        ) => absoluteUrl
      s"""  <li><a href="${resolvedLinkUrl}">${text}</a></li>"""
    s"""<ul class="${relatedListClass}">""" + LINESEP + related.items.map( defaultRenderItem ).mkString( LINESEP ) + LINESEP + "</ul>"

  def defaultRender(
    site                 : ZTSite,
    renderedOnSiteRooted : Rooted,
    mbMediaDirSiteRooted : Option[Rooted],
    related              : Related,
    relatedDivClass      : String = "related",
    relatedTitleClass    : String = "related-title",
    relatedListClass     : String = "related-list"
  ) : String =
    val renderRelatedTitle = related.category.fold(related.base)(c => s"${related.base} &mdash; ${c}")
    val sb = new java.lang.StringBuilder
    sb.append(s"""<div class="${relatedDivClass}">""")
    sb.append(LINESEP)
    sb.append(s"""  <div class="${relatedTitleClass}">${renderRelatedTitle}</div>""")
    sb.append(LINESEP)
    sb.append(defaultRenderList(site,renderedOnSiteRooted,mbMediaDirSiteRooted,related,relatedListClass))
    sb.append(LINESEP)
    sb.append(LINESEP)
    sb.append( """</div>""")
    sb.toString

  def defaultRenderMulti(
    site                     : ZTSite,
    renderedOnSiteRooted : Rooted,
    mbMediaDirSiteRooted : Option[Rooted],
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
    relatedMulti.relateds.foreach(related => defaultRender(site, renderedOnSiteRooted, mbMediaDirSiteRooted, related, relatedDivClass, relatedTitleClass, relatedListClass))
    sb.append("</div>")
    sb.toString

  def defaultRenderSingleOrMulti(
    site                     : ZTSite,
    renderedOnSiteRooted : Rooted,
    mbMediaDirSiteRooted : Option[Rooted],
    relatedOrMulti           : Related | Related.Multi,
    relatedMultiClass        : String = "related-multi",
    relatedMultiTitleClass   : String = "related-multi-title",
    relatedDivClass          : String = "related",
    relatedTitleClass        : String = "related-title",
    relatedListClass         : String = "related-list"
  ) : String =
    relatedOrMulti match
      case related : Related       => defaultRender(site,renderedOnSiteRooted,mbMediaDirSiteRooted,related,relatedDivClass,relatedTitleClass,relatedListClass)
      case multi   : Related.Multi => defaultRenderMulti(site,renderedOnSiteRooted,mbMediaDirSiteRooted,multi,relatedMultiClass,relatedMultiTitleClass,relatedDivClass,relatedTitleClass,relatedListClass)

  case class Multi( relateds : immutable.Seq[Related], title : Option[String] )
  case class Item( href : String, author : Option[String] = None, title : Option[String] )
case class Related( base : String = "Related", category : Option[String] = None, items : immutable.Seq[Related.Item] )

