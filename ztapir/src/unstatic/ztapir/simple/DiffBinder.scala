package unstatic.ztapir.simple

import scala.collection.immutable
import scala.jdk.CollectionConverters.*

import scala.util.control.NonFatal

import unstatic.UrlPath.*
import unstatic.ztapir.{ZTSite,ZTEndpointBinding}

import com.mchange.conveniences.throwable.*

import sttp.model.MediaType
import unstatic.UnstaticException
import java.nio.charset.StandardCharsets

import com.github.difflib.text.*

object DiffBinder:
  object DiffPathFinder:
    val Default : DiffPathFinder = ( origPath : Rooted, revisionBeforeSpec : String, revisionAfterSpec : Option[String] ) =>
      require( !origPath.isRoot && !origPath.isDir, s"'$origPath' represents a directory or root, we only convert individual leaf resources to prior revisions." )
      val origFilename = origPath.elements.last
      val lastDot = origFilename.lastIndexOf('.')
      def diffSuffix =  "-diff-" + revisionBeforeSpec + "-to-" + revisionAfterSpec.getOrElse("current")
      val newFilename =
        if lastDot == 0 then
          throw new IllegalArgumentException("'$origFilename' appears to be a dotfile. We do not retrieve or convert dotfiles.")
        else if lastDot > 0 then
          origFilename.substring(0,lastDot) + diffSuffix + origFilename.substring(lastDot)
        else
          origFilename + diffSuffix
      origPath.resolveSibling(newFilename)
  type DiffPathFinder = ( Rooted, String, Option[String] ) => Rooted
  class JavaDiffUtils( val site : ZTSite, val diffPathFinder : DiffPathFinder = DiffPathFinder.Default ) extends DiffBinder:
    def diffEndpointBinding(
      sourceBindingBySiteRootedPath : Rooted => ZTEndpointBinding,
      revisionPathFinder : RevisionBinder.RevisionPathFinder,
      origSiteRootedPath : Rooted,
      revisionBeforeSpec : String,
      revisionAfterSpec : Option[String], // if None, we're diffing against the current version
      identifiers : immutable.Set[String]
    ) : ZTEndpointBinding =
      def mayBeTextUtf8( mediaType : MediaType ) : Boolean =
        mediaType.isText && mediaType.charset.fold(true)( cs => cs.equalsIgnoreCase("utf8") || cs.equalsIgnoreCase("utf-8") )
      val diffSiteRootedPath = diffPathFinder( origSiteRootedPath, revisionBeforeSpec, revisionAfterSpec )  
      def beforeAfterBindings : Either[String,Tuple2[ZTEndpointBinding.Generable,ZTEndpointBinding.Generable]] =
        try
          val beforeRaw = sourceBindingBySiteRootedPath( revisionPathFinder( origSiteRootedPath, revisionBeforeSpec ) )
          val afterRaw =
            revisionAfterSpec match
              case Some( ras ) => sourceBindingBySiteRootedPath( revisionPathFinder( origSiteRootedPath, ras ) )
              case None => sourceBindingBySiteRootedPath( origSiteRootedPath )
          val ( before, after ) =
            (beforeRaw, afterRaw) match
              case ( bg : ZTEndpointBinding.Generable, ag : ZTEndpointBinding.Generable ) =>
                if (! mayBeTextUtf8(bg.contentType) ) throw new UnstaticException( s"Before resource appears not to be UTF-8 text. Only UTF-8 text currently supported." )
                if (! mayBeTextUtf8(ag.contentType) ) throw new UnstaticException( s"After resource appears not to be UTF-8 text. Only UTF-8 text currently supported." )
                Tuple2( bg, ag )
              case _ =>
                throw new UnstaticException( s"Diffs are generable only from generable bindings. At least one of the before/after bindings are not generable. [beforeRaw: ${beforeRaw}, afterRaw: ${afterRaw}]" )
          Right( before, after )
        catch
          case NonFatal(e) => Left( e.fullStackTrace )
      def diffUtf8BytesToMarkdown( beforeBytes : immutable.ArraySeq[Byte], afterBytes : immutable.ArraySeq[Byte] ) : String =
        val beforeString = new String( beforeBytes.unsafeArray.asInstanceOf[Array[Byte]], StandardCharsets.UTF_8 )
        val beforeLines = beforeString.lines.toList
        val afterString = new String( afterBytes.unsafeArray.asInstanceOf[Array[Byte]], StandardCharsets.UTF_8 )
        val afterLines = afterString.lines.toList
        // stealing from java-diff-utils' README example, https://github.com/java-diff-utils/java-diff-utils
        val diffRowGenerator =
          DiffRowGenerator.create()
            .showInlineDiffs(true)
            .inlineDiffByWord(true)
            .oldTag(_ => "~~")
            .newTag(_ => "**")
            .build()
        val rows = diffRowGenerator.generateDiffRows(beforeLines, afterLines).asScala
        val sb = new StringBuilder
        sb.append(s"|${revisionBeforeSpec}|${revisionAfterSpec}|" + LINESEP);
        sb.append("|--------|---------|" + LINESEP);
        rows.foreach: row =>
          sb.append(s"|${row.getOldLine()}|${row.getNewLine()}|" + LINESEP)
        sb.toString()  
      def beforeAfterToDiffMarkdown( tup : Tuple2[ZTEndpointBinding.Generable,ZTEndpointBinding.Generable] ) : Either[String,zio.Task[String]] =
        val task =
          for
            beforeBytes <- tup(0).bytesGenerator
            afterBytes <- tup(1).bytesGenerator
          yield
            diffUtf8BytesToMarkdown( beforeBytes, afterBytes )
        Right( task )
      def diffMarkdownEffectToHtml( markdownEffect : zio.Task[String] ) : Either[String,zio.Task[String]] =
        val title = s"Diff: ${revisionBeforeSpec} => ${revisionAfterSpec}"
        val out =
          markdownEffect.map: markdown =>
            s"""|<html>
                |  <head>
                |    <title>${title}</title>
                |    <style>
                |      body { overflow-x: scroll; }
                |    </style>
                |  </head>
                |  <body>
                |    <h1>${title}</h1>
                |    <hr>
                |${Flexmark.defaultMarkdownToHtml(markdown,None)}
                |  </body>
                |</html>""".stripMargin
        Right(out)
      val eitherOut =
        for
          tup        <- beforeAfterBindings
          mdEffect   <- beforeAfterToDiffMarkdown(tup)
          htmlEffect <- diffMarkdownEffectToHtml(mdEffect)
        yield
          ZTEndpointBinding.publicReadOnlyHtml(diffSiteRootedPath, site, htmlEffect, None, identifiers)
      eitherOut match
        case Right( success ) => success
        case Left ( oopsies ) =>
          val title = s"Error. Could not diff ${revisionBeforeSpec} => ${revisionAfterSpec}"
          scribe.warn( s"${title}. Generating an error report in its place: ${oopsies}" )
          val task =
            zio.ZIO.attempt:
              s"""|<html>
                  |  <head>
                  |    <title>${title}</title>
                  |  </head>
                  |  <body>
                  |    <h1>${title}</h1>
                  |    <hr>
                  |<pre><code>${oopsies}</code></pre>
                  |  </body>
                  |</html>""".stripMargin
          ZTEndpointBinding.publicReadOnlyHtml(diffSiteRootedPath, site, task, None, identifiers)

trait DiffBinder:
  def diffPathFinder : DiffBinder.DiffPathFinder
  def diffEndpointBinding(
    sourceBindingBySiteRootedPath : Rooted => ZTEndpointBinding,
    revisionPathFinder : RevisionBinder.RevisionPathFinder,
    origSiteRootedPath : Rooted,
    revisionBeforeSpec : String,
    revisionAfterSpec : Option[String], // if None, we're diffing against the current version
    identifiers : immutable.Set[String]
  ) : ZTEndpointBinding


