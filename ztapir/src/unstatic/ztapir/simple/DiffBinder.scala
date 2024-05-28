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
import scala.annotation.tailrec

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
  object JavaDiffUtils:
    val ShortSpanLimit = 8
    val EqualsEdgeLength = 3
    assert( ShortSpanLimit > EqualsEdgeLength * 2, s"ShortSpanLimit ${ShortSpanLimit} should be at least twice EqualsEdgeLength ${EqualsEdgeLength}" )
  class JavaDiffUtils( val site : ZTSite, val diffPathFinder : DiffPathFinder = DiffPathFinder.Default ) extends DiffBinder:
    import JavaDiffUtils.*
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
      def escapeAngleBraces( s : String ) : String = s.flatMap( c => if c == '<' then "&lt;" else if c == '>' then "&gt;" else c.toString )
      def diffUtf8BytesToTable( beforeBytes : immutable.ArraySeq[Byte], afterBytes : immutable.ArraySeq[Byte] ) : String =
        val beforeString = escapeAngleBraces( new String( beforeBytes.unsafeArray.asInstanceOf[Array[Byte]], StandardCharsets.UTF_8 ) )
        val beforeLines = beforeString.lines.toList
        val afterString = escapeAngleBraces( new String( afterBytes.unsafeArray.asInstanceOf[Array[Byte]], StandardCharsets.UTF_8 ) )
        val afterLines = afterString.lines.toList
        // stealing from java-diff-utils' README example, https://github.com/java-diff-utils/java-diff-utils
        val diffRowGenerator =
          DiffRowGenerator.create()
            .showInlineDiffs(true)
            .inlineDiffByWord(true)
            .oldTag((tag, start) => if start then s"""<span class="OLD ${tag}">""" else "</span>")
            .newTag((tag, start) => if start then s"""<span class="NEW ${tag}">""" else "</span>")
            .build()
        val rows = immutable.LazyList.from(1).zip(diffRowGenerator.generateDiffRows(beforeLines, afterLines).asScala).map: (lineno, diffRow ) =>
          ( diffRow.getTag(), lineno, diffRow.getOldLine(), diffRow.getNewLine() )

        type RowTup = Tuple4[DiffRow.Tag, Int, String, String]

        @tailrec
        def replaceLongEqualsSpans( unprocessed : Vector[RowTup], accum : Vector[RowTup | None.type] ) : Vector[RowTup | None.type] =
          unprocessed.headOption.map( _(0) ) match
            case None => accum
            case Some( DiffRow.Tag.EQUAL ) =>
              val equalLength = unprocessed.segmentLength( _(0) == DiffRow.Tag.EQUAL )
              val (eqSpan, rest) = unprocessed.splitAt(equalLength)
              val retainedEqSpan =
                if equalLength <= ShortSpanLimit then
                  eqSpan
                else
                  (eqSpan.slice(0,EqualsEdgeLength) :+ None) ++ eqSpan.drop(eqSpan.size - EqualsEdgeLength)
              replaceLongEqualsSpans(rest, accum++retainedEqSpan)
            case Some( _ ) =>
              val notEqualLength = unprocessed.segmentLength( _(0) != DiffRow.Tag.EQUAL )
              val (neqSpan, rest) = unprocessed.splitAt(notEqualLength)
              replaceLongEqualsSpans(rest, accum++neqSpan)

        val processedRows = replaceLongEqualsSpans(rows.toVector,Vector.empty)
        def normalLine(rt : RowTup) = s"""<tr class="${rt(0)}"><th class="lineno">${rt(1)}</th><td>${rt(2)}</td><td>${rt(3)}</td></tr>"""
        val skipLine = """<tr class="GAP"><th class="lineno">&#8942;</th><td>&#8942;</td><td>&#8942;</td></tr>"""
        val stringRows =
          processedRows.map: pr =>
            pr match
              case None        => skipLine
              case rt : RowTup => normalLine(rt)

        val sb = new StringBuilder( processedRows.size * 2 )
        sb.append("""<table class="diff">""" + LINESEP)
        sb.append(s"""<tr><th>Line</th><th>${revisionBeforeSpec}</th><th>${revisionAfterSpec.getOrElse("current")}</th></tr>""" + LINESEP);
        sb.append( stringRows.mkString("",LINESEP,LINESEP) )
        sb.append("</table>" + LINESEP)
        sb.toString()

        /*
        val rows = immutable.LazyList.from(1).zip(diffRowGenerator.generateDiffRows(beforeLines, afterLines).asScala)
        val sb = new StringBuilder
        sb.append("""<table class="diff">""" + LINESEP)
        sb.append(s"""<tr><th>Line</th><th>${revisionBeforeSpec}</th><th>${revisionAfterSpec.getOrElse("current")}</th></tr>""" + LINESEP);
        var inEqualSpan = false
        rows.foreach: row =>
          val tag = row(1).getTag()
          def normalLine = sb.append(s"""<tr class="${tag}"><th class="lineno">${row(0)}</th><td>${row(1).getOldLine()}</td><td>${row(1).getNewLine()}</td></tr>""" + LINESEP)
          (inEqualSpan, tag.toString() == "EQUAL") match
            case ( false, true ) =>
              sb.append("""<tr class="GAP"><th class="lineno">&#8942;</th><td>&#8942;</td><td>&#8942;</td></tr>""" + LINESEP)
              inEqualSpan = true
            case ( false, false ) =>
              normalLine
            case ( true, true ) =>
              /* skip */
            case ( true, false ) =>
              normalLine
              inEqualSpan = false
        sb.append("</table>" + LINESEP)
        sb.toString()
        */
      def beforeAfterToDiffTable( tup : Tuple2[ZTEndpointBinding.Generable,ZTEndpointBinding.Generable] ) : Either[String,zio.Task[String]] =
        val task =
          for
            beforeBytes <- tup(0).bytesGenerator
            afterBytes <- tup(1).bytesGenerator
          yield
            diffUtf8BytesToTable( beforeBytes, afterBytes )
        Right( task )
      def diffTableEffectToHtml( tableEffect : zio.Task[String] ) : Either[String,zio.Task[String]] =
        val title = s"""Diff of ${origSiteRootedPath}"""
        val out =
          tableEffect.map: table =>
            s"""|<html>
                |  <head>
                |    <meta charset="utf-8">
                |    <title>${title}</title>
                |    <style>
                |      body { font-family: monospace; font-stretch: condensed; }
                |      table { width: 100%; border-spacing: 0; }
                |      th, td { margin: 0; font-size: 10pt; }
                |      td { background-color: #eeeeee; border: 1px solid #cccccc; margin: 0; padding 3;}
                |      tr.EQUAL td { background-color: #cccccc; }
                |      th { background-color: black; color: white; }
                |      tr.GAP td { text-align: center; background-color: #cccccc; color: black; font-weight: bold; font-size: 18pt; }
                |      tr.GAP th { text-align: center; background-color: black; color: white; font-weight: bold; font-size: 18pt; }
                |      span.DELETE { background-color: #ff7777; text-decoration: line-through;}
                |      span.CHANGE { background-color: cyan; }
                |      span.OLD.CHANGE { text-decoration: line-through; }
                |      span.NEW.CHANGE { text-decoration: underline; text-decoration-style: double; }
                |      span.INSERT { background-color: #77ff77; text-decoration: underline; text-decoration-style: double; }
                |    </style>
                |  </head>
                |  <body>
                |    <h1>${title}</h1>
                |    <h2>Revision ${revisionBeforeSpec} => ${revisionAfterSpec.getOrElse("current")}</h2>
                |    <hr>
                |${table}
                |  </body>
                |</html>""".stripMargin
        Right(out)
      val eitherOut =
        for
          tup        <- beforeAfterBindings
          mdEffect   <- beforeAfterToDiffTable(tup)
          htmlEffect <- diffTableEffectToHtml(mdEffect)
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


