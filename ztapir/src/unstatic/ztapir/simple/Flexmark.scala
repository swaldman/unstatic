package unstatic.ztapir.simple

import com.vladsch.flexmark.util.ast.Node
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.data.MutableDataSet
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.footnotes.FootnoteExtension
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension

import java.util.Arrays

object Flexmark:
  def defaultMarkdownToHtml( markdownText : String, mbGeneratorUniqueId: Option[String] ) : String =
    // we would like the id/name/hrefs to be guaranteed unique, so we'd like
    // to prepend fully-qualified-generator-names as unique IDs to those attributes in
    // footnote and headings.
    //
    // unfortunately, the settings we thought would do that... don't
    // so, for now, the whole exercise is commented out

    //def d2d( s : String ) = s.map( c => if c == '.' then '-' else c )
    //val idPrefix = options.generatorFullyQualifiedName.map(d2d).map( _ + "-").getOrElse("")

    val flexmarkSettings = new MutableDataSet()
      .set(HtmlRenderer.GENERATE_HEADER_ID, true)
      .set(AnchorLinkExtension.ANCHORLINKS_SET_NAME, true	)
      .set(AnchorLinkExtension.ANCHORLINKS_ANCHOR_CLASS, "anchorlink"	)
      .set(AnchorLinkExtension.ANCHORLINKS_WRAP_TEXT, false) // to prevent headers from being styled as links
      //.set(AnchorLinkExtension.ANCHORLINKS_TEXT_PREFIX, idPrefix) // not what we want, alas
      //.set(FootnoteExtension.FOOTNOTE_REF_PREFIX, idPrefix) //not what we want, alas, alas!
      .set(TablesExtension.CLASS_NAME, "flexmark-table")
      .set(TablesExtension.COLUMN_SPANS, false) // table extension options for full GFM table compatibility
      .set(TablesExtension.APPEND_MISSING_COLUMNS, true)
      .set(TablesExtension.DISCARD_EXTRA_COLUMNS, true)
      .set(TablesExtension.HEADER_SEPARATOR_COLUMN_MATCH, true)
      .set(
        Parser.EXTENSIONS,
        Arrays.asList(
          AnchorLinkExtension.create(),
          FootnoteExtension.create(),
          StrikethroughExtension.create(),
          TablesExtension.create(),
        )
      )
      .toImmutable

    val parser = Parser.builder(flexmarkSettings).build();
    val renderer = HtmlRenderer.builder(flexmarkSettings).build();

    // You can re-use parser and renderer instances
    val document = parser.parse(markdownText);
    s"""|<div class="flexmark markdown">
        |${renderer.render(document)}
        |</div>""".stripMargin






