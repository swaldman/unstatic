package unstatic.ztapir

import scala.collection.*
import zio.*
import java.nio.file.{Files, StandardCopyOption, Path as JPath}
import unstatic.*
import unstatic.UrlPath.*

private object ZTStaticGen:
  case class Result( generated : immutable.Seq[Rooted], copied : immutable.Seq[Tuple2[Rooted,JPath]], ignored : immutable.Seq[Rooted], ungenerable : immutable.Seq[Rooted])

  private type IgnoredFiles = immutable.Seq[JPath]

  def shouldIgnore( source : JPath, ignorePrefixPaths : immutable.Seq[JPath]) : Boolean = ignorePrefixPaths.exists( ip => source.startsWith(ip) )

  private def overwriteCopyRegularFile( source : JPath, dest : JPath, ignorePrefixPaths : immutable.Seq[JPath] ) : Task[IgnoredFiles] = ZIO.attempt {
    scribe.trace(s"overwriteCopyRegularFile( ${source}, ${dest} )")
    if shouldIgnore(source, ignorePrefixPaths) then
      immutable.Seq(source)
    else
      val destParent = dest.getParent
      Files.createDirectories(destParent)
      Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING)
      immutable.Seq.empty
  }
  private def overwriteCopyDirectory(source : JPath, dest : JPath, ignorePrefixPaths : immutable.Seq[JPath], ignoreRegularFile : JPath => Boolean = _ => false) = ZIO.attempt {
    import scala.jdk.StreamConverters.*

    scribe.trace(s"overwriteCopyDirectory( ${source}, ${dest} )")

    def markProcessableRegFilePath( path : JPath ) : ( JPath, Option[Boolean] ) =
      if Files.isDirectory(path) then
        ( path, Some(false) )
      else if Files.isRegularFile(path) then
        ( path, Some(true) )
      else
        ( path, None )

    val markedSrcPaths = Files.walk(source).toScala(List).map(markProcessableRegFilePath)

    val ignoreForFileType = markedSrcPaths.collect { case (path, None)        => path }
    val srcDirPaths       = markedSrcPaths.collect { case (path, Some(false)) => path }
    val srcRegFilePaths   = markedSrcPaths.collect { case (path, Some(true) ) => path }

    // warn on unignored unhandleable file type
    ignoreForFileType.filter(p => !shouldIgnore(p,ignorePrefixPaths)).foreach { path =>
      scribe.warn(s"Skipping path '${path}', neither directory nor regular file")
    }

    val (ignoreSrcDirPaths, actionableSrcDirPaths)         = srcDirPaths.partition(sdp => shouldIgnore(sdp, ignorePrefixPaths))
    val (ignoreSrcRegFilePaths, actionableSrcRegFilePaths) = srcRegFilePaths.partition(srfp => shouldIgnore(srfp, ignorePrefixPaths) || ignoreRegularFile(srfp))

    val destDirPaths = actionableSrcDirPaths.map(p => dest.resolve(source.relativize(p)))
    destDirPaths.foreach(p => Files.createDirectories(p))

    val srcDestRegFilePathTups = actionableSrcRegFilePaths.map(p => (p, dest.resolve(source.relativize(p))))
    srcDestRegFilePathTups.foreach { case(s,d) => Files.copy(s,d,StandardCopyOption.REPLACE_EXISTING)}

    (ignoreForFileType ++ ignoreSrcDirPaths ++ ignoreSrcRegFilePaths).toSeq
  }
  private def ensureExists( path : JPath ) = ZIO.attempt {
    if !Files.exists(path) then Files.createDirectories(path) else ()
  }
  private def verifyCheckIsDirVsRegularFile(path : JPath, expectsFile : Boolean) = ZIO.attempt {
    val out = Files.isDirectory(path)
    if (!out && !Files.isRegularFile(path)) then
      throw new UnexpectedStaticLocationType(s"Expected to generate directory or regular file, '${path}' is neither.")
    else if (out && expectsFile)
      scribe.warn(
        s"Expected to generate a single file for '${path}', but found a directory to generate instead." +
        "Static generation should succeed, but HTTP generation may fail."
      )
    else if (!out && !expectsFile)
      scribe.warn (
        s"Expected to generate a directory for '${path}', but found a file to generate instead." +
        "Static generation should succeed, but HTTP generation may fail."
      )
    out
  }

  def generate(
    endpointBindings        : immutable.Seq[ZTEndpointBinding],
    genSiteRootDir          : JPath,
    ignorePrefixes          : immutable.Seq[Rooted] = Nil
  ) : Task[Result] =
    scribe.trace("generate(...)")

    if ignorePrefixes.contains(Rooted.root) then
      throw new NotYetSupported("ignorePrefixes cannot (yet?) contain the root prefix, which would ignore all.")

    val (ignoredEndpointBindings, unignoredEndpointBindings)
      = endpointBindings.partition( epb => ignorePrefixes.exists(pfx => pfx.isPrefixOf(epb.siteRootedPath)) )

    // XXX: Should we warn on location bindings that might contain ignored prefixes?
    //      or take care not to copy ignored directories?
    val (locationBindings, nonlocationBindings) =
      unignoredEndpointBindings.foldLeft( Tuple2(Vector.empty[ZTEndpointBinding.FromFileSystem], Vector.empty[ZTEndpointBinding]) ){ (accum, next) =>
        next match
          case fsd : ZTEndpointBinding.FromFileSystem => ( accum(0) :+ fsd, accum(1) )
          case not                                    => ( accum(0), accum(1) :+ not )
      }

    val (ungenerableEndpointBindings, generableEndpointBindings) =
      nonlocationBindings.foldLeft( Tuple2(Vector.empty[ZTEndpointBinding], Vector.empty[ZTEndpointBinding.Generable]) ){ (accum, next) =>
        next match
          case gen : ZTEndpointBinding.Generable => ( accum(0), accum(1) :+ gen )
          case not                               => ( accum(0) :+ not, accum(1) )
      }

    // println(s"unignoredLocationBindings: " + unignoredLocationBindings.mkString(", "))

    val noExceptionNoIgnoredFilesResult =
      val generated             = generableEndpointBindings.map( _.siteRootedPath )
      val copied                = locationBindings.map(lb => (lb.siteRootedPath,lb.source))
      val ignoredForGeneration  = ignoredEndpointBindings.map( _.siteRootedPath )
      val ungenerable           = ungenerableEndpointBindings.map( _.siteRootedPath )
      Result( generated, copied, ignoredForGeneration, ungenerable )

    def findDestPathFor(siteRootedPath: Rooted) = ZIO.attempt {
      if siteRootedPath == Rooted.root then
        genSiteRootDir
      else
        val elements = siteRootedPath.elements
        genSiteRootDir.resolve(JPath.of(elements.head, elements.tail: _*))
    }
    def generateStaticLocation( siteRootedPath : Rooted, source : JPath, ignorePrefixPaths : immutable.Seq[JPath], expectsFile : Boolean ) : Task[IgnoredFiles] =
      for
        destPath     <- findDestPathFor(siteRootedPath)
        _            <- ensureExists(source)
        sourceIsDir  <- verifyCheckIsDirVsRegularFile(source, expectsFile)
        ignoredFiles <- if sourceIsDir then overwriteCopyDirectory( source, destPath, ignorePrefixPaths ) else overwriteCopyRegularFile( source, destPath, ignorePrefixPaths )
      yield ignoredFiles

    def writeStringFor( siteRootedPath : Rooted, contents : String, codec : scala.io.Codec = scala.io.Codec.UTF8 ) : Task[Unit] =
      for
        destParent <- findDestPathFor(siteRootedPath.parent) // will throw if root
        destPath   <- findDestPathFor(siteRootedPath)//.debug("dest path")
        _          <- ZIO.attempt( if !Files.exists(destParent) then Files.createDirectories(destParent) )
        _          <- ZIO.attempt( Files.writeString(destPath, contents, codec.charSet) )
      yield()

    def writeBytesFor( siteRootedPath : Rooted, contents : immutable.ArraySeq[Byte] ) : Task[Unit] =
      for
        destParent <- findDestPathFor(siteRootedPath.parent) // will throw if root
        destPath   <- findDestPathFor(siteRootedPath)//.debug("dest path")
        _          <- ZIO.attempt( if !Files.exists(destParent) then Files.createDirectories(destParent) )
        _          <- ZIO.attempt( Files.write(destPath, contents.toArray) )
      yield()

    // note that we reverse, so that if there are conficts, early location bindings will overwrite late ones
    val locationsTask =
      val ignorePrefixPaths = ignorePrefixes.map { ip =>
        JPath.of( ip.elements.head, ip.elements.tail : _* ) // for now, we don't accept root as an ignore prefix
      }
      val tasks = locationBindings.reverse.map { lb =>
        val expectsFile =
          lb match
            case _ : ZTEndpointBinding.FromStaticFile       => true
            case _ : ZTEndpointBinding.FromStaticDirectory  => false
        generateStaticLocation(lb.siteRootedPath, lb.source, ignorePrefixPaths, expectsFile)
      }
      ZIO.mergeAll(tasks)(immutable.Seq.empty)( _ ++ _ )

    val generablesTask =
      val tasks = generableEndpointBindings.map { generable =>
        for
          contents <- generable.bytesGenerator
          _ <- writeBytesFor(generable.siteRootedPath, contents)
        yield ()
      }
      ZIO.foreachDiscard(tasks)(identity)

    for
      ignoredPaths <- locationsTask
      _            <- generablesTask
    yield
      val newIgnored = ignoredPaths.map(jp => Rooted.parseAndRoot(jp.toString()))
      noExceptionNoIgnoredFilesResult.copy(ignored = noExceptionNoIgnoredFilesResult.ignored ++ newIgnored)

  def generateZTSite( site : ZTSite, genSiteRootDir: JPath, ignorePrefixes: immutable.Seq[Rooted] = Nil) : Task[Result] =
    scribe.trace("generateZTSite(...)")
    generate( site.endpointBindings, genSiteRootDir, ignorePrefixes )

