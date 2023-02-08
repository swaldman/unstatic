package unstatic.ztapir

import scala.collection.*
import zio.*
import java.nio.file.{Files, StandardCopyOption, Path as JPath}
import unstatic.*
import unstatic.UrlPath.*

private object ZTStaticGen:
  case class Result( generated : immutable.Seq[Rooted], copied : immutable.Seq[StaticLocationBinding], ignored : immutable.Seq[Rooted], ungenerable : immutable.Seq[Rooted])

  private def overwriteCopyRegularFile( source : JPath, dest : JPath ) = ZIO.attempt {
    // println(s"overwriteCopyRegularFile( ${source}, ${dest} )")
    val destParent = dest.getParent
    Files.createDirectories(destParent)
    Files.copy(source, dest, StandardCopyOption.REPLACE_EXISTING)
  }
  private def overwriteCopyDirectory(source : JPath, dest : JPath, ignoreRegularFile : JPath => Boolean = _ => false) = ZIO.attempt {
    import scala.jdk.StreamConverters.*

    // println(s"overwriteCopyDirectory( ${source}, ${dest} )")
    val srcPaths = Files.walk(source).toScala(List).filter(p => !Files.isDirectory(p) && !ignoreRegularFile(p))
    val destPaths = srcPaths.map(p => dest.resolve(source.relativize(p)))
    srcPaths.zip(destPaths).foreach { case (srcPath, destPath) =>
      if (Files.isDirectory(srcPath)) then Files.createDirectories(destPath)
      else
        Files.createDirectories(destPath.getParent)
        Files.copy(srcPath, destPath, StandardCopyOption.REPLACE_EXISTING)
    }
  }
  private def checkIsDir(path : JPath) = ZIO.attempt( Files.isDirectory(path) )

  def generate(
    endpointBindings        : immutable.Seq[ZTEndpointBinding],
    staticLocationBindings  : immutable.Seq[StaticLocationBinding],
    genSiteRootDir          : JPath,
    ignorePrefixes          : immutable.Seq[Rooted] = Nil
  ) : Task[Result] =
    val (ignoredEndpointBindings, unignoredEndpointBindings)
      = endpointBindings.partition( epb => ignorePrefixes.exists(pfx => pfx.isPrefixOf(epb.siteRootedPath)) )

    // we don't ignore any static location bindings
    val unignoredLocationBindings = staticLocationBindings


    val (ungenerableEndpointBindings, generableEndpointBindings) =
      unignoredEndpointBindings.foldLeft( Tuple2(Vector.empty[ZTEndpointBinding], Vector.empty[ZTEndpointBinding.Generable]) ){ (accum, next) =>
        next match
          case gen : ZTEndpointBinding.Generable => ( accum(0), accum(1) :+ gen )
          case not                               => ( accum(0) :+ not, accum(1) )
      }

    // println(s"unignoredLocationBindings: " + unignoredLocationBindings.mkString(", "))

    val noExceptionResult =
      val generated = generableEndpointBindings.map( _.siteRootedPath )
      val copied = unignoredLocationBindings
      val copiedSiteRootedPaths = unignoredLocationBindings.map(_.siteRootedPath).toSet
      val ignoredForGeneration = ignoredEndpointBindings.map( _.siteRootedPath )
      val ungenerable =
        def generatedFromLocation( zteb : ZTEndpointBinding ) =
          zteb match
            case fsd : ZTEndpointBinding.FromStaticDirectory if copiedSiteRootedPaths(fsd.siteRootedPath) => true
            case _ => false
        ungenerableEndpointBindings.filterNot(generatedFromLocation).map( _.siteRootedPath )
      Result( generated, copied, ignoredForGeneration, ungenerable )

    def findDestPathFor(siteRootedPath: Rooted) = ZIO.attempt {
      if siteRootedPath == Rooted.root then
        genSiteRootDir
      else
        val elements = siteRootedPath.elements
        genSiteRootDir.resolve(JPath.of(elements.head, elements.tail: _*))
    }
    def generateLocation( siteRootedPath : Rooted, source : JPath ) : Task[Unit] =
      for
        destPath    <- findDestPathFor(siteRootedPath)//.debug("destPath")
        sourceIsDir <- checkIsDir(source)
        _           <- if sourceIsDir then overwriteCopyDirectory( source, destPath ) else overwriteCopyRegularFile( source, destPath )
      yield ()

    def writeStringFor( siteRootedPath : Rooted, contents : String, codec : scala.io.Codec = scala.io.Codec.UTF8 ) : Task[Unit] =
      for
        destParent <- findDestPathFor(siteRootedPath.parent) // will throw if root
        destPath   <- findDestPathFor(siteRootedPath)//.debug("dest path")
        _          <- ZIO.attempt( if !Files.exists(destParent) then Files.createDirectories(destParent) )
        _          <- ZIO.attempt( Files.writeString(destPath, contents, codec.charSet) )
      yield()

    def writeBytesFor( siteRootedPath : Rooted, contents : immutable.Seq[Byte] ) : Task[Unit] =
      for
        destParent <- findDestPathFor(siteRootedPath.parent) // will throw if root
        destPath   <- findDestPathFor(siteRootedPath)//.debug("dest path")
        _          <- ZIO.attempt( if !Files.exists(destParent) then Files.createDirectories(destParent) )
        _          <- ZIO.attempt( Files.write(destPath, contents.toArray) )
      yield()

    // note that we reverse, so that if there are conficts, early location bindings will overwrite late ones
    val locationTasks = unignoredLocationBindings.reverse.map( slb => generateLocation(slb.siteRootedPath, slb.source) )

    // println(s"locationTasks: " + locationTasks.mkString(", "))

    val endpointTasks = generableEndpointBindings.map { generable =>
          for
            contents <- generable.bytesGenerator
            _ <- writeBytesFor(generable.siteRootedPath, contents)
          yield ()
    }
    ZIO.foreachDiscard(locationTasks ++ endpointTasks)(identity).map( _ => noExceptionResult )

  def generateZTSite( site : ZTSite, genSiteRootDir: JPath, ignorePrefixes: immutable.Seq[Rooted] = Nil) : Task[Result] =
    generate( site.endpointBindings, site.locationBindings, genSiteRootDir, ignorePrefixes )

