package unstatic.ztapir.simple

import unstatic.*, ztapir.*, UrlPath.*

import unstatic.ztapir.ZTEndpointBinding

import scala.collection.immutable
import java.nio.file.Path as JPath

object RevisionBinder:
  type RevisionPathFinder = (Rooted, String) => Rooted // yields a site-rooted path
  object GitByCommit:
    val DefaultRevisionPathFinder : RevisionPathFinder  = (origPath : Rooted, revisionSpec : String) =>
      require( !origPath.isRoot && !origPath.isDir, s"'$origPath' represents a directory or root, we only convert individual leaf resources to prior revisions." )
      val origFilename = origPath.elements.last
      val lastDot = origFilename.lastIndexOf('.')
      val newFilename =
        if lastDot == 0 then
          throw new IllegalArgumentException("'$origFilename' appears to be a dotfile. We do not retrieve or convert dotfiles.")
        else if lastDot > 0 then
          origFilename.substring(0,lastDot) + "-oldcommit-" + revisionSpec + origFilename.substring(lastDot)
        else
          origFilename + "-oldcommit-" + revisionSpec
      origPath.resolveSibling(newFilename)
  class GitByCommit( site : ZTSite, repoDir : JPath, origSiteRootedPathToInRepoPath : Rooted => Rel, val revisionPathFinder : RevisionPathFinder = GitByCommit.DefaultRevisionPathFinder ) extends RevisionBinder:
    def revisionEndpointBinding( revisionSpec : String, origSiteRootedPath : Rooted, mimeType : String, identifiers : immutable.Set[String] ) : ZTEndpointBinding =
      val revisionSiteRootedPath = revisionPathFinder(origSiteRootedPath, revisionSpec)
      val inRepoPath = origSiteRootedPathToInRepoPath(origSiteRootedPath)
      ZTEndpointBinding.fromGitRepoCommitPath(revisionSiteRootedPath, site, repoDir, revisionSpec, inRepoPath, mimeType, identifiers)

trait RevisionBinder:
  def revisionPathFinder : RevisionBinder.RevisionPathFinder
  def revisionEndpointBinding( revisionSpec : String, origSiteRootedPath : Rooted, mimeType : String, identifiers : immutable.Set[String] ) : ZTEndpointBinding

