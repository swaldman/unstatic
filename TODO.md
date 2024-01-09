# TODO
 - define a giter8 seed that fully sets up a skeletal site
 - it would be nice to have non-media-dir-dependent hash-specials more universally available
 - extension to check attribute keys from entry untemplate?
 - Keep (and commit in type to keeping) identifiers sorted by shortest
 - Support hash-specials like ##post-id#anchor-within-post
 - See what breaks if you make input types less dependent...
 - Figure out multiplexing backend endpoints
   - see e.g. https://github.com/softwaremill/tapir/pull/2724
 - Flag to auto-open a browser window to site root on
   server startup.
 - Better exceptions, search XXX and TODO
 - Add warnings in ZTStaticGen for eg root paths, 
   generableTo links that seem dir like, etc?
 - warn on ZTEndpoint construction with inappropriate _.isDir for siteRootedPath?
   - I don't think this is worth it. consider exceptional cases like dir-endpoints
     served as indexes
 - Speculation: Can we use Mastodon/Fediverse for a comments
   section?
   - Each post a pseudouser people can @ or follow?
   - Maybe each post a hashtag, @ to a common user, 
     comments@interfluidity.com?
   - See https://carlschwan.eu/2020/12/29/adding-comments-to-your-static-blog-with-mastodon/
 - In generating links, consider strint slugs?
   - https://github.com/slugify/slugify/blob/master/src/main/java/com/github/slugify/Slugify.java
   - https://glaforge.dev/posts/2024/01/url-slug-or-how-to-remove-accents-in-java/
   - be sure to
     1. Define totally distinct linkTitle / link generating functions, leaving the original implementations in place
     2. Use new link generating functions in new projects
     3. In old projects, discriminate on untemplate publication date attribute
        - use old form for pre-update datetimes, new form for post-update datetimes
