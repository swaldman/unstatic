# TODO
 - it would be nice to have non-media-dir-dependent hash-specials more universally available
 - extension to check attribute keys from entry untemplate?
 - Keep (and commit in type to keeping) identifiers sorted by shortest
 - See what breaks if you make input types less dependent...
 - fqn-utils to simply filters, something on which you can define
   "withPathElement" and "startsWith", etc.
 - get rid of parallel, reduntant nonZT location bindings.
   - For ZT apps, just use ZTEndpoits 
 - warn or fail if no file exists @ hash-special media-dir refs
 - Figure out multiplexing backend endpoints
   - see e.g. https://github.com/softwaremill/tapir/pull/2724
 - Set up to easily include dashified untemplate fqn/identifier as CSS class
   - Maybe always keep identifiers sorted by shortest?
   - Supply identifiers as inputs?
   - Or maybe just have users use <( attrs("Anchor") )>
 - Flag to auto-open a browser window to site root on
   server startup.
 - Better exceptions, search XXX and TODO
 - Add warnings in ZTStaticGen for eg root paths, 
   generableTo links that seem dir like, etc?
 - Speculation: Can we use Mastodon/Fediverse for a comments
   section?
   - Each post a pseudouser people can @ or follow?
   - Maybe each post a hashtag, @ to a common user, 
     comments@interfluidity.com?
   - See https://carlschwan.eu/2020/12/29/adding-comments-to-your-static-blog-with-mastodon/
