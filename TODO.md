# TODO
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
