# TODO

 - warn or fail if no file exists @ hash-special media-dir refs
 - Figure out multiplexing backend endpoints
   - see e.g. https://github.com/softwaremill/tapir/pull/2724
 - Set up to easily include dashified untemplate fqn/identifier as CSS class
   - Maybe always keep identifiers sorted by shortest?
   - Supply identifiers as inputs?
   - Or maybe just have users use <( attrs("Anchor") )>
 - Flag to auto-open a browser window to site root on
   server startup.
 - Main executable: Be sure to print failure information.
   - Right now, we exit silently (albeit with a failure 
     exit code) when stuff happens
 - Better exceptions, search XXX and TODO
 - Add warnings in ZTStaticGen for eg root paths, 
   generableTo links that seem dir like, etc?
 - Speculation: Can we use Mastodon/Fediverse for a comments
   section?
   - Each post a pseudouser people can @ or follow?
   - Maybe each post a hashtag, @ to a common user, 
     comments@interfluidity.com?
   - See https://carlschwan.eu/2020/12/29/adding-comments-to-your-static-blog-with-mastodon/
