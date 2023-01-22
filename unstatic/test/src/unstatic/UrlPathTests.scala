package unstatic

import utest.*

object UrlPathTests extends TestSuite:
  val BaseUrl = UrlPath.Abs("https://www.mchange.com/")
  val HelloThereUnrooted = UrlPath.Unrooted.fromElements("hello", "there")
  val HelloThere = BaseUrl.resolve(HelloThereUnrooted)
  val HelloAgain = HelloThere.resolveSibling(UrlPath.Unrooted("again"))

  val tests = Tests {
    test("UrlPath.Abs.resolve") {
      assert(HelloThere == UrlPath.Abs("https://www.mchange.com/hello/there"))
    }
    test("UrlPath.Abs.relativize") {
      assert(BaseUrl.relativize(HelloThere) == HelloThereUnrooted)
    }
    test("UrlPath.Abs.resolveSibling") {
      assert(HelloAgain == BaseUrl.resolve(UrlPath.Unrooted("hello/again")))
    }
  }
