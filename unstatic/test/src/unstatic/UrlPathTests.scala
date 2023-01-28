package unstatic

import utest.*

import UrlPath.*

object UrlPathTests extends TestSuite:
  val BaseUrl = UrlPath.Abs("https://www.mchange.com/")
  val HelloThereRel = UrlPath.Rel.fromElements("hello", "there")
  val HelloThere = BaseUrl.resolve(HelloThereRel)
  val HelloAgain = HelloThere.resolveSibling(UrlPath.Rel("again"))

  val tests = Tests {
    test("UrlPath.Abs.resolve") {
      assert(HelloThere == UrlPath.Abs("https://www.mchange.com/hello/there"))
    }
    test("UrlPath.Abs.relativize") {
      assert(BaseUrl.relativize(HelloThere) == HelloThereRel)
    }
    test("UrlPath.Abs.resolveSibling") {
      assert(HelloAgain == BaseUrl.resolve(UrlPath.Rel("hello/again")))
    }
    test("UrlPath.Rooted.relativize") {
      assert(Rooted("/2022/01/25/grant-to-usp").relativize(Rooted("/2022/01/25/grant-to-usp/index.html")) == Rel("index.html"))
    }
  }
