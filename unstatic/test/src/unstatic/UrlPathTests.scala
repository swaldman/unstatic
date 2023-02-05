package unstatic

import utest.*

import UrlPath.*

object UrlPathTests extends TestSuite:
  val BaseUrl = UrlPath.Abs("https://www.mchange.com/")
  val HelloThereRel = UrlPath.Rel.fromElements("hello", "there")
  val HelloThere = BaseUrl.resolve(HelloThereRel)
  val HelloAgain = HelloThere.resolveSibling(UrlPath.Rel("again"))

  def doesThrow[T <: Throwable]( exceptionGeneratingExpression: => Any) =
    try { exceptionGeneratingExpression; false } catch { case e : T => true; case _ => false }

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
    test("UrlPath.Rooted.canonical") {
      test - assert(Rooted("/hello/../there").canonical == Rooted("/there"))
      test - assert(Rel("hello/../../there").canonical == Rel("../there"))
      test - assert(Rel("./hello/.././../there/.").canonical == Rel("../there"))
      test - assert( doesThrow[BadPath]( Rooted("/hello/.././../there/.") ) )
      test - assert(Rooted("/hello/..").canonical == Rooted.root)
    }
    test("UrlPath.parentOption") {
      test - assert(Rooted("/hello/there").parentOption == Some(Rooted("/hello")))
      test - assert(Rooted.root.parentOption == None)
      test - assert(Rel.here.parentOption == Some(Rel("..")))
    }
    test("UrlPath.parent") {
      test - assert( doesThrow[BadPath](Rooted.root.parent) )
      test - assert( doesThrow[BadPath](Rooted("/hello").parent.parent) )
    }
  }
