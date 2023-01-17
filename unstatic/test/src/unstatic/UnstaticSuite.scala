package unstatic

import org.scalatest.funsuite.AnyFunSuite

class UnstaticSuite extends AnyFunSuite:
  test("'http://www.poop.com:80/looky/' should relativize 'http://www.poop.com:80/looky/looky/here' to 'looky/here'") {
    assert(AbsPath("http://www.poop.com:80/looky/").relativize(AbsPath("http://www.poop.com:80/looky/looky/here")) == RelPath("looky/here"))
  }
  test("'http://www.poop.com:80/looky/looky/here' should relativize 'http://www.poop.com:80/looky/here' to '../here'") {
    assert(AbsPath("http://www.poop.com:80/looky/looky/here").relativize(AbsPath("http://www.poop.com:80/looky/here")) == RelPath("../here"))
  }
  test("'http://www.poop.com:80/looky/looky/here' should relativize 'http://www.poop.com:80/looky/' to '../'") {
    assert(AbsPath("http://www.poop.com:80/looky/looky/here").relativize(AbsPath("http://www.poop.com:80/looky/")) == RelPath("../"))
  }



