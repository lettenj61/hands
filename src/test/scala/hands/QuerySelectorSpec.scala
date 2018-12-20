package hands

import hands.Eyes.QueryAttribute

import org.scalatest._

class QuerySelectorSpec extends FunSpec {

  describe("QuerySelector") {
    it("specify element by tag name") {
      val Right(q) = Eyes.parseSelector("table")
      assert(q.tagName == Some("table"))
    }
    it("specify element by id") {
      val Right(q) = Eyes.parseSelector("#come")
      assert(q.id == Some("come"))
    }
    it("specify element by class name") {
      val Right(q) = Eyes.parseSelector(".give.me.love")
      assert(q.classNames == Seq("give", "me", "love"))
    }
    it("specify element by attribute values") {
      // single quoted
      val Right(q1) = Eyes.parseSelector("[name='joe']")
      assert(q1.attributes == Seq(QueryAttribute("name", Some("="), Some("joe"))))

      // double quoted
      val Right(q2) = Eyes.parseSelector("[checked=\"false\"]")
      assert(q2.attributes == Seq(QueryAttribute("checked", Some("="), Some("false"))))

      // multiple attributes
      val Right(q3) = Eyes.parseSelector("[data-v-9018][display^=\"none\"]")
      assert(q3.attributes == Seq(
        QueryAttribute("data-v-9018", None, None),
        QueryAttribute("display", Some("^="), Some("none"))
      ))
    }
  }
}
