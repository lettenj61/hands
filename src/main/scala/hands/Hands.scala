package hands

import scala.annotation.{ switch, tailrec }
import scala.collection.mutable
import scala.util.matching.Regex

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

import org.scalajs.dom
import org.scalajs.dom.ext._
import org.scalajs.dom.html

/**
  * Query selector API
  */
@JSExportTopLevel("Eyes")
class Eyes(var rootNode: dom.ParentNode) extends js.Object {
  def this() = this(dom.document)

  /**
    * All elements this watcher aware of.
    * Cache entire collection for better lookup performance
    */
  var nodes: js.Array[dom.Element] = collectNodes()

  private def collectNodes(): js.Array[dom.Element] = {
    val elements = if (rootNode eq dom.document) {
      // support when no `children` in document
      dom.document.body.children
    } else {
      rootNode.children
    }
    elements.foldLeft[js.Array[dom.Element]](js.Array()) {
      case (arr, elem) =>
        arr += elem
        if (elem.children.length > 0) {
          elem.children.foreach(arr.+=)
        }
        arr
    }
  }

  def watch(parentNode: dom.ParentNode): this.type = {
    if (js.isUndefined(parentNode) || (parentNode eq null)) {
      dom.console.warn("unable to watch null element as source, skipping.")
    } else {
      rootNode = parentNode
      nodes = collectNodes()
    }
    this
  }

  def search(selectors: String): js.Array[dom.Element] =
    Eyes.parseSelector(selectors) match {
      case Left(err) => throw err
      case Right(query) =>
        val parents = Eyes.filterElements(nodes, query)
        val subQueries = query.subQueries

        if (subQueries.size == 0) parents
        else {
          val attempts = subQueries.to[mutable.Buffer]
          val found = js.Array[dom.Element]()
          while (attempts.nonEmpty) {
            val attempt = attempts.remove(0)
            for (parent <- parents) {
              found ++= Eyes.filterElements(parent.children.toJSArray, attempt)
            }
          }
          found
        }
    }
}

object Eyes {

  final case class QueryAttribute(
    name: String,
    op: Option[String],
    value: Option[String]
  )

  final case class QuerySelector(
    tagName: Option[String],
    id: Option[String],
    classNames: Seq[String],
    attributes: Seq[QueryAttribute],
    subQueries: Seq[QuerySelector]
  )

  def tagSelector(tagName: String): QuerySelector =
    QuerySelector(Some(tagName), None, Nil, Nil, Nil)

  def classSelector(classNames: Seq[String]): QuerySelector =
    QuerySelector(None, None, classNames, Nil, Nil)

  def idSelector(id: String): QuerySelector =
    QuerySelector(None, Some(id), Nil, Nil, Nil)

  def attrSelector(attributes: Seq[QueryAttribute]): QuerySelector =
    QuerySelector(None, None, Nil, attributes, Nil)

  lazy val AttrSelector: Regex =
    """(\[([\w\-_]+)([~|\^$*]?=)?(["'][^"]+["'])?\])""".r

  def matchesToAttrs(attr: String): Seq[QueryAttribute] = {
    @tailrec def loop(
      curr: String,
      rest: String,
      coll: List[QueryAttribute]
    ): List[QueryAttribute] = {
      if (curr == "") coll
      else {
        val AttrSelector(_, name, op, value) = curr
        val qa = QueryAttribute(
          name,
          Option(op),
          Option(value).map(_.replaceAll("[\"']", ""))
        )
        val (s1, s2) = rest.splitAt(rest.indexOf("]") + 1)
        loop(s1, s2, qa :: coll)
      }
    }
    val (s1, s2) = attr.splitAt(attr.indexOf("]") + 1)
    loop(s1, s2, Nil)
  }

  def splitClassNames(qualified: String): (String, Seq[String]) = {
    val Array(head, classNames @ _*) = qualified.split("\\.")
    (head, classNames.filter(_ != ""))
  }

  def parseNonAttribute(selector: String): QuerySelector = {
    (selector(0): @switch) match {

      case '#' =>
        // ID
        if (selector.indexOf(".") > -1) {
          val (hashedId, classNames) = splitClassNames(selector)
          QuerySelector(None, Some(hashedId.tail), classNames.to[List], Nil, Nil)
        } else {
          idSelector(selector.substring(1))
        }

      case '.' =>
        // CLASS
        classSelector(selector.split("\\.").filter(_ != "").to[List])

      case _ =>
        // TAG NAME
        if (selector.indexOf("#") > -1) {
          // We have ID
          val (tagName, rest) = selector.splitAt(selector.indexOf("#"))
          if (rest.indexOf(".") > -1) {
            val (hashedId, classNames) = splitClassNames(rest)
            QuerySelector(Some(tagName), Some(hashedId.tail), classNames.to[List], Nil, Nil)
          } else {
            QuerySelector(Some(tagName), Some(rest.substring(1)), Nil, Nil, Nil)
          }
        } else if (selector.indexOf(".") > -1) {
          // We have CLASS
          val (tagName, classNames) = splitClassNames(selector)
          QuerySelector(Some(tagName), None, classNames.to[List], Nil, Nil)
        } else {
          tagSelector(selector)
        }

    }
  }

  def parseComponent(selector: String): QuerySelector = {
    AttrSelector.findFirstMatchIn(selector) match {
      case Some(m) =>
        // TODO: optimize
        val attributes =
          matchesToAttrs(m.source.toString.dropWhile(_ != '[')).reverse
        if (m.before == "") attrSelector(attributes)
        else {
          val baseQuery = parseNonAttribute(m.before.toString)
          baseQuery.copy(attributes = attributes)
        }
      case None =>
        parseNonAttribute(selector)
    }
  }

  def parseSelector(selectors: String): Either[js.JavaScriptException, QuerySelector] = {
    val parts = selectors.split("\\s+")
    try {
      val qs = parseComponent(parts(0))
      Right(parts.length match {
        case 0 => qs
        case _ => qs.copy(subQueries = parts.tail.map(parseComponent))
      })
    } catch {
      case ex: Exception =>
        Left(js.JavaScriptException(ex.getMessage))
    }
  }

  def filterElements(
    nodes: js.Array[dom.Element],
    query: QuerySelector
  ): js.Array[dom.Element] = {
    val QuerySelector(tagName, id, classNames, attributes, _) = query
    nodes.filter { node =>
      val elem = node.asInstanceOf[html.Element]
      var pass = true
      // check tag name
      if (tagName.nonEmpty) {
        pass = pass && tagName.exists(_.equalsIgnoreCase(elem.tagName))
      }
      // check id value
      if (id.nonEmpty) {
        pass = pass && id.contains(elem.getAttribute("id"))
      }
      // check class names
      val actualClasses = elem.className.split(" ")
      pass = pass && classNames.forall(actualClasses.contains)
      // check attributes
      pass = pass && attributes.forall {
        case QueryAttribute(name, Some(op), Some(value)) =>
          op match {
            case "="  => elem.getAttribute(name) == value
            case "~=" => elem.getAttribute(name).split(" ").contains(value)
            case "|=" =>
              val attrValue = elem.getAttribute(name)
              attrValue == value || attrValue == (value + "-")
            case "^=" => elem.getAttribute(name).startsWith(value)
            case "$=" => elem.getAttribute(name).endsWith(value)
            case "*=" => elem.getAttribute(name).contains(value)
            case _    => elem.getAttribute(name) ne null
          }
        case QueryAttribute(name, _, _) =>
          elem.getAttribute(name) ne null
        case _ =>
          true
      }
      pass
    }
  }
}

@JSExportTopLevel("Hands")
class Hands extends js.Object {

}
