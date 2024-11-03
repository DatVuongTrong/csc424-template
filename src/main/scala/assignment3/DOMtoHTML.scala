package assignment3

import java.io.File

type ErrOr[T] = Either[String, T]

object DOMtoHTML:
  def apply(input: String): ErrOr[HTML] = {
    for
      dom <- Parser(input)
      html <- parseHTML(dom)
    yield
      html
  }

  def apply(inFile: File): ErrOr[HTML] = {
    for
      dom <- Parser(inFile)
      html <- parseHTML(dom)
    yield
      html
  }

  def parseHTML(dom: DOM): ErrOr[HTML] = {
    dom match
      case Element("html", _, Seq(h, b)) =>
        for
          head <- parseHead(h)
          body <- parseBody(b)
        yield
          HTML(head, body)
      case _ =>
        Left("Expected <html> with two children")
  }

  def parseHead(dom: DOM): ErrOr[Head] = {
    dom match
      case Element("head", _, Seq(t)) =>
        for
          title <- parseTitle(t)
        yield
          Head(title)
      case _ =>
        Left("Expected <head> with one child")
  }

  def parseTitle(dom: DOM): ErrOr[Title] = {
    dom match
      case Element("title", _, Seq(Text(s))) =>
        Right(Title(s))
      case _ =>
        Left("Expected <title> element")
  }

  def parseBody(dom: DOM): ErrOr[Body] = {
    dom match
      case Element("body", _, children) =>
        for
          content <- collect(children, parseGrouping)
        yield
          Body(content)
      case _ =>
        Left("Expected <body> element")
  }

  def parseGrouping(dom: DOM): ErrOr[GroupingContent] = {
    import GroupingContent.*

    dom match
      case Element("h1", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H1(content)
      case Element("h2", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H2(content)
      case Element("h3", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H3(content)
      case Element("p", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          P(content)
      case Element("hr", _, Seq()) =>
        Right(HR)
      case Element("ol", _, children) =>
        for
          items <- collect(children, parseItem)
        yield
          OL(items)
      case Element("ul", _, children) =>
        for
          items <- collect(children, parseItem)
        yield
          UL(items)
      case _ =>
        Left("Expected grouping element")
  }

  def parseItem(dom: DOM): ErrOr[Item] = {
    dom match
      case Element("li", _, children) =>
        for
          content <- collect(children, parsePhrasing) // TODO allow grouping?
        yield
          Item(content)
      case _ =>
        Left("Expected <li>")
  }

  def parsePhrasing(dom: DOM): ErrOr[PhrasingContent] = {
    import PhrasingContent.*

    dom match
      case Element("em", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          Em(content)
      case Element("strong", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          Strong(content)
      case Element("a", attributes, children) =>
        for
          href <- extractRequired(attributes, "href")
          content <- collect(children, parsePhrasing)
        yield
          A(href, content)
      case Text(content) =>
        Right(Txt(content))
      case _ =>
        Left("Expected phrasing element")
  }

  def collect[T](children: Seq[DOM], parser: DOM => ErrOr[T]): ErrOr[Seq[T]] = {
    children.foldLeft(Right(Seq()): ErrOr[Seq[T]]) {
      case (accum, child) =>
        for
          xs <- accum
          x <- parser(child)
        yield
          xs :+ x
    }
  }

  def extractRequired(attributes: Seq[Attribute], name: String): ErrOr[String] = {
    extractOptional(attributes, name).toRight(s"Expected $name")
  }

  def extractOptional(attributes: Seq[Attribute], name: String): Option[String] = {
    attributes.find(_.name == name).map(_.value)
  }

  def convertToMarkdown(html: HTML): Seq[String] = {
    val result = generateHeadMarkdown(html.head) ++ generateBodyMarkdown(html.body)
    result
  }

  private def generateHeadMarkdown(head: Head): Seq[String] = {
    Seq(
      "---",
      s"title: ${head.title.title}",
      "---",
      ""
    )
  }

  private def generateBodyMarkdown(body: Body): Seq[String] = {
    body.content.flatMap(convertGroupingContentToMarkdown)
  }

  private def convertGroupingContentToMarkdown(content: GroupingContent): Seq[String] = content match {
    case GroupingContent.H1(children) => Seq("# " + convertInlineElementsToMarkdown(children), "")
    case GroupingContent.H2(children) => Seq("## " + convertInlineElementsToMarkdown(children), "")
    case GroupingContent.H3(children) => Seq("### " + convertInlineElementsToMarkdown(children), "")
    case GroupingContent.P(children) => Seq(convertInlineElementsToMarkdown(children), "")
    case GroupingContent.OL(items) => generateListMarkdown(items, ordered = true)
    case GroupingContent.UL(items) => generateListMarkdown(items, ordered = false)
    case GroupingContent.HR => Seq("---", "")
  }

  private def generateListMarkdown(items: Seq[Item], ordered: Boolean): Seq[String] = {
    val listItems = items.zipWithIndex.map { case (item, idx) =>
      val prefix = if (ordered) s"${idx + 1}. " else "- "
      val content = convertInlineElementsToMarkdown(item.children)
      prefix + content
    } 
    listItems :+ ""
  }

  private def convertInlineElementsToMarkdown(elements: Seq[PhrasingContent]): String = {
    elements.map(convertPhrasingContentToMarkdown).mkString("")
  }

  private def convertPhrasingContentToMarkdown(content: PhrasingContent): String = content match {
    case PhrasingContent.Em(text) => s"*${convertInlineElementsToMarkdown(text)}*"
    case PhrasingContent.Strong(text) => s"**${convertInlineElementsToMarkdown(text)}**"
    case PhrasingContent.A(url, linkText) => s"[${convertInlineElementsToMarkdown(linkText)}](${url})"
    case PhrasingContent.Txt(text) => text
  }

@main def replHTMLTest(): Unit = {
  import scala.io.StdIn.readLine
  import scala.annotation.tailrec

  @tailrec
  def loop: Unit = {
    print("> ")
    val input = readLine()

    if input == null || input == "exit" then
      println("Goodbye")
    else if input == "" then
      loop
    else
      DOMtoHTML(input) match
        case Right(html) =>
          val markdown=DOMtoHTML.convertToMarkdown(html);
          println(markdown.mkString("\n"))
        case Left(message) =>
          println("ErrOr: " + message)
      loop
  }

  loop
}

@main def fileHTMLTest(): Unit = {
  val inFile = new File("src/main/scala/assignment3/demo.html")
  
  DOMtoHTML(inFile) match
        case Right(html) =>
          val markdown=DOMtoHTML.convertToMarkdown(html);
          println(markdown.mkString("\n"))
        case Left(message) =>
          println("ErrOr: " + message)
}
