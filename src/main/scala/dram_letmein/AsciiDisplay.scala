package dram_letmein

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object AsciiDisplay {
  /**
    * Formats the given tree as lines of `String`s
    *
    * @param root The root node of the tree
    * @return The tree, but formatted as lines
    */
  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    /**
      * The main worker method
      *
      * @param lastChild Is `cur` the last child of its parent?
      * @param curIndent How should this node be indented?
      * @param curIndentEmpty How should blank lines be indented? (They shouldn't contain trailing spaces)
      * @param cur The node we're working on
      * @return The subtree, but formatted as lines
      */
    def worker(lastChild: Boolean, curIndent: String, curIndentEmpty: String)(cur: TreeNode[String]): Seq[String] = {
      val nextIndent = if (lastChild) s"$curIndent  " else s"$curIndent| "
      val nextIndentEmpty = if (lastChild) curIndentEmpty else s"$curIndent|"
      val dataFormat = s"$curIndent+-${cur.data}"
      val childrenFormat =
        if (cur.children.isEmpty) Seq()
        else {
          val initFormat =
            cur.children.init.flatMap(worker(lastChild = false, nextIndent, nextIndentEmpty))
          val lastFormat =
            worker(lastChild = true, nextIndent, nextIndentEmpty)(cur.children.last)
          initFormat ++ lastFormat
        }
      val ending = if (! lastChild && cur.children.nonEmpty) Seq(nextIndentEmpty) else Seq()
      Seq(dataFormat) ++ childrenFormat ++ ending
    }
    worker(lastChild = true, "", "")(root)
  }
}
