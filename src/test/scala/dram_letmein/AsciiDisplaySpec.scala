package dram_letmein

import org.scalatest.FunSpec

import AsciiDisplay._

class AsciiDisplaySpec extends FunSpec {
  describe("asciiDisplay") {
    // Redundant? Maybe
    // In a sense, these two 'examples' are not 'real' tests.
    // They only serve to check that the two examples given gets processed right.
    it("should pass example 1") {
      assert(asciiDisplay(TreeNode("Root",
        children = List(TreeNode("level1-1"),
          TreeNode("level1-2"),
          TreeNode("level1-3"))))
        === Seq(
        "+-Root",
        "  +-level1-1",
        "  +-level1-2",
        "  +-level1-3"))
    }

    it("should pass example 2") {
      assert(asciiDisplay(TreeNode("Root",
        children = List(
          TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
          TreeNode("level1-2"),
          TreeNode("level1-3"))))
        === Seq(
        "+-Root",
        "  +-level1-1",
        "  | +-level2-1",
        "  |   +-level3-1",
        "  |",
        "  +-level1-2",
        "  +-level1-3"))
    }

    it("should handle trees with root only") {
      assert(asciiDisplay(TreeNode("Root")) === Seq("+-Root"))
    }

    it("should handle basic trees") {
      assert(asciiDisplay(
        TreeNode("Root", Seq(
          TreeNode("a"),
          TreeNode("b"),
          TreeNode("c"))))
        === Seq(
        "+-Root",
        "  +-a",
        "  +-b",
        "  +-c"))
    }

    it("should insert blank lines for non-last-child complex subtrees") {
      assert(asciiDisplay(
        TreeNode("Root", Seq(
          TreeNode("a", Seq(
            TreeNode("b"))),
          TreeNode("c"))))
        === Seq(
        "+-Root",
        "  +-a",
        "  | +-b",
        "  |",
        "  +-c"))

      assert(asciiDisplay(
        TreeNode("Root", Seq(
          TreeNode("a", Seq(
            TreeNode("b"))),
          TreeNode("c", Seq(
            TreeNode("d"))),
          TreeNode("e"),
          TreeNode("f"))))
        === Seq(
        "+-Root",
        "  +-a",
        "  | +-b",
        "  |",
        "  +-c",
        "  | +-d",
        "  |",
        "  +-e",
        "  +-f"))
    }

    it("should not insert blank lines for last child") {
      assert(asciiDisplay(
        TreeNode("Root",Seq(
          TreeNode("a"),
          TreeNode("b"),
          TreeNode("c"),
          TreeNode("d",Seq(
            TreeNode("e", Seq(
              TreeNode("f"))))))))
        === Seq(
        "+-Root",
        "  +-a",
        "  +-b",
        "  +-c",
        "  +-d",
        "    +-e",
        "      +-f"))
    }

    it("should insert blank lines for nested complex subtrees") {
      assert(asciiDisplay(
        TreeNode("Root", Seq(
          TreeNode("a", Seq(
            TreeNode("b", Seq(
              TreeNode("c", Seq(
                TreeNode("d", Seq(
                  TreeNode("e"))))),
              TreeNode("f"))))),
          TreeNode("g"))))
        === List(
        "+-Root",
        "  +-a",
        "  | +-b",
        "  |   +-c",
        "  |   | +-d",
        "  |   |   +-e",
        "  |   |",
        "  |   +-f",
        "  |",
        "  +-g"))
    }
  }
}