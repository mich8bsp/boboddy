import MuPuzzle.{Rule1, Rule2, Rule3, Rule4}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class MuPuzzleTest extends AnyFlatSpecLike with Matchers {

  "rule 1" should "add U if ends with I" in {
    Rule1("MU") shouldBe Set.empty[String]
    Rule1("M") shouldBe Set.empty[String]
    Rule1("MI") shouldBe Set("MIU")
  }

  "rule 2" should "change Mx into Mxx" in {
    Rule2("UM") shouldBe Set.empty[String]
    Rule2("MIU") shouldBe Set("MIUIU")
    Rule2("MUM") shouldBe Set("MUMUM")
    Rule2("MU") shouldBe Set("MUU")
    Rule2("MM") shouldBe Set("MMM")
  }

  "rule 3" should "replace III with U" in {
    Rule3("MIIU") shouldBe Set.empty[String]
    Rule3("MIIIU") shouldBe Set("MUU")
    Rule3("IIIM") shouldBe Set("UM")
    Rule3("MIII") shouldBe Set("MU")
    Rule3("MUIIIMUMIII") shouldBe Set("MUUMUMIII", "MUIIIMUMU")
  }

  "rule 4" should "drop UU" in {
    Rule4("MIIU") shouldBe Set.empty[String]
    Rule4("MUUI") shouldBe Set("MI")
    Rule4("MUU") shouldBe Set("M")
    Rule4("UUM") shouldBe Set("M")
    Rule4("UU") shouldBe Set("")
    Rule4("MIUUMMUUI") shouldBe Set("MIMMUUI", "MIUUMMI")
  }
}
