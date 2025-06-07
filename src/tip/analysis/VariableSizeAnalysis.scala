package tip.analysis

import tip.cfg._
import tip.lattices.IntervalLattice._
import tip.lattices._

trait VariableSizeAnalysis extends ValueAnalysisMisc with Dependencies[CfgNode] {

  import tip.cfg.CfgOps._

  val cfg: ProgramCfg

  val valuelattice: IntervalLattice.type

  val liftedstatelattice: LiftLattice[statelattice.type]

  /**
    * Int values occurring in the program, plus -infinity and +infinity.
    */
  private val B = cfg.nodes.flatMap { n =>
    n.appearingConstants.map { x =>
      IntNum(x.value): Num
    } + MInf + PInf
  }

  object TipType extends Enumeration {
    protected case class TipTypeVal(min: Num, max: Num) extends super.Val {
      def bitsize = if (min == IntervalLattice.MInf || max == IntervalLattice.PInf) IntervalLattice.PInf else (min, max) match {
        case (IntNum(a), IntNum(b)) => b - a + (if (a == 0) 1 else 0)
      }

      def asPair = (min, max)
    }

    def suitable(min: Num, max: Num): TipTypeVal = (min, max) match {
      case (IntNum(a), IntNum(b)) if a >= Bool.min && a <= Bool.max && b >= Bool.min && b <= Bool.max => Bool
      case (IntNum(a), IntNum(b)) if a >= Byte.min && a <= Byte.max && b >= Byte.min && b <= Byte.max => Byte
      case (IntNum(a), IntNum(b)) if a >= Char.min && a <= Char.max && b >= Char.min && b <= Char.max => Char
      case (IntNum(a), IntNum(b)) if a >= Int.min && a <= Int.max && b >= Int.min && b <= Int.max => Int
      case (_, _) => BigInt
    }


    val Bool = TipTypeVal(0, 1)
    val Byte = TipTypeVal(-128, 127)
    val Char = TipTypeVal(0, 65535)
    val Int = TipTypeVal(-2147483648, 2147483647)
    val BigInt = TipTypeVal(MInf, PInf)
//    val Any = TipTypeVal(MInf, PInf)
  }

  def loophead(n: CfgNode): Boolean = indep(n).exists(cfg.rank(_) > cfg.rank(n))

  private def minB(b: IntervalLattice.Num) = B.filter(b <= _).min

  private def maxB(a: IntervalLattice.Num) = B.filter(_ <= a).max

  def widenInterval(x: valuelattice.Element, y: valuelattice.Element): valuelattice.Element =
    (x, y) match {
      case (IntervalLattice.EmptyInterval, _) => y
      case (_, IntervalLattice.EmptyInterval) => x
      case ((l1, h1), (l2, h2)) => TipType.suitable(maxB(Set(l1, l2).min), minB(Set(h1, h2).max)).asPair // something like this
    }

  def widen(x: liftedstatelattice.Element, y: liftedstatelattice.Element): liftedstatelattice.Element =
    (x, y) match {
      case (liftedstatelattice.Bottom, _) => y
      case (_, liftedstatelattice.Bottom) => x
      case (liftedstatelattice.Lift(xm), liftedstatelattice.Lift(ym)) =>
        liftedstatelattice.Lift(declaredVars.map { v =>
          v -> widenInterval(xm(v), ym(v))
        }.toMap)
    }
}
