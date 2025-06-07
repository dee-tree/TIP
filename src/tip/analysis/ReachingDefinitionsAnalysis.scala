package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.ast.AstOps._
import tip.ast._
import tip.cfg.CfgOps._
import tip.cfg._
import tip.lattices._
import tip.solvers._

/**
  * Base class for live variables analysis.
  */
abstract class ReachingDefinitionsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {

  val lattice: MapLattice[CfgNode, PowersetLattice[AStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case v: AVarStmt => s ++ v.declIds.map { i => AVarStmt(List(i), v.loc) }
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier => s.filter { stmt =>
                stmt match {
                  case v: AVarStmt => v.declIds.head.name != id.name
                  case pas: AAssignStmt => pas.left match {
                    case pid: AIdentifier => pid.name != id.name
                    case _ => ???
                  }
                }
              } + as
              case _ => ???
            }
          case _ => s
        }
      case _ => s
    }
}

/**
  * Reaching definitions analysis that uses the simple fixpoint solver.
  */
class ReachingDefinitionsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Reaching definitions analysis that uses the worklist solver.
  */
class ReachingDefinitionsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
