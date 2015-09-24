import scala.annotation.tailrec

final class Op(val sym: Char, val commutative: Boolean, etor: (Int,Int) => Int, vtor: (Int,Int) => Boolean) {
    def apply(x: Int, y: Int) = etor(x,y)
    def validate(x: Int, y: Int) = if (null == vtor) true else vtor(x, y)
}

object Op {
    private val add = new Op('+', true, (x: Int, y: Int) => x+y, null)
    private val sub = new Op('-', false, (x: Int, y: Int) => x-y, (x: Int, y: Int) => x > y)
    private val mul = new Op('*', true, (x: Int, y: Int) => x*y, (x: Int, y: Int) => x != 1 && y != 1)
    private val div = new Op('/', false, (x: Int, y: Int) => x/y, (x: Int, y: Int) => (x % y == 0 && y != 0 && y != 1))
    val all = List(Op.add, Op.sub, Op.mul, Op.div)
}

class OpInstance(op1: Int, op2: Int, opr: Op) {
    override def toString = op1.toString + opr.sym + op2 + '=' + opr(op1, op2) 
}

case class Challenge(target: Int, values: List[Int]) {
    def solved = values.contains(target)
}

case class ChallengeAndOps(problem: Challenge, operations: List[OpInstance])

final class Solver {

    def unique[T,X](l: List[T], k: (T) => X) = l.groupBy(k).map{ case(key,value) => value.head }.toList

    def uniqueChallengeAndOps = unique( _:List[ChallengeAndOps], (x: ChallengeAndOps) => x.problem.values)

    def extractAt(l: List[Int], i: Int, j: Int) = {
        val z = l.indices zip l
        val r = z.groupBy( (x:(Int,Int)) => if (x._1 == i) 0 else if (x._1 == j) 1 else 2) 
        ((r(0).head._2,r(1).head._2), (r.get(2) orElse Some(Nil)).get.unzip._2.toList)
    }

    def pairComb(l:List[Int], commute: Boolean) = 
        for (i <- 0 to l.length - 1 ; j <- 0 to l.length - 1 ; if (i != j && (commute || i < j))) yield extractAt(l, i, j)

    def opComb(l:List[Int], op: Op):List[(List[Int], OpInstance)] = {
        val tester = (x:((Int,Int),List[Int])) => x match { case((o1, o2), l) => op.validate(o1, o2) }
        val mapper = (x:((Int,Int),List[Int])) => x match { case((o1, o2), l) => ((op(o1, o2)::l).sorted, new OpInstance(o1, o2, op)) }
        pairComb(l, !op.commutative).toList.collect( { case x if (tester(x)) => mapper(x) } )
    }

    def recombine(pnop: ChallengeAndOps, combs: List[(List[Int],OpInstance)]) = 
        combs.map( (cmb:(List[Int],OpInstance)) => ChallengeAndOps(Challenge(pnop.problem.target, cmb._1), cmb._2 :: pnop.operations) ) 

    def deriveChallengeByOp(pnop: ChallengeAndOps, op: Op) = pnop match {
        case ChallengeAndOps(Challenge(_, pbValues @ List(_,_, _*)), _) => recombine(pnop, opComb(pbValues, op)) 
        case _ => Nil
    }

    def deriveChallengesByOps(pnop: ChallengeAndOps, ops: List[Op]) = ops.map((x:Op)=>deriveChallengeByOp(pnop,x)).flatten

    @tailrec
    def doSolve(pnops: Seq[ChallengeAndOps]): Option[Seq[OpInstance]] = pnops match {
        case x :: xs if (x.problem.solved) =>  Some(x.operations.reverse)
        case x :: xs =>  doSolve(uniqueChallengeAndOps(xs ::: deriveChallengesByOps(x, Op.all)))
        case _ => None
    }

    def solve(p: Challenge) = doSolve( ChallengeAndOps(Challenge(p.target, p.values.sorted),Nil) :: Nil)
}
