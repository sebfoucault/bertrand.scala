
final class Op(val sym: Char, val commutative: Boolean, etor: (Int,Int) => Int, vtor: (Int,Int) => Boolean) {
    def apply(x: Int, y: Int) = etor(x,y)
    def validate(x: Int, y: Int) = if (null == vtor) true else vtor(x, y)
}

object Op {
    val add = new Op('+', true, (x: Int, y: Int) => x+y, null)
    val sub = new Op('-', false, (x: Int, y: Int) => x-y, (x: Int, y: Int) => x > y)
    val mul = new Op('*', true, (x: Int, y: Int) => x*y, null)
    val div = new Op('/', false, (x: Int, y: Int) => x/y, (x: Int, y: Int) => (x % y == 0 && y != 0))
    val all = List(Op.add, Op.sub, Op.mul, Op.div)
}

class OpInstance(op1: Int, op2: Int, opr: Op) {
    override def toString = op1.toString + opr.sym + op2 + '=' + opr(op1, op2) 
}

case class Problem(target: Int, values: List[Int]) {
    def solved = values.contains(target)
}

case class ProblemAndOps(problem: Problem, operations: List[OpInstance])

class Solver {

    def extractAt[T](l: List[T], i: Int, j: Int) = {
        val z = l.indices zip l
        val a = z.filter(_._1==i).head._2
        val b = z.filter(_._1==j).head._2
        val r = z.filter( (t) => (t._1 != i && t._1 != j)).map(_._2).toList 
        ((a,b), r)
    }

    def pairComb[A](l:List[A], commute: Boolean) = {
        for (i <- 0 to l.length - 1 ; j <- 0 to l.length - 1 ; if (i != j && (commute || i < j))) yield {
            extractAt(l, i, j)
        }
    }

    def opComb(l:List[Int], op: Op):List[(List[Int], OpInstance)] = {
        val tester = (x:((Int,Int),List[Int])) => op.validate(x._1._1, x._1._2)
        val mapper = (x:((Int,Int),List[Int])) => (op(x._1._1, x._1._2) :: x._2, new OpInstance(x._1._1, x._1._2, op))
        pairComb(l, !op.commutative).toList.filter(tester).map(mapper)
    }

    def recombine(pnop: ProblemAndOps, combs: List[(List[Int],OpInstance)]): List[ProblemAndOps] = combs match {
        case x :: xs => ProblemAndOps(Problem(pnop.problem.target, x._1), x._2 :: pnop.operations) :: recombine(pnop, xs)
        case Nil => Nil
    }

    def deriveProblemByOp(pnop: ProblemAndOps, op: Op) = pnop match {
        case ProblemAndOps(Problem(target, pbValues @ List(_,_, _*)), ops) => recombine(pnop, opComb(pbValues, op)) 
        case _ => Nil
    }

    def deriveProblemsByOps(pnop: ProblemAndOps, ops: List[Op]): List[ProblemAndOps] = ops match {
        case Nil => Nil
        case x :: xs => deriveProblemByOp(pnop, x) ::: deriveProblemsByOps(pnop, xs)
    }

    def solve(pnop: ProblemAndOps) = {
        if (pnop.problem.solved) Some(pnop.operations.reverse) else doSolve(deriveProblemsByOps(pnop, Op.all))
    }

    def doSolve(pnops: List[ProblemAndOps]): Option[List[OpInstance]] = pnops match {
        case x :: xs => solve(x) orElse doSolve(xs)
        case _ => None
    }

    def solve(p: Problem) = {
        doSolve( ProblemAndOps(p,Nil) :: Nil)
    }
}
