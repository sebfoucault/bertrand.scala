import scala.annotation.tailrec

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

case class Challenge(target: Int, values: List[Int]) {
    def solved = values.contains(target)
}

case class ChallengeAndOps(problem: Challenge, operations: List[OpInstance])

final class Solver {

    def unique[T,X](l: List[T], k: (T) => X): List[T] = {
        l.groupBy(k).map{ case(key,value) => value.head }.toList
    }

    def uniqueChallengeAndOps = unique( _:List[ChallengeAndOps], (x: ChallengeAndOps) => x.problem.values.sorted)

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

    def recombine(pnop: ChallengeAndOps, combs: List[(List[Int],OpInstance)]): List[ChallengeAndOps] = {
        val r = combs.map( (cmb:(List[Int],OpInstance)) => ChallengeAndOps(Challenge(pnop.problem.target, cmb._1), cmb._2 :: pnop.operations) )
        uniqueChallengeAndOps(r)   
    }

    def deriveChallengeByOp(pnop: ChallengeAndOps, op: Op) = pnop match {
        case ChallengeAndOps(Challenge(_, pbValues @ List(_,_, _*)), _) => recombine(pnop, opComb(pbValues, op)) 
        case _ => Nil
    }

    def deriveChallengesByOps(pnop: ChallengeAndOps, ops: List[Op]): List[ChallengeAndOps] = {
        val r = ops.map((x:Op)=>deriveChallengeByOp(pnop,x)).flatten
        uniqueChallengeAndOps(r)
    }

    @tailrec
    def doSolve(pnops: List[ChallengeAndOps]): Option[List[OpInstance]] = pnops match {
        case x :: xs => {
            if (x.problem.solved) 
                Some(x.operations.reverse) 
            else 
                doSolve(uniqueChallengeAndOps(xs ::: deriveChallengesByOps(x, Op.all)))
        }   
        case _ => None
    }

    def solve(p: Challenge) = {
        doSolve( ChallengeAndOps(p,Nil) :: Nil)
    }
}
