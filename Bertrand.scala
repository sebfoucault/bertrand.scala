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

    type Derivation = (List[Int], OpInstance)
    type Extraction = ((Int,Int), List[Int])

    def unique[T,X](l: List[T], k: (T) => X) = l.groupBy(k).map{ case(key,value) => value.head }.toList

    def uniqueChallengeAndOps = unique( _:List[ChallengeAndOps], (x: ChallengeAndOps) => x.problem.values)

    def extractAt(l: List[Int], i: Int, j: Int): Extraction = {
        val z = l.indices zip l
        val a = z.filter(_._1==i).head._2
        val b = z.filter(_._1==j).head._2
        val r = z.filter( (t) => (t._1 != i && t._1 != j)).map(_._2).toList 
        ((a,b), r)    
    }

    def pairComb(l:List[Int], commute: Boolean) = 
        for (i <- 0 to l.length - 1 ; j <- 0 to l.length - 1 ; if (i != j && (commute || i < j))) yield extractAt(l, i, j)

    def opComb(l:List[Int], op: Op):List[Derivation] = {
        val tester = (x:Extraction) => x match { case((o1, o2), l) => op.validate(o1, o2) }
        val mapper = (x:Extraction) => x match { case((o1, o2), l) => ((op(o1, o2)::l).sorted, new OpInstance(o1, o2, op)) }
        pairComb(l, !op.commutative).toList.collect( { case x if (tester(x)) => mapper(x) } )
    }

    def recombine(pnop: ChallengeAndOps, combs: List[Derivation]) = 
        combs.map( (cmb:Derivation) => ChallengeAndOps(Challenge(pnop.problem.target, cmb._1), cmb._2 :: pnop.operations) ) 

    def deriveChallengeByOp(pnop: ChallengeAndOps, op: Op) = pnop match {
        case ChallengeAndOps(Challenge(_, pbValues @ List(_,_, _*)), _) => recombine(pnop, opComb(pbValues, op)) 
        case _ => Nil
    }

    def deriveChallenges(pnop: ChallengeAndOps, ops: List[Op]) = ops.map((x:Op) => deriveChallengeByOp(pnop,x)).flatten

    def deriveChallenges(pnops: Seq[ChallengeAndOps], ops: List[Op]): List[ChallengeAndOps] = {
        def d(pnops: Seq[ChallengeAndOps], ops: List[Op], acc: List[ChallengeAndOps]): List[ChallengeAndOps] = pnops match{
            case x :: xs => d(xs, ops, deriveChallenges(x, ops) ::: acc)
            case Nil => acc
        }
        d(pnops, ops, Nil)
    }
    
    def solution(pnops: Seq[ChallengeAndOps]): Option[Seq[OpInstance]] = pnops match {
        case x :: xs if (x.problem.solved) =>  Some(x.operations.reverse)
        case x :: xs => solution(xs)
        case _ => None
    }

    def doSolve(pnops: Seq[ChallengeAndOps]): Option[Seq[OpInstance]] = solution(pnops) match {
        case Some(s) => Some(s)
        case None => doSolve(uniqueChallengeAndOps(deriveChallenges(pnops, Op.all)))
    }

    def solve(p: Challenge) = doSolve( ChallengeAndOps(Challenge(p.target, p.values.sorted),Nil) :: Nil)
}
