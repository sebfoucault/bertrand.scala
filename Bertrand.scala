import scala.annotation.tailrec

final class Op(val sym: Char, val commutative: Boolean, etor: (Int,Int) => Int, vtor: (Int,Int) => Boolean) {
    def apply(x: Int, y: Int) = etor(x,y)
    def validate(x: Int, y: Int) = if (null == vtor) true else vtor(x, y)
}

object Op {
    val add = new Op('+', true, (x: Int, y: Int) => x+y, null)
    val sub = new Op('-', false, (x: Int, y: Int) => x-y, (x: Int, y: Int) => x > y)
    val mul = new Op('*', true, (x: Int, y: Int) => x*y, (x: Int, y: Int) => x != 1 && y != 1)
    val div = new Op('/', false, (x: Int, y: Int) => x/y, (x: Int, y: Int) => (x % y == 0 && y != 0 && y != 1))
}

class OpInstance(op1: Int, op2: Int, opr: Op) {
    override def toString = op1.toString + opr.sym + op2 + '=' + opr(op1, op2) 
}

case class Challenge(target: Int, values: List[Int])
case class ChallengeOps(challenge: Challenge, ops: List[OpInstance])

final class Solver {

    case class Derivation(vals: List[Int], op: OpInstance)
    case class Extraction(first: Int, second: Int, rest:List[Int])

    def unique[T,X](l: List[T], k: (T) => X) = l.groupBy(k).map{ case(key,value) => value.head }.toList

    def uniqueChallengeOps = unique( _:List[ChallengeOps], (x: ChallengeOps) => x.challenge.values)

    def extractAt(l: List[Int], i: Int, j: Int): Extraction = {
        val z = l.zipWithIndex
        val a = z.filter( _ match { case (_,ix) => ix == i } ) match { case (v,_) :: s => v }
        val b = z.filter( _ match { case (_,ix) => ix == j } ) match { case (v,_) :: s => v }
        val r = z.filter( _ match { case (_,ix) => ix != i && ix != j } ).map( _ match { case (v,_) => v } ) 
        Extraction(a,b,r)    
    }

    def pairComb(l:List[Int], commute: Boolean) = 
        for (i <- 0 to l.length - 1 ; j <- 0 to l.length - 1 ; if (i != j && (commute || i < j))) yield extractAt(l, i, j)

    def opComb(l:List[Int], op: Op) = {
        val tester = (x:Extraction) => op.validate(x.first, x.second)
        val mapper = (x:Extraction) => Derivation((op(x.first, x.second)::x.rest).sorted, new OpInstance(x.first, x.second, op))
        pairComb(l, !op.commutative).toList.collect( { case x if (tester(x)) => mapper(x) } )
    }

    def recombine(pnop: ChallengeOps, combs: List[Derivation]) = 
        combs.map( (drv:Derivation) => ChallengeOps(Challenge(pnop.challenge.target, drv.vals), drv.op :: pnop.ops) ) 

    def deriveChallengeByOp(pnop: ChallengeOps, op: Op) = pnop match {
        case ChallengeOps(Challenge(_, pbValues @ List(_,_, _*)), _) => recombine(pnop, opComb(pbValues, op)) 
        case _ => Nil
    }

    def deriveChallenge(pnop: ChallengeOps, ops: List[Op]) = ops.map((x:Op) => deriveChallengeByOp(pnop,x)).flatten

    def deriveChallenges(pnops: Seq[ChallengeOps], ops: List[Op]) = {
        def d(pnops: Seq[ChallengeOps], ops: List[Op], acc: List[ChallengeOps]): List[ChallengeOps] = pnops match {
            case x :: xs => d(xs, ops, deriveChallenge(x, ops) ::: acc)
            case Nil => acc
        }
        d(pnops, ops, Nil)
    }
    
    def solution(pnops: Seq[ChallengeOps]): Option[Seq[OpInstance]] = pnops match {
        case ChallengeOps(Challenge(tgt, vals), ops) :: xs if (vals.contains(tgt)) =>  Some(ops.reverse)
        case x :: xs => solution(xs)
        case _ => None
    }

    def doSolve(pnops: Seq[ChallengeOps]): Option[Seq[OpInstance]] = solution(pnops) match {
        case Some(s) => Some(s)
        case None => doSolve(uniqueChallengeOps(deriveChallenges(pnops, List(Op.add, Op.sub, Op.mul, Op.div))))
    }

    def solve(p: Challenge) = doSolve( ChallengeOps(Challenge(p.target, p.values.sorted),Nil) :: Nil)
}
