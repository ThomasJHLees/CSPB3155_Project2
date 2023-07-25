package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => {
            val eval1 = evalExpr(l, env)
            eval1 match {
                case NumValue(l1) => {
                    val newcList:List[(Double,Double)] = List((0,0),(l1,0))
                    val newPoly = List(Polygon(newcList))
                    FigValue(new MyCanvas(newPoly))
                }
                case default => throw new IllegalArgumentException("A NumValue must be provided to Line")
            }
        }
        case EquiTriangle(sideLength) => {
            val eval1 = evalExpr(sideLength, env)
            eval1 match {
                case NumValue(side) => {
                    val pnt1:(Double, Double) = (0,0)
                    val pnt2:(Double, Double) = (side,0)
                    val pnt3:(Double, Double) = (side/2, (math.sqrt(3*side)/2))
                    val newcList:List[(Double, Double)] = List(pnt1, pnt2, pnt3)
                    val newPoly = List(Polygon(newcList))
                    FigValue(new MyCanvas(newPoly))
                }
                case default => throw new IllegalArgumentException("A NumValue must be provided to Rectangle")
            }
        }
        case Rectangle(sideLength) => {
            val eval1 = evalExpr(sideLength, env)
            eval1 match {
                case NumValue(side) => {
                    val newcList:List[(Double, Double)] = List((0,0),(side,0),(side,side),(0,side))
                    val newPoly = List(Polygon(newcList))
                    FigValue(new MyCanvas(newPoly))
                }
                case default => throw new IllegalArgumentException("A NumValue must be provided to Rectangle")
            }
        }
        case Circle(rad) => {
            val eval1 = evalExpr(rad, env)
            eval1 match {
                case NumValue(v) => {
                    val cent = (v,v)
                    val newCirc = List(MyCircle(cent, v))
                    FigValue(new MyCanvas(newCirc))
                }
                case default => throw new IllegalArgumentException("A NumValue must be provided to Circle")
            }
            
        }
        case Plus (e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            v1 match {
                case NumValue(v) => {
                    v2 match {
                        case NumValue(vv) => NumValue(v+vv)
                        case default => throw new IllegalArgumentException("e1 and e2 must share a type")
                    }
                }
                case FigValue(v) => {
                    v2 match {
                        case FigValue(vv) => FigValue(v.overlap(vv))
                        case default => throw new IllegalArgumentException("e1 and e2 must share a type")
                    }
                }
                case default => throw new IllegalArgumentException("Unsupported type. Please use FigValue or NumValue")
            }
        }
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            v1 match {
                case NumValue(v) => {
                    v2 match {
                        case NumValue(vv) => NumValue(v*vv)
                        case default => throw new IllegalArgumentException("e1 and e2 must share a type")
                    }
                }
                case FigValue(v) => {
                    v2 match {
                        case FigValue(vv) => FigValue(v.placeRight(vv))
                        case default => throw new IllegalArgumentException("e1 and e2 must share a type")
                    }
                }
                case default => throw new IllegalArgumentException("Unsupported type. Please use FigValue or NumValue")
            }
        }
        case Div(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            v1 match {
                case NumValue(v) => {
                    v2 match {
                        case NumValue(vv) => NumValue(v/vv)
                        case default => throw new IllegalArgumentException("e1 and e2 must share a type if e1 is NumValue")
                    }
                }
                case FigValue(v) => {
                    v2 match {
                        case FigValue(vv) => FigValue(v.placeTop(vv))
                        case NumValue(vv) => FigValue(v.rotate(vv))
                        case default => throw new IllegalArgumentException("e2 must be FigValue or NumValue if e1 is FigValue")
                    }
                }
                case default => throw new IllegalArgumentException("Unsupported type. Please use FigValue or NumValue")
            }
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => {Closure(x, e, env)} //Taken from assignment 6
        case LetRec(f, x, e1, e2) => { //Taken from assignment 6
            val newEnv = ExtendREC(f, x, e1, env)
            evalExpr(e2, newEnv)
        }
        case FunCall(fCallExpr, arg) => { //Taken from assignment 6
            val v1 = evalExpr(fCallExpr, env)
            val v2 = evalExpr(arg, env)
            v1 match {
                case Closure(x, closure_ex, closed_env) => {
                    // First extend closed_env by binding x to v2
                    val new_env = Extend(x, v2, closed_env)
                    // Evaluate the body of the closure under the extended environment.
                    evalExpr(closure_ex, new_env)
                }
                case _ => throw new IllegalArgumentException(s"Function call error: expression $fCallExpr does not evaluate to a closure")
            }
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
