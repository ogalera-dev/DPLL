package codi

import scala.math.Numeric.Implicits.infixNumericOps

object Main extends App {

  def ajuda(): Unit ={
    println("scala programa instancia [1|2] [-l]")
    System.exit(-1)
  }

  override
  def main(args: Array[String]):Unit={
    if((args.length != 2 && args.length != 3) || (args(1) != "1" && args(1) != "2") || (args.length == 3 && args(2) != "-l")) ajuda
    val log = args.length == 3 && args(2) == "-l"
    val problema = new DPLL(args(0), args(1).toInt, log)
    println("El model és")
    problema.mostrarCNF()
    if(problema.dpll){
      println("\n\nHO....... SORPRESA! ÉS SAT!")
      problema.mostrarModel()
    }
    else println("\n\nHO....... SORPRESA! NO ÉS SAT!")
  }
}
