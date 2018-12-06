package codi

import scala.math.Numeric.Implicits.infixNumericOps

object Main extends App {

  def ajuda(): Unit ={
    println("scala nom instancia [1|2]")
  }

  override
  def main(args: Array[String]):Unit={
    if(args.length != 2 || (args(1) != "1" && args(1) != "2")) ajuda
    val problema = new DPLL(args(0), args(1).toInt)
    println("El model és")
    problema.mostrarCNF()
    println("\n\n")
    if(problema.dpll){
      println("HO....... SORPRESA! ÉS SAT!")
      problema.mostrarModel()
    }
    else println("HO....... SORPRESA! NO ÉS SAT!")
  }
}
