package codi

import scala.collection.mutable
import scala.io.Source

//Objecte amb els models disponibles
object Mode extends Enumeration{
  val PRIMER_FALSE:Int = 1
  val PRIMER_TRUE = 2
}

class DPLL (fitxer: String, mode : Int = Mode.PRIMER_TRUE, log: Boolean = false) {

  //El model serà un mapa d'enter a boolea, on els enters seran totes totes les variables
  //números amb els seus negatius, i el boolea indicarà si forma part de la solució (true) o no (false).
  private val model = mutable.Map.empty[Int, Option[Boolean]]

  //Es carrega el contingut del fitxer
  private val fontDades = Source.fromFile(fitxer)

  //Es spearen les linies per \n
  private val linies = fontDades.mkString.split("\n")

  //Linies del model parsejat.
  private val CNF = parsejarCNF(linies.toList)

  //Nombre de BackTrackings que fa l'algoritme
  private var nBackTracking = 0

  //Nombre de variables del model, el maxim de tots els valors positius o el mínim de tots els valors negatius (en positiu)
  private val llargada = {
    val m = (CNF map {x => x.max}).max
    val n = -1* (CNF map {x => x.min}).min
    if(m > n) m else n
  }

  //Posa l'enter index a true i a false el seu negat
  def posarATrue(index: Int)={
    model(index) = Some(true)
    model(-index) = Some(false)
  }

  //Posa l'enter index a false i a true el seu negat
  def posarAFalse(index: Int) = {
    model(index) = Some(false)
    model(-index) = Some(true)
  }

  //Carregar la instància del CNF
  def parsejarCNF(linies: List[String]): List[List[Int]] = linies match{
    //Cas base
    case List() => List()
    case s::xs =>
      //En cas de començar per c, es filtren els comentaris
      if(s(0) == 'c') parsejarCNF(xs)
      //En cas de trobar la línia que especifica el format, també es salta perquè aquest ja vindrà inplicit
      else if(s(0) == 'p') parsejarCNF(xs)
      //Altrament, es sap que forma part d'una CNF
      else (s.replace(" 0","").split(" ").toList.map(_.toInt).filter(0 !=)) :: parsejarCNF(xs)
  }

  //Solver principal del programa, implementa Backtracking
  def dpll(): Boolean ={
    def dpllImm(index: Int): Boolean = {
      //Primer cal assignar totes les variables, si ja s'han assignat totes, es comprova si el model es satisfa amb l'assignació.
      if(index == 0) {
        if(log){
          println("S'ha assignat valor a totes les variables, el model és:")
          mostrarModel()
        }
        satisfa
      }
      else{
        //Segons el mode utilitzat, es comença posant les variables a true (Mode PRIMER_TRUE) o es comença posant les variables a false (PRIMER_FALSE)
        if(mode == Mode.PRIMER_TRUE) posarATrue(index)
        else posarAFalse(index)

        if(dpllImm(index-1)) true
        else{
          nBackTracking += 1
          //Com hi ha hagut punt de decisió, ara es prova l'altre opció segons mode.
          if(mode == Mode.PRIMER_TRUE) posarAFalse(index)
          else posarATrue(index)

          dpllImm(index-1)
        }
      }
    }
    dpllImm(llargada)
  }



  def satisfa(): Boolean ={
    //De la llista de conjuncions (CNFs) es fa un plegat a la dreta amb un true com a element neutre per a que s'hagin de complir totes les conjuncions, de tal manera que:
    //Per cada disjunció de la CNF, es fa un  plegat a la dreta amb un false com a element neutre per així forçar a complir totes les disjuncions

    //Aquesta funció diu si la llista de CNFs és satisfactible per l'assignació que hi ha actualment en la variable "model"
    CNF.foldRight(true)((x, acc) => acc && x.foldRight(false)((y, acc) => acc || (model(y) == Some(true))))
  }

  //Mostra la CNF carregada
  def mostrarCNF() : Unit = {
    CNF map (x => {x map {y => print(y + " ")}; print("\n")})
  }


  //Mostra el model obtingut
  def mostrarModel(){
    Range(1,llargada+1,1) map (x => (print(x), print(" "), println(model(x)==Some(true))))
    println("Nombre de BackTrackings: "+ nBackTracking)
  }


}
