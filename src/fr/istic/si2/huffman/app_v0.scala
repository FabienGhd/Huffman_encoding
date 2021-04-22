package fr.istic.si2.huffman


import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V0 : arbre de code fixé, encodage/décodage de caractères
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  // V0
  //utilisons des valeurs intermédaire afin de simplifier le code de l'arbre final
  val gauche: Huffman = Noeud(0.57,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d')))
  
  val droite: Huffman = Noeud(0.43,Feuille(0.21,'b'),
                                                    Noeud(0.22,
                                                        Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e')))
  val h: Huffman = Noeud(1.00, gauche, droite)
    

  //V0
  /**
   * @param h un arbre de Huffman
   * affiche un caractère, suivi de sa liste de bit, puis de la chaîne de code et la liste de bits décodée
   */
  def applicationv0_simple(h: Huffman): Unit = {
   for(c <- 'a' to 'z') {
     
     encodeSymbol(c,h) match {
       
       case None => println(c + " ne peut pas être encodé.")
       
       case Some(list_bit) => println(c + " " + list_bit + " " + listBitToString(list_bit) + " " + decode(list_bit,h))
     }
   }
  }
  applicationv0_interaction(h)
  
  //créeons autre application v0 avec, cette fois-ci, une interaction avec l'utilisateur
  /**
   * @param h un arbre de Huffman
   * demande un caractère à l'utilisateur, affiche ce caractère, suivi de sa liste de bit, puis de la chaîne de code et la liste de bits décodée
   */
  def applicationv0_interaction(h: Huffman): Unit = { 
    var continue: Boolean = true
   
    while(continue) {
      println("Veuillez entrer un caractère à encodé: ")
      val c1: Char = io.StdIn.readChar()
      
      encodeSymbol(c1, h) match {
        case None => println("Le caractère entré ne peut pas être encodé")
                                                                           
        case Some(list_bit) => println(c1 + " " + list_bit + " " + listBitToString(list_bit) + " " + decode(list_bit,h))   
                               
      }
      println("Encore ? [Y/n]")
      val answer = io.StdIn.readChar()
      
      //traitement de la réponse de l'utilisateur : 
      continue = (answer == 'y') || (answer == 'Y')
    }
  }
 
   
 
  

}