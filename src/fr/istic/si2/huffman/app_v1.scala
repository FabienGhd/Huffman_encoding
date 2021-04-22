package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._

/**
 * Application principale V1 : arbre de code fixé
 */
object HuffmanApp1 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val gauche: Huffman = Noeud(0.57,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d')))
  
  val droite: Huffman = Noeud(0.43,Feuille(0.21,'b'),
                                                    Noeud(0.22,
                                                        Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e')))
  val h: Huffman = Noeud(1.00, gauche, droite)

  /*
  println(encodeList('a'::'b'::'c'::Nil,h))
  println(decode(Zero::Zero::One::Zero::One::One::One::Nil,h))
  */
  
  
  
  //dans cette application, les caractères non présents dans l'alphabet de l'arbre de code sont OUBLIÉ
  /**
   * @param h un arbre de huffman
   * L'utilisateur est demandé de rentrer une chaîne.
   * Affiche la chaîne encodée standard avec sa taille, 
   * suivi de cette chaîne encodée selon l'arbre h avec sa taille également,
   * et enfin affiche la chaîne decodée selon h
   */
  def applicationv1(h: Huffman): Unit = {
    
    var continue: Boolean = true
    
    while(continue) {
      
      println("Chaîne à encoder ?")
      val s = io.StdIn.readLine()
      
      println("Chaîne encodée standard : ")
      println("         " + vers16Bits(s))
      println("         taille (nb Bits) : " + vers16Bits(s).length())
      
      println("Chaîne encodée Huffman : ")
      if(encode(s,h) == Nil) {
       
        println("ERREUR! Aucun des caractères de la chaîne entré : " + s + " est présent dans l'arbre de Huffman en paramètre")
        
      } else {
      println("         " + encode(s,h))
      println("         taille (nb Bits) : " + encode(s,h).length)
      println("Chaîne décodée Huffman : ")
      println("         " + decode(encode(s,h), h))
      }
      
      println("Encore ? [Y/n]")
      val answer = io.StdIn.readChar()
      
      //traitement de la réponse de l'utilisateur : 
      continue = (answer == 'y') || (answer == 'Y')
      
    } 
  }// fin application v1
  
  applicationv1(h)
  
}