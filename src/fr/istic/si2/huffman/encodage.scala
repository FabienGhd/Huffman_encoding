package fr.istic.si2.huffman

import Utils._
import ConstructionCode._

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h (si c est bien présent dans h)
   */
  //v0
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    h match {
      case Feuille(_,e1) => if(e1 == c) {Some(Nil)} else None 
      case Noeud(_,l,r) => encodeSymbol(c,r) match {
                                              case Some(x) => Some(One::x)
                                              case None => encodeSymbol(c,l) match {
                                                                              case Some(x) => Some(Zero::x)
                                                                              case None => None
        }
      }
    }
  }
    

  

  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  //V1
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
    
    l match {
      
      case Nil => Nil
      case e1::m => (encodeSymbol(e1,h)) match {
                                          case Some(x) => x ++ encodeList(m,h) 
                                          case None => encodeList(m,h)
      }
    }
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  //V1
  def encode(s: String, h: Huffman): List[Bit] = {
    encodeList(s.toList, h)
  }

  /**
   * @param h un arbre de Huffman
   * @return une chaîne de 0 et 1 uniquement représentant l'arbre h (voir partie 1.3 de l'énoncé)
   *         Les caractères encodables avec h sont représentés dans leur encodage binaire 16 bits.
   */
  def descriptionHuffman(h: Huffman): String = {
    h match {
      
      case Feuille(_, c) => "0" + vers16Bits(c.toString)
      
      case Noeud(_, left, right) => "1" + descriptionHuffman(left) + descriptionHuffman(right)
    }
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  //V3
  def encode(message: String): String = {
    
    val tree = codeHuffman(analyseFrequences(message))
    
    descriptionHuffman(tree) + listBitToString(encode(message,tree))
  }
}