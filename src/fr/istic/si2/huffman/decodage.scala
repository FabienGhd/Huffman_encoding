package fr.istic.si2.huffman

import Utils._

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   *          si l est un chemin valide de h
   */
  //v0
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h, l) match {
      
      case (Noeud(_,_,_),Nil) => None
      
      case (Feuille(_,_), x::tail) => None
      
      case (Feuille(_,e1), Nil) => Some(e1)
      
      case (Noeud(_,left,right), Zero::tail) => decodeSymbolv0(left,tail)
      
      case (Noeud(_,left,right), One::tail) => decodeSymbolv0(right,tail)
      
    }
  }
  
  
  
  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l
   *         - deuxième composante : la liste des bits restant à décoder
   */
  //V1
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    (h,l) match {
      
    case (Noeud(_,_,_), Nil) => (None,Nil)
    
    case (Feuille(_,e1), Nil) => (Some(e1), Nil)
    
    case (Feuille(_,e1), m) => (Some(e1),m)
    
    case (Noeud(_,l,r), One::m) => decodeSymbol(r,m)
    
    case (Noeud(_,l,t), Zero::m) => decodeSymbol(l,m)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   */
  // V1
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    
    decodeSymbol(h, l) match {
      
      case (None, _) => None
      
      case (Some(x), Nil) => Some(x.toString)
      
      case (Some(x), tail) => decode(tail,h) match {
                                                case Some(y) => Some(x + y)
                                                case None => None
      
      }
                                            
      
    }
  }
  
  
  //---------------- Fonctions auxiliaires pour réaliser la fonction lireDescription (V3) ----------------\\
           
  /**
   * @param l une liste de Bit
   * @param n un entier positif
   * @return la liste de bit l privée de ses n premier(s) élements
   */
  def drop(l: List[Bit], n: Int): List[Bit] = {
    
    (l,n) match {
      
      case (Nil,_) => Nil
      
      case (x :: tail,0) => x :: drop(tail,0)
      
      case (x :: tail, n) => drop(tail, n-1)
    }
  }
  
  /**
   * @param l une liste de Bit
   * @param n un entier
   * @return la liste de bit l des n premier(s) élement de cette liste
   */
  def grab(l: List[Bit], n: Int): List[Bit] = {
    
    (l,n) match {
      
      case (Nil,_) => Nil
      
      case (x :: tail,0) => Nil
      
      case (x :: tail,n) => x :: grab(tail,n-1)
    }
  }
  /**
   * @param l une liste de bits décrivant, au moins, la représentation binaire d'un arbre de Huffman
   * @return un tuple de taille 2 comprenant :
   *         - l'arbre de code de Huffman reconstruit à partir du début de l
   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
   */
  def lireDescription(l: List[Bit]): (Huffman, List[Bit]) = {
    
    l match {
      
      case Nil => sys.error("Impossible car liste vide")
      
      case Zero :: tail =>  
                          val debut_list = (listBitToString(grab(tail,16)))
                          
                          ((Feuille(0.0, toChar(debut_list))), drop(tail,16))
      
      case One :: tail =>
                          val (left, x) = lireDescription(tail)
                          val (right, y) = lireDescription(x)
                          
                          (Noeud(0.0, left, right), y)
                            
  }
  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    
    val (tree, message) = lireDescription(stringToListBit(messageEnc))
    
    decode(message, tree) match {
      case None => ""
      case Some(x) => x
    }
  }

}