package fr.istic.si2.huffman

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  //V2
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil => Nil
      case h::tail => Feuille(h._2, h._1) :: initHuffman(tail) //h._2 est la frequence et h._1 est le charactère 
    }
  }
  
  /**
   * @param h un arbre de Huffman
   * @return la frequence d'une feuille ou d'un noeud de h
   */
  def getFrequency(h: Huffman): Double = {
    h match {
      
      case Feuille(freq,_) => freq
      case Noeud(freq,_,_) => freq
    }
  }
  
  /**
   * @h un arbre de Huffman
   * @param l une liste de Huffman
   * @return la liste de Huffman l avec f inséré respectant l'ordre croissant des fréquences
   */
  def insert(h: Huffman, l: List[Huffman]): List[Huffman] = {
    l match {
     
      case Nil => h::Nil
      case x :: tail => if(getFrequency(x) >= getFrequency(h)) { h :: x :: tail } else { x :: insert(h,tail)}
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match {
      case Nil => Nil
      case x :: tail => insert(x, triSelonFreq(tail))
    }
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    l match {
      case e1 :: e2 :: tail => Noeud(getFrequency(e1) + getFrequency(e2), e1, e2) :: tail
      case _ => sys.error("Fusion impossible si la liste contient strictement moins de 2 élements.")
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    l match {
      case Nil => sys.error("Impossible de faire un fusion sur une liste vide")
      case x :: Nil => x
      case _ => fusion(uneFusion(triSelonFreq(l)))
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }
  
   
  //---------------- Fonctions auxiliaires pour faire l'analyse de frequences ----------------\\

  /**
   * @param c un caratère
   * @param l une chaîne de caractère
   * @return le nombre d'occurence(s) de c dans l
   */
  def occurrence(c: Char, l: List[Char]): Double = {
    l match {
      
      case Nil => 0
      case e1 :: tail => if(e1 == c) {1 + occurrence(c,tail)} else {occurrence(c,tail)}
    }
  }
  
  /**
   * @param c un caractère
   * @param l une liste de caractère
   * @return la liste de carctère sans les apparitions de a
   */
  def removeChar(c: Char, l: List[Char]): List[Char] = {
    l match {
      
      case Nil => Nil
      case e1 :: tail => if(e1 == c) {removeChar(c, tail)} else {e1 :: removeChar(c,tail)}
    }
  }
  
  /**
   * @param l une liste de caractère
   * @return une liste avec une seule et seulement une seuule apparition de chaque caractère de l 
   */
  def sansDoublon(l: List[Char]): List[Char] = {
    l match {
      
      case Nil => Nil
      case e1 :: tail => e1 :: sansDoublon(removeChar(e1,tail))
    }
  }
  
  //la frequence d'apparition d'un caractère dans une liste peut s'obtenir a l'aide du rapport suivant:
  //  (nombre d'apparition du char) / (longueur de la liste)
  /**
   * @param c un cartère
   * @param l une liste de caractère
   * @return la fréquence d'occurence de c dans l 
   */
  def get_freq_occurence(c: Char, l: List[Char]): Double = {
    occurrence(c, l) / l.length
  }
  
  /**
   * @param c un caractère
   * @param l une liste de caractère
   * @return un tuple de taille 2 suivant: (c /frequence d'apparition de c dans l) 
   */
  def get_couple(c: Char, l: List[Char]): (Char, Double) = {
    (c, get_freq_occurence(c,l))
  }
  
  /**
   * @param l1 une chaine de caractère
   * @param l2 une chaine de caractère d'élements identique à l1
   * @return une liste de tuple de taille 2: (caractère/double) correspondant au tableau de frequences des caractères contenu dans l1
   */
  def get_couple_list(l1: List[Char], l2: List[Char]): List[(Char, Double)] = {
  
    sansDoublon(l1) match {
      
      case Nil => Nil
      case e1 :: tail => (get_couple(e1, l2))::get_couple_list(tail, l2)
    }
  }
  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {
    get_couple_list(s.toList, s.toList)
  }

}