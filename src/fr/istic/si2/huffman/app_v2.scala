package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._

/**
 * Application principale V2 : avec construction du code
 */
object HuffmanApp2 extends App {

  /**
   * Une liste de couples caractère / fréquence d'apparition
   * à utiliser par l'application principale.
   */
  val lfreqs: List[(Char, Double)] = 
    ('a',0.25)::('b',0.21)::('c',0.18)::('d',0.14)::('e',0.09)::('f',0.07)::('g',0.06)::Nil

   
  /**
   * @param freqChar une liste de tuples de taille 2: (Char,Double)
   * affiche l'arbre de code selon freqChar
   * Demande a l'utilisateur une chaine
   * affiche dans un fichier texte l'encodage d'un texte 
   */
  def applicationv2(freqChar: List[(Char, Double)]): Unit = {
    
    var continue: Boolean = true
    while(continue) {
    
    println("Arbre de code correspondant à une liste de caractères/fréquences d'apparition: ")
    println("---")
    println("    " + codeHuffman(freqChar))
    println("---")
    println()
    
    
    println("Afin de créer l'arbre par analyse de la frequence des caractères, veuillez d'abord entrer une chaîne de caractère: ")
      val text = io.StdIn.readLine()
    println("---")
    println("    " + codeHuffman(analyseFrequences(text)))
    println("---")
    
    
    
   
    println("Veuillez entrer un nom de fichier texte dont vous souhaitez encoder le contenu: ")
    println("~~~~> Aide: par exemple nous pouvons utilisé les fichiers qui nous sont donnés comme: fichiers/texte2.txt")
      val file_name: String = io.StdIn.readLine()
    println()
    
    //lit le contenu, l'encode et analyse les différentes fréquences
    val file_content = lireFichier(file_name)
    val encoding = vers16Bits(file_content)
    val freq = analyseFrequences(file_content)
    
    
    //ecrit la chaîne encodée dans un nouveau fichier
    val encoded: List[Bit] = encode(file_content, codeHuffman(freq))
    val encoded_string = listBitToString(encoded)
    ecrireFichier("fichiers/compressionV2ByFabien.txt", encoded_string)
    println("     On peut retrouver le fichier compressé dans fichiers/compressionV2ByFabien.txt")
    println("---")
    println()
    
    println("Encore ? [Y/n]")
      val answer = io.StdIn.readChar()
      
      //traitement de la réponse de l'utilisateur : 
      continue = (answer == 'y') || (answer == 'Y')
      
  }
  }
 applicationv2(lfreqs)
}