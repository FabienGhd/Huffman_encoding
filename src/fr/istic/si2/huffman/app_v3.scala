package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._
import ConstructionCode._

/**
 * Application principale V3 : avec transmission du code
 */
object HuffmanApp3 extends App {

  val gauche: Huffman = Noeud(0.57,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d')))
  
  val droite: Huffman = Noeud(0.43,Feuille(0.21,'b'),
                                                    Noeud(0.22,
                                                        Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e')))
  val h: Huffman = Noeud(1.00, gauche, droite) 
  
  /**
   * @param h un arbre de Huffman
   * L'utilisateur est demandé de de rentrer le fichiers à encoder.
   * Affiche La representation binaire de l'arbre de Huffman correspondant à ce fichier entré
   * Affiche l'emplacement du fichier compressé puis l'emplacement du fichier décodé
   */
   def applicationv3(h: Huffman): Unit = {
    
    var continue: Boolean = true
    
    while(continue) {
      
      println("Fichier à encoder ?")
      println("Par exemple: fichiers/texte1.txt")
        val file_name = io.StdIn.readLine()
      
      val file_content = lireFichier(file_name)
      val encoding = vers16Bits(file_content) 
      val freq = analyseFrequences(file_content)
      
      
      println("La representation binaire de l'arbre de Huffman correspondant au fichier que vous avez entré:")
      println("---")
      println("    " + descriptionHuffman(codeHuffman(freq)))
      println("---")
      println()
      
      //compression 
      val encoded = encode(file_content)
     
      ecrireFichier("fichiers/compressionV3ByFabien.txt", encoded)
      println("Vous pouvez retrouver le fichier COMPRESSÉ dans fichiers/CompressionV3ByFabien.txt")
      println("___")
      println()
      
      
      //decode
      val decoded = decode(encoded)
      ecrireFichier("fichiers/decodeV3ByFabien.txt", decoded)
      println("Vous pouvez retrouver le fichier DECODÉ dans fichiers/fichiers/decodeV3ByFabien.txt")
      println()
      println("---")

      
       println("Encore ? [Y/n]")
         val answer = io.StdIn.readChar()
      
      //traitement de la réponse de l'utilisateur : 
      continue = (answer == 'y') || (answer == 'Y')
    
    }
  }
  applicationv3(h)

}