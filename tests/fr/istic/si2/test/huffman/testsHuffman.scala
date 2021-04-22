package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._
import fr.istic.si2.huffman.ConstructionCode._


class TestsHuffman {

  val _ = new AppInit(HuffmanApp0) // Ne pas supprimer cette ligne.
  
  //variables globales:
  
  val gauche: Huffman = Noeud(0.57,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d')))
  
  val droite: Huffman = Noeud(0.43,Feuille(0.21,'b'),
                                                    Noeud(0.22,
                                                        Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e')))
  val h: Huffman = Noeud(1.00, gauche, droite)
  
  val delta: Double = 0.01 //precision pour les tests
  
  
   //---------------------------------------------------\\
   //__________________APPLICATION V0 ___________________\\
   //-----------------------------------------------------\\

  //---------------- TESTS decodeSymbolv0 ------------------\\
  
  @Test
  def EncodeSymbolv0_tests() {
    
    assertEquals(Some('a'),decodeSymbolv0(h, Zero::Zero::Nil))
    assertEquals(None,decodeSymbolv0(h, Nil))
    assertEquals(None,decodeSymbolv0(h, One::One::Nil))
    assertEquals(Some('g'),decodeSymbolv0(h, One::One::Zero::One::Nil))
  }
   
   
  //---------------- TESTS encodeSymbol ------------------\\

  @Test
  def encodeSymbol_tests() {
    
    assertEquals(Some(One::One::One::Nil),encodeSymbol('e',h))
    assertEquals(None,encodeSymbol('w',h))
    assertEquals(Some(Zero::One::Zero::Nil),encodeSymbol('c',h))
  }
 
  //---------------- TESTS ListBitToString ------------------\\
  @Test
  def testlistBitToString_tests() {
    assertEquals("", listBitToString(Nil))
    assertEquals("0", listBitToString(Zero::Nil))
    assertEquals("1", listBitToString(One::Nil))
    assertEquals("1100", listBitToString(One::One::Zero::Zero::Nil))
     
  }
  
  
    


  
  
   //---------------------------------------------------\\
   //__________________APPLICATION V1 ___________________\\
   //-----------------------------------------------------\\


   //---------------- TESTS encodeList ------------------\\
  @Test
  def encodeList_tests() {
    
    assertEquals(Zero::One::Zero::Nil,encodeList('c'::Nil,h))
    assertEquals(Nil,encodeList('w'::Nil,h))
    assertEquals(One::One::One::One::Zero::Zero::One::One::Nil,encodeList('e'::'b'::'d'::Nil,h))
    assertEquals(Zero::Zero::One::One::Zero::One::Zero::Zero::Zero::Zero::Nil,encodeList('a'::'g'::'a'::'a'::Nil,h))
  }
  
  //---------------- TESTS decodeSymbol ------------------\\
  @Test
  def decodeSymbol_tests() {
    
    assertEquals((Some('a'),One::One::One::Nil), decodeSymbol(h, Zero::Zero::One::One::One::Nil))
    assertEquals((Some('g'),One::Zero::Zero::One::Nil), decodeSymbol(h, One::One::Zero::One::One::Zero::Zero::One::Nil))
    assertEquals((None,Nil), decodeSymbol(h,Nil))
  }
 
  //---------------- TESTS decode ------------------\\
  @Test
  def decodeV1_tests() {
    
     assertEquals(None, decode(Nil, h))
     assertEquals(Some("be"), decode(One :: Zero :: One :: One :: One :: Nil, h))
     assertEquals(Some("abf"), decode(Zero::Zero::One::Zero::One::One::Zero::Zero::Nil, h))
     assertEquals(Some("fabed"), decode(One::One::Zero::Zero::Zero::Zero::One::Zero::One::One::One::Zero::One::One::Nil, h))

  }

  
  
   //---------------------------------------------------\\
   //__________________APPLICATION V2 ___________________\\
   //-----------------------------------------------------\\
  
  //---------------- TESTS initHuffman ------------------\\

  @Test
  def initHuffman_tests() {
    
    assertEquals(Nil, initHuffman(Nil)) 
    assertEquals((Feuille(0.78, 'f')::Feuille(0.21, 'e')::Nil), initHuffman(('f', 0.78) :: ('e', 0.21) :: Nil))
    assertEquals((Feuille(0.45, 'x')::Nil), initHuffman(('x', 0.45)::Nil))
  }
  
  //---------------- TESTS getFrequency ------------------\\
  @Test
  def getFrequency_tests() {
    
    val actual: Huffman = Feuille(0.50, 'e')
    val expected: Double = 0.50
    
    assertTrue(getFrequency(actual)==0.500)
    assertTrue(getFrequency(Noeud(0.2,Feuille(0.1,'a'),Feuille(0.1,'b')))==0.20)
    assertEquals(expected, getFrequency(actual), delta)


  }
  //---------------- TESTS insert ------------------\\
  @Test
  def insert_tests() {
    val actual = Noeud(0.2,Feuille(0.1,'a'),Feuille(0.1,'b')) 
    val expected = Noeud(0.2,Feuille(0.1,'a'),Feuille(0.1,'b'))::Feuille(0.40,'c') :: Nil
    
    assertEquals(expected, insert(actual, Feuille(0.40, 'c') :: Nil))
    assertEquals(actual::Nil, insert(actual, Nil))


  }

  
  //---------------- TESTS triSelonFrequence ------------------\\
  @Test
  def triSelonFrequence_tests() {
    
    assertEquals(Nil, triSelonFreq(Nil))
    assertEquals(Feuille(1.0, 'a') :: Nil, triSelonFreq(Feuille(1.0, 'a') :: Nil))
    assertEquals(Feuille(0.10, 'a') :: Feuille(0.13, 'b') :: Feuille(0.22, 'c') :: Nil, triSelonFreq(Feuille(0.22, 'c') :: Feuille(0.13, 'b') :: Feuille(0.10, 'a') :: Nil))
  }
  
  //---------------- TESTS uneFusion ------------------\\
  @Test
  def uneFusion_tests() {
     val left: Huffman = Noeud(0.2,Feuille(0.1,'a'),Feuille(0.1,'b'))
     val right: Huffman = Noeud(0.8,Feuille(0.4,'a'),Feuille(0.4,'b'))

    assertTrue(uneFusion(Feuille(0.71,'a')::Feuille(0.01,'b')::Nil)==(Noeud(0.72,Feuille(0.71,'a'),Feuille(0.01,'b'))::Nil))
    assertTrue(uneFusion(left :: right ::Nil)==(Noeud(1.0,left,right)::Nil))

    
  }

  //---------------- TESTS fusion ------------------\\
  @Test
  def fusion_tests() {
   
    val actual: List[Huffman] = Feuille(0.84, 'a') :: Feuille(0.10, 'b') :: Feuille(0.06, 'c') :: Nil
    val expected: Huffman = Noeud(1.0, Noeud(0.16, Feuille(0.06, 'c'), Feuille(0.10, 'b')), Feuille(0.84, 'a'))
    
    assertEqualsHuffman(Feuille(1.0, 'a'), fusion(Feuille(1.0, 'a') :: Nil), delta)
    assertEqualsHuffman(expected, fusion(actual), delta)
  }

  //---------------- TESTS codeHuffman ------------------\\
  @Test
  def codeHuffman_tests() {
    
    val actual = ('a', 0.54) :: ('b', 0.23) :: ('c', 0.23) :: Nil
    val expected = Noeud(1.0, Noeud(0.46, Feuille(0.23, 'b'),Feuille(0.23,'c')),Feuille(0.54, 'a'))
    
    assertEqualsHuffman(Feuille(1.0, 'z'), codeHuffman(('z', 1.0) :: Nil), delta)
    assertEqualsHuffman(Noeud(0.5,Feuille(0.2, 'a'),Feuille(0.3, 'b')),codeHuffman(('a', 0.2)::('b',0.3)::Nil), delta)
    assertEqualsHuffman(expected, codeHuffman(actual), delta)


  }
  
  
    //---------------- TESTS occurrence ------------------\\
  @Test
  def tests_occurence() {
    
    assertTrue(occurrence('p',Nil)==0)
    assertTrue(occurrence('c','b'::'a'::'c'::'b'::Nil)==1)
  }

  //---------------- TESTS removeChar ------------------\\
  @Test
  def tests_removeChar() {
    
     assertEquals('b'::'a'::'c'::'b'::Nil, removeChar('p', 'b'::'a'::'c'::'b'::Nil))
     assertEquals('b'::'c'::'d'::Nil, removeChar('a', 'b'::'a'::'c'::'d'::Nil))
  }
  //---------------- TESTS sansDoublon ------------------\\
  @Test
  def tests_sansDoublon() {
    
     assertEquals('b'::'a'::'c'::Nil, sansDoublon('b'::'a'::'c'::'b'::Nil))
     assertEquals('y'::'a'::'c'::'b'::Nil, sansDoublon('y'::'a'::'c'::'a'::'b'::'y'::'y'::Nil))
  }

   //---------------- TESTS get_freq_occurence ------------------\\
  @Test
  def tests_get_freq_occurence() {
      
      val actual: List[Char] = 'b'::'a'::'c'::'b'::Nil
      val expected: Double = 0.25
  
  assertEquals(expected, get_freq_occurence('c',actual), delta)
  assertEquals(0, get_freq_occurence('z',actual), delta)
  assertEquals(0.5, get_freq_occurence('b',actual), delta)
  assertEquals(1.0, get_freq_occurence('a','a'::'a'::'a'::'a'::'a'::Nil), delta)
  }
  
  //---------------- TESTS get_couple ------------------\\
  @Test
  def tests_get_couple() {
    
      
      val actual: List[Char] = 'b'::'a'::'c'::'b'::Nil
      val expected: (Char, Double) = ('b', 0.5)
  
  assertEquals(expected, get_couple('b',actual))
  assertEquals(('z', 0.0), get_couple('z',actual))
  assertEquals(('c',0.25), get_couple('c',actual))
  assertEquals(('a',1.0), get_couple('a','a'::'a'::'a'::'a'::'a'::Nil))
  
  }
  
  
  //---------------- TESTS analyseFrequences ------------------\\
  @Test
  def test_analyseFrequences() {
     val delta: Double = 0.01
     val actual: List[Char] = 'b'::'a'::'c'::'b'::Nil
     val expected = ('b',0.5)::('a', 0.25)::('c',0.25)::Nil
     
     assertEquals(expected, analyseFrequences("bacb"))
     assertEquals(Nil, analyseFrequences(""))
     assertEquals(('a', 1.0)::Nil, analyseFrequences("aaaaaaa"))
  }

  
  
   //---------------------------------------------------\\
   //__________________APPLICATION V3 ___________________\\
   //-----------------------------------------------------\\
  
  //---------------- TESTS descriptionHuffman ------------------\\
  @Test
  def DescriptionHuffman_tests() {
    
    val tree = Noeud(1.00, Feuille(0.5, 'a'), Noeud( 0.5, Feuille(0.25, 'b'), Feuille(0.25, 'c')))

    val expected = "1" + "0" + vers16Bits("a") + "1" + "0" + vers16Bits("b") + "0" + vers16Bits("c")

    assertEquals(expected, descriptionHuffman(tree))
    assertEquals("0" + vers16Bits("a"), descriptionHuffman(Feuille(0.0, 'a')))
    
  }
  
  //---------------- TESTS drop ------------------\\
  @Test
  def drop_tests() {
    
    assertEquals(Nil, drop(Nil,0))
    assertEquals(Nil, drop(Nil,59))
    assertEquals(Nil, drop(One :: Nil,1))
    assertEquals(Zero::Nil, drop(One :: Zero :: Nil,1))
    assertEquals(Nil, drop(One :: Zero :: Nil,36))
    assertEquals(One :: Zero :: Nil, drop(One :: Zero :: One :: One :: Zero :: Nil,3))

  }
  //---------------- TESTS grab ------------------\\
  @Test
  def grab_tests() {
    
    assertEquals(Nil, grab(Nil,0))
    assertEquals(Nil, grab(Nil,21))
    assertEquals(One :: Nil, grab(One :: Zero :: Nil,1))
    assertEquals(Zero::One::One::Zero::Nil, grab(Zero::One::One::Zero::One::Zero::Nil,4))
    assertEquals(One :: Zero :: Nil, grab( One :: Zero :: Nil,36))
  }
  
  //---------------- TESTS lireDescription ------------------\\
  @Test
  def lireDescription_tests() {
    
    assertEquals((Feuille(0.0, 'k'), Nil), lireDescription(stringToListBit("0" + vers16Bits("k"))))

    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'r'), Feuille(0.0, 'n')), Nil),
      lireDescription(stringToListBit("10" + vers16Bits("r") + "0" + vers16Bits("n"))))
      
    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'r'), Feuille(0.0, 'n')), One :: One :: Zero :: One :: Zero :: Nil),
      lireDescription(stringToListBit("10" + vers16Bits("r") + "0" + vers16Bits("n") + "11010")))
      
    assertEquals(
      (Noeud(0.0, Feuille(0.0, 'x'), Noeud(0.0, Feuille(0.0, 'y'), Feuille(0.0, 'z'))),  One :: One :: Zero :: One :: Zero :: Nil),
      lireDescription(stringToListBit("10" + vers16Bits("x") + "10" +
        vers16Bits("y") + "0" + vers16Bits("z") + "11010")))
  }
  
  //---------------- TESTS decodeV3 ------------------\\
  @Test
  def decodeV3_tests() {
    
    assertEquals("hhaee", decode("10" + vers16Bits("a") + "10" + vers16Bits("e") + "0" + vers16Bits("h") + "111101010"))
    
    assertEquals("f", decode("0" + vers16Bits("f")))
    
    assertEquals("", decode("10" + vers16Bits("r") + "0" + vers16Bits("n")))
    
    assertEquals("rnrn", decode("10" + vers16Bits("r") + "0" + vers16Bits("n") + "0101"))    
    
    
   
    
    
  }
  

  

   
  // Pour écrire des tests unitaires qui comparent des Double,
  // il sera nécessaire d'utiliser l'assertion
  // def assertEquals(expected: Double, actual: Double, delta: Double): Unit
  // où le 3ème paramètre delta est la précision à utiliser.
  // On donne un exemple de test ci-dessous démontrant son utilisation.

  /**
   * Test qui illustre l'utilisation de l'assertion
   * assertEquals sur les Double
   */
  @Test
  def testEgaliteDouble() {
    // Exemple de problème d'imprécision sur les Double
    assertNotEquals(2.97, 2.8 + 0.17)

    // Problème résolu en utilisant une assertion spéciale à 3 paramètres Double
    assertEquals(2.97, 2.8 + 0.17, 0.01)

    // Selon la précision utilisée, deux Double sont soient égaux, soit différents
    assertEquals(0.001, 0.002, 0.001)
    assertNotEquals(0.001, 0.002, 0.0001)
  }

  // On vous fournit également une fonction auxiliaire permettant de
  // définir des tests pour contrôler l'"égalité" de deux arbres de Huffman.
  // Vous pouvez l'utiliser dans vos tests comme n'importe quelle autre assertion.

  /**
   * Vérifie que h1 et h2 sont égaux, en comparant les fréquences Double
   * avec la précision d. Echoue si ce n'est pas le cas.
   * @param h1 un arbre de Huffman
   * @param h2 un arbre de Huffman
   * @param d un double
   */
  def assertEqualsHuffman(h1: Huffman, h2: Huffman, d: Double): Unit = {
    (h1, h2) match {
      case (Feuille(f1, c1), Feuille(f2, c2)) => {
        assertEquals(f1, f2, d);
        assertEquals(c1, c2)
      }
      case (Noeud(f1, h11, h12), Noeud(f2, h21, h22)) =>
        assertEquals(f1, f2, d);
        assertEqualsHuffman(h11, h21, d);
        assertEqualsHuffman(h12, h22, d)
      case _ => fail("Les deux arbres n'ont pas la même structure")
    }
  }

}
