import org.scalatest.FunSuite;

class TestGrille extends FunSuite {

  /**
   * Test pour une grille complète en 3x3
   */
    test("Grille 3x3 complète"){
      val l = List("XXX",
                   "XXX",
                   "XXX");
      val executionRes = JeuDeLaVie.chainesToGrille(l)

      val goodRes = List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2))

      assert(executionRes == goodRes)
    }

  /**
   * Test pour une grille vide
   */
    test("Grille 3x3 vide"){
      val l = List("  ",
                   "  ",
                   "  ")
      val executionRes = JeuDeLaVie.chainesToGrille(l);

      val goodRes = List()

      assert(executionRes == goodRes)
    }

  /**
   * Test pour une grille quelconque
   */
    test("Grille 3x3 quelconque"){
      val l = List(" XX",
                   " XX",
                   "X  ")
      val executionRes = JeuDeLaVie.chainesToGrille(l);

      val goodRes = List((0,1), (0,2), (1,1), (1,2), (2,0))

      assert(executionRes == goodRes)
    }

  /**
   * Test pour une grille non carré
   */
    test("Grille non carré") {
      val l = List(" XX XXX",
        "",
        "X X ")
      val executionRes = JeuDeLaVie.chainesToGrille(l);

      val goodRes = List((0, 1), (0, 2), (0, 4), (0, 5), (0, 6), (2, 0), (2, 2))

      assert(executionRes == goodRes)
    }

  /**
   * Test pour une grille avec d'autres caractères
   */
    test("Grille autres caractères"){
      val l = List(" XX5XXX",
                   "",
                   "XAXl ")
      val executionRes = JeuDeLaVie.chainesToGrille(l);

      val goodRes = List((0,1), (0,2), (0,4), (0,5), (0,6),(2,0), (2,2))

      assert(executionRes == goodRes)
    }

}
