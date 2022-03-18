import org.scalatest.FunSuite;

class TestMoteur extends FunSuite{

  /**
   * Test si l'on récupère bien les cases d'une case normale (1,1)
   */
  test("voisines case normale"){
    val executionRes = JeuDeLaVie.voisine8(1,1)

    val goodRes = List((0,0), (0,1), (0,2), (1,0), (1,2), (2,0), (2,1), (2,2))

    assert(executionRes == goodRes)
  }

  /**
   * Test si l'on récupère bien les cases d'une case au centre (0,0)
   */
  test("voisines case en (0,0)"){
    val executionRes = JeuDeLaVie.voisine8(0,0)

    val goodRes = List((-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1))

    assert(executionRes == goodRes)
  }

  /**
   * Test si l'on récupère bien les cases d'une case négative (-2,-2)
   */
  test("voisines case négative"){
    val executionRes = JeuDeLaVie.voisine8(-2,-2)

    val goodRes = List((-3,-3), (-3,-2), (-3,-1), (-2,-3), (-2,-1), (-1,-3), (-1,-2), (-1,-1))

    assert(executionRes == goodRes)
  }

  /**
   * Test sur des cellules survivantes normal, quelqu'une doivent rester
   */
  test("Cellules survivantes normales"){
    val l = List(" XX",
                  "  X",
                  "XXX");
    val executionRes = JeuDeLaVie.survivantes(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List((0,1), (0,2), (2,1), (2,2))

    assert(executionRes == goodRes)
  }

  /**
   * Test avec une grille 3x3 avec une cellule, ainsi aucune ne doit survivre
   */
  test("Aucune survit"){
    val l = List(" X ",
                  "  ",
                  "   ");
    val executionRes = JeuDeLaVie.survivantes(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List()

    assert(executionRes == goodRes)
  }

  /**
   * Test des candidates pour une grille normale
   */
  test("Candidates grille normale"){
    val l = List("X X",
                  "  X",
                  "  X");

    val executionRes = JeuDeLaVie.candidates(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List((0,1), (1,0), (1,1), (2,1))

    assert(executionRes == goodRes)
  }

  /**
   * Test pour une grille pleine grille vide
   */
  test("Grille vide"){
    val l = List("   ",
                "   ",
                "   ");

    val executionRes = JeuDeLaVie.candidates(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List()

    assert(executionRes == goodRes)
  }

  /**
   * Test pour une grille pleine
   */
  test("Candidates grille pleine"){
    val l = List("XXX",
      "XXX",
      "XXX");

    val executionRes = JeuDeLaVie.candidates(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List()

    assert(executionRes == goodRes)
  }

  /**
   * Test naissance grille normale
   */
  test("Naissances pour une grille normale"){
    val l = List("X X",
                  "  X",
                  "X ");

    val executionRes = JeuDeLaVie.naissances(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List((0,1))

    assert(executionRes == goodRes)
  }

  /**
   * Test naissance grille normale
   */
  test("Aucune naissance"){
    val l = List("X X",
                  "  ",
                  "X X");

    val executionRes = JeuDeLaVie.naissances(JeuDeLaVie.chainesToGrille(l))

    val goodRes = List()

    assert(executionRes == goodRes)
  }
}
