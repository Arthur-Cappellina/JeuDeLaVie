object JeuDeLaVie {

  type Grille = List[(Int, Int)];

  /**
   * Methode permettant de transformer une liste de chaines de caractères en une grille
   * @param l la liste de chaines de caracteres
   * @return la grille correspondantes
   */
  def chainesToGrille(l:List[String]): Grille = l match{
    case Nil => Nil
    case t:+q => if(l.length <= 1) chainesToGrilleAux(q, 0, l.length-1)
                  else chainesToGrille(t)++chainesToGrilleAux(q, 0, l.length-1)
  }

  /**
   * Methode auxiliaire a la méthode chaineToGrille, elle permet de transformer d'ajouter une chaine de caractère a une grille
   * @param s la chaine que l'on souhaite ajouter a la grille
   * @param col le numéro de colonne du caractère courant traité
   * @param ligne le numéro de la ligne correspondante a la chaine
   * @return la grille a ajouter a la grille générale
   */
  def chainesToGrilleAux(s: String, col: Int, ligne: Int): Grille = {
    if(col >= s.length-1)
      if(s.length > 0 && s.charAt(col) == 'X') (ligne, col)::Nil else Nil
    else
      if(s.charAt(col) == 'X') (ligne, col)::chainesToGrilleAux(s, col+1, ligne) else chainesToGrilleAux(s, col+1, ligne)
  }

  /**
   * Methode permettant de récupérer le coin supérieur gauche a une grille
   * @param g la grille en question
   * @return la case
   */
  def coinSuperieurGauche(g: Grille) : (Int, Int) = {
    def ligneMin(g: Grille): Int = g match {
      case t::q => if(ligneMin(q) > t._1) t._1 else ligneMin(q)
      case Nil => 1000
    }
    def colMin(g: Grille): Int = g match {
      case t::q => if(colMin(q) > t._2) t._2 else colMin(q)
      case Nil => 1000
    }
    (ligneMin(g), colMin(g))
  }
  /**
   * Methode permettant de récupérer le coin inférieur droit a une grille
   * @param g la grille en question
   * @return la case
   */
  def coinInferieurDroit(g: Grille) : (Int, Int) = {
    def ligneMax(g: Grille): Int = g match {
      case t::q => if(ligneMax(q) < t._1) t._1 else ligneMax(q)
      case Nil => -1000
    }
    def colMax(g: Grille): Int = g match {
      case t::q => if(colMax(q) < t._2) t._2 else colMax(q)
      case Nil => -1000
    }

    (ligneMax(g), colMax(g))
  }

  /**
   * Methode permettant d'afficher une grille
   * @param g la grille a afficher
   */
  def afficherGrille(g: Grille): Unit = {

    def afficherGrilleAux(grilleVierge: Grille, grille: Grille, colMax: Int): Unit = grilleVierge match{
      case Nil => print("")
      case t::q =>
        if(!grille.contains(t)) {
          if(t._2 == colMax) print(" \n") else print(" ")
          afficherGrilleAux(q, grille, colMax)
        } else {
          if(t._2 == colMax) print("x\n") else print("x")
          afficherGrilleAux(q, grille, colMax)
        }
    }
    afficherGrilleAux(creerGrilleVierge(coinSuperieurGauche(g), coinInferieurDroit(g), coinSuperieurGauche(g)._2), g, coinInferieurDroit(g)._2)
  }

  /**
   * Methode permettant de créer une grille de taille variable a partir de 2 coins
   * @param caseCourante au déport, le coin supérieur gauche, mais est utilisé pour créer chaque case
   * @param coinInferieurDroit la dernière case de la grille
   * @param minColonne le numéro de colonne minimum
   * @return la nouvelle grille vierge
   */
  def creerGrilleVierge(caseCourante: (Int, Int), coinInferieurDroit: (Int, Int), minColonne: Int) : Grille = {
    if(caseCourante == coinInferieurDroit) caseCourante::Nil
    else
      if(caseCourante._2 == coinInferieurDroit._2) (caseCourante._1, caseCourante._2)::creerGrilleVierge((caseCourante._1+1,minColonne), coinInferieurDroit, minColonne)
      else caseCourante::creerGrilleVierge((caseCourante._1,caseCourante._2+1), coinInferieurDroit, minColonne)
  }

  /**
   * Methode permettant de récupérer les cases autour  d'une case
   * @param l la ligne de la case
   * @param c la colonne
   * @return la liste des cases autour de celle demandé
   */
  def voisine8(l: Int, c:Int): List[(Int, Int)] = {
    def voisine8Aux(caseCourante: (Int, Int), caseFinale: (Int, Int)) : List[(Int, Int)] = {
      if(caseCourante == caseFinale) caseCourante::Nil
      else
          if(caseCourante._2 == c+1) caseCourante::voisine8Aux((caseCourante._1+1, c-1), caseFinale)
          else if (caseCourante != (l,c)) caseCourante::voisine8Aux((caseCourante._1, caseCourante._2+1), caseFinale)
               else voisine8Aux((caseCourante._1, caseCourante._2+1), caseFinale)
    }
    voisine8Aux((l-1, c-1), (l+1, c+1))
  }

  /**
   * Methodes permettant de déterminer le nombre de cases communes entre les cases voisines d'une case et les case d'une grille.
   *    Utile pour savoir si une cellule doit survivre ou non
   * @param caseChoisie la case ou l'on vérifie ses voisines
   * @param g la grille sur laquelle on fait les vérifications
   * @return la listes des cases communes
   */
  def cellulesCommunes(caseChoisie: (Int, Int), g: Grille): Int = {
    (voisine8(caseChoisie._1, caseChoisie._2) filter (cellule => g.contains(cellule))).length
  }

  /**
   * Methode renvoyant la grille des cellules qui survivent à l'issu de l'étape
   * @param g la grille des cellules vivantes
   * @return la grilles des cellules encores en vie
   */
  def survivantes(g: Grille): Grille = {
    def survivantesAux(cellulesVivantes: Grille) : Grille = cellulesVivantes match{
      case Nil => Nil
      case t::q => if(cellulesCommunes(t, g) == 2 || cellulesCommunes(t, g) == 3) t::survivantesAux(q) else survivantesAux(q)
    }
    survivantesAux(g)
  }

  /**
   * Methodes permettant de récuperer toutes les cellules mortes d'une grille
   * @param g la grille des cellules vivantes
   * @return la grille des cellules mortes
   */
  def candidates(g: Grille) : Grille = {
    def candidatesAux(cellulesVivantes: Grille) : Grille = cellulesVivantes match{
      case Nil => Nil
      case t::q => cellulesMortes(voisine8(t._1, t._2), g, coinSuperieurGauche(g), coinInferieurDroit(g))++candidatesAux(q)
    }
    candidatesAux(g).distinct
  }

  /**
   * Methode permettant de récuperer la liste des cellules mortes voisines a une case
   * @param l la liste des cellules voisines a la case
   * @param g la grille des cellules vivantes
   * @param coinSupGauche le coin supérieur gauche de la grille
   * @param coinInfDroit le coin inférieur droit
   * @return la liste des cellules mortes d'une grille
   */
  def cellulesMortes(l: List[(Int, Int)], g: Grille, coinSupGauche: (Int, Int), coinInfDroit: (Int, Int)): Grille = l match{
    case Nil => Nil
    case t::q => if(!g.contains(t) && t._1 >= coinSupGauche._1 && t._2 >= coinSupGauche._2 && t._1 <= coinInfDroit._1 && t._2 <= coinInfDroit._2)
                       t::cellulesMortes(q, g, coinSupGauche, coinInfDroit)
                  else cellulesMortes(q, g, coinSupGauche, coinInfDroit)
  }

  def naissances(g: Grille) : Grille = {
    def naissanceAux(l: List[(Int, Int)]) : Int = l match {
      case Nil => 0
      case t::q => if(g.contains(t)) 1+naissanceAux(q) else naissanceAux(q)
    }
    candidates(g) filter (candidate => naissanceAux(voisine8(candidate._1, candidate._2)) == 3)
  }

  def jeuDeLaVie(init: Grille, n: Int): Unit = {
    if(n == 0) afficherGrille(init) else {
      afficherGrille(init)
      jeuDeLaVie(survivantes(init)++naissances(init).distinct, n-1)
    }
  }

  def main(args: Array[String]): Unit = {
    val l = List("X X",
                 "  ",
                 "X X");
    jeuDeLaVie(chainesToGrille(l), 1)
  }
}
