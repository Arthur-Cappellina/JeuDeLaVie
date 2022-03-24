object Alternatives {
  def naitJDLV(nbVoisines:Int):Boolean = (nbVoisines==3)

  def survitJDLV(nbVoisines: Int): Boolean = (nbVoisines==2||nbVoisines==3)

  def naitFred(nbVoisines: Int): Boolean = (nbVoisines % 2 == 0)

  def survitFred(nbVoisines: Int): Boolean = naitFred(nbVoisines)

}
