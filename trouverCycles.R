trouverCycles = function(graphe) {
  Cycles = NULL
  for(sommet1 in V(graphe)) {
    #Il faut que le degré entrant soit non nul sinon pas de boucle avec ce sommet
    if(degree(graphe, sommet1, mode="in") >= 1) {
      listeVoisins= neighbors(graphe, sommet1, mode="out")
      # On traite les sommets rangés après pas ceux avant car, si cycle/circuit, on les a déjà traités.
      listeVoisins = listeVoisins[listeVoisins > sommet1]
      for(sommet2 in listeVoisins) {
        cycleTemporaire = lapply(all_simple_paths(graphe, sommet2,sommet1, mode="out"), function(p) c(sommet1,p))
        # On élimine les cycles de longueur 2
        cycleTemporaire = cycleTemporaire[which(sapply(cycleTemporaire, length) > 3)]
        cycleTemporaire = cycleTemporaire[sapply(cycleTemporaire, min) == sapply(cycleTemporaire, `[`, 1)]
        Cycles  = c(Cycles, cycleTemporaire)
      }
    }
    else {}
  }
  Cycles
}

