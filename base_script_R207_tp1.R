##########################################################################
## Installation de package et chargement du package, si nécessaire #######
##########################################################################
if (!require("igraph")) {
  install.packages("igraph")  
}
library(igraph)
if (!require("igraphdata")) {
  install.packages("igraphdata")
}
library(igraphdata)

##########################################################################
############################### Pré requis  ##############################
##########################################################################

#' Création d'une matrice vide de dimensions données.
#'
#' @param nbLignes 
#' @param nbColonnes 
#'
#' @return une matrice vide de dimensions données. 
#' @export
#'
#' @examples
#' creationMatriceVide(nbLignes=3,nbColonnes=3)
creationMatriceVide <- function( nbLignes, nbColonnes){
  return(matrix(nrow= nbLignes,ncol = nbColonnes ))
}

#' Création d'une matrice 
#'
#' @param valeurs 
#' @param nbLignes 
#' @param nbColonnes 
#'
#' @return Une matrice aux dimensions demandées mais sans valeurs si le vecteur 
#' valeurs n'a pas assez de valeurs. 
#' @export
#'
#' @examples
creationMatrice <- function(valeurs, nbLignes, nbColonnes){
  if(length(valeurs)==(nbLignes*nbColonnes)){
    return(matrix(data=valeurs, nrow = nbLignes, ncol = nbColonnes, byrow = TRUE))
  }else{
    print("Le nombre de valeurs ne correspond pas aux dimensions de la matrice. ")
    print("Veuillez vérifier les informations saisies." )
    return(creationMatriceVide(nbLignes,nbColonnes))    
  }
}

#' Additionne deux matrices
#'
#' @param matriceA 
#' @param matriceB 
#'
#' @return matriceA + matriceB
#' @export
#'
#' @examples
additionDeDeuxMatrices <- function(matriceA, matriceB){
  if (all(dim(matriceA)==dim(matriceB))) {
    matriceResultat <- creationMatriceVide(nrow(matriceA), ncol(matriceA))
    for (compteurLigne in seq_len(nrow(matriceA))) {
      for (compteurColonne in seq_len(ncol(matriceA))) {
        matriceResultat[compteurLigne,compteurColonne] <- matriceA[compteurLigne, compteurColonne]+matriceB[compteurLigne,compteurColonne]
      }      
    }
    return(matriceResultat)
  }else {
    print("Erreur les deux matrices n'ont pas les mêmes dimensions.")
    print("L'opération est donc impossible.")
    return()
  }
}



#' Produit de deux matrices
#'
#' @param matriceA 
#' @param matriceB 
#'
#' @return matriceA * matriceB
#' @export
#'
#' @examples
produitDeDeuxMatrices <- function(matriceA, matriceB){
  if (ncol(matriceA)== nrow(matriceB)) {
    matriceResultat <- creationMatriceVide(nbLignes = nrow(matriceA), nbColonnes = ncol(matriceB))
    for (compteurLigne in seq_len(nrow(matriceA))) {
      for (compteurColonne in seq_len(ncol(matriceB))) {
        somme <-0
        for (compteur in seq_len(ncol(matriceA))) {
          somme  <- somme + matriceA[compteurLigne, compteur] * matriceB[compteur, compteurColonne]      
        }
        matriceResultat[compteurLigne, compteurColonne] <- somme
      }      
    }
    return(matriceResultat)
  }else{
    print("Erreur les matrices n'ont pas les bonnes dimensions")
    return()
  }
}


#' Permet de faire la conversion d'une matrice quelconque en matrice boolénne
#' en prenant le postulat que si une valeur est différente de 0, c'est qu'en booléen 
#' cette valeur vaut 1. 
#'
#' @param uneMatrice 
#'
#' @return une matrice booleenne 
#' @export
#'
#' @examples
#' matriceBoolenneA <- convertionMatriceEnMatriceBoolenne(matriceA)
#' 
convertionMatriceEnMatriceBoolenne <- function( uneMatrice){
  return(ifelse(uneMatrice ==0 , 0 , 1))
  
}


additionDeDeuxMatricesBooleennes <- function(matriceA, matriceB){
  
  matriceResultat <- additionDeDeuxMatrices(matriceA = matriceA, matriceB = matriceB)
  return(convertionMatriceEnMatriceBoolenne(matriceResultat))
}



produitDeDeuxMatricesBooleennes <- function(matriceBoolenneA, matriceBoolenneB){
  matriceResultat <- produitDeDeuxMatrices(matriceBoolenneA, matriceBoolenneB)
  return(convertionMatriceEnMatriceBoolenne(matriceResultat))
}

library(igraph)

# (a) Création du graphe
# Liste d'arêtes
edges <- c("Alice", "Bob", "Alice", "Claire", "Alice", "David",
           "Bob", "Claire", "Bob", "Elise",
           "Claire", "David", "Claire", "Elise")
g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE),
                         directed = FALSE)
plot(g)

# Matrice d'adjacence
adj_matrix <- matrix(c(0, 1, 1, 1, 0,
                       1, 0, 1, 0, 1,
                       1, 1, 0, 1, 1,
                       1, 0, 1, 0, 0,
                       0, 1, 1, 0, 0), nrow = 5, byrow = TRUE)
g2 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected",
                                  vertices = c("Alice", "Bob", "Claire",
                                               "David", "Elise"))
plot(g2)

# Data frame
edges_df <- data.frame(from = c("Alice", "Alice", "Alice", "Bob", "Bob",
                                "Claire", "Claire", "Claire"),
                       to = c("Bob", "Claire", "David", "Claire", "Elise",
                              "David", "Elise", "Bob"))
g3 <- graph_from_data_frame(edges_df, directed = FALSE)
plot(g3)

# Liste d'adjacence littérale
g4 <- graph_from_literal(Alice - Bob - Claire - David, Bob - Elise,
                         Claire - Elise, David, Elise)
plot(g4)

# (b) Nombre total d'amitiés
ecount(g1)

# (c) Ajout de François
g5 <- add_vertices(g1, 1, name = "François")
g5 <- add_edges(g5, c("François", "Bob", "François", "David",
                      "François", "Elise"))
plot(g5)

# (d) Degré moyen
mean(degree(g5))

# (e) Suppression des relations d'Alice
g6 <- delete_edges(g5, c("Alice|Bob", "Alice|Claire"))
plot(g6)