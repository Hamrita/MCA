topsis=function(x = NULL, poids = NULL, crit = NULL){
  poids <- poids/sum(poids)
  N <- matrix(nrow = nrow(x), ncol = ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      N[i,j] <- x[i,j] / sqrt(sum(x[,j] ^ 2))
    }
  }
  W=diag(poids)
  V=N%*%W
  colnames(V)=colnames(x)
  rownames(V)=rownames(x)
  cat("\n La Matrice normalisée et ponderée\n\n")
  print(V)
  u <- as.integer(crit == "+") * apply(V, 2, max) + 
    as.integer(crit == "-") * apply(V, 2, min)
  l <- as.integer(crit == "-") * apply(V, 2, max) + 
    as.integer(crit == "+") * apply(V, 2, min)
  distance_u =function(x){
    sqrt(sum((x - u) ^ 2))
  }
  distance_l =function(x){
    sqrt(sum((x - l) ^ 2))
  }
  du <- apply(V, 1, distance_u)
  dl <- apply(V, 1, distance_l)
  cat("\n E+ et E- \n\n")
  print(cbind("E+"=du,"E-"=dl))
  score <- dl/(dl+du)
  cat("\n\n")
  return(data.frame(row.names(x), score = score, rank = rank(-score)))
}

# Exemple

xx=matrix(c(6, 5, 5, 5, 6, 7, 6, 6, 7, 7, 5, 6, 7, 7, 5, 7,
            5, 5, 4 ,4),nc=4, byrow = T)
colnames(xx)=c("Style","Fiabilité","Consommation","Prix")
rownames(xx)=c("Renault","Golf","Ford Focus","Peugeot 4070", "Picasso")

# normalisation: lineaire euclidienne

normalisee=euclidienne(xx,rep("max",4))
colnames(normalisee)=colnames(xx)
rownames(normalisee)=rownames(xx)

topsis(normalisee,c(0.1,0.4,0.2,0.3), c("+","+","-","-"))