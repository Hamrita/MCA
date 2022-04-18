#=================================================
#  Fonctions de normalisation
#=================================================

# lineaire1: 
lineaire1=function(x,crit){
  n=nrow(x); p=ncol(x)
  z=matrix(0,n,p)
  for(j in 1:p){
    if(crit[j]=="max"){
      max_value=max(x[,j])
      z[, j] = x[, j] / max_value
    }else{
      min_value=min(x[,j])
      z[, j] =min_value/ x[, j]
    }
  }
  return(z)
}
# exemple
# x=matrix(c(17000, 22000, 19000, 26000, 15000 , 12, 32, 16, 64, 8),nc=2)
# lineaire1(x,c("min","max"))

# lineaire2

lineaire2=function(x,crit){
  z=matrix(0,nc=NCOL(x),nr=NROW(x))
  M=apply(x, 2, "max")
  m=apply(x,2,"min")
  den=M-m
  for(i in 1:NCOL(x)){
  if(crit[i]=="max"){
    z[,i]=(x[,i]-m[i])/den[i]
  }else{
    z[,i]=(M[i]-x[,i])/den[i]
  }
  }
  return(z)
}

# Exemple
# x=matrix(c(17000, 22000, 19000, 26000, 15000 , 12, 32, 16, 64, 8),nc=2)
# lineaire2(x,c("min","max"))

euclidienne=function(x,crit){
z=matrix(0,nc=NCOL(x), nr=NROW(x))  
den=sqrt(apply(x^2,2,"sum"))
for(i in 1:NCOL(x)){
  if(crit[i]=="max"){
  z[,i]=x[,i]/den[i]
  }else{
    z[,i]=1-(x[,i]/den[i])
  }
}
return(z)
}


