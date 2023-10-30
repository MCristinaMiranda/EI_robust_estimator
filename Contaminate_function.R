contaminate=function(x,q,alfa,d){
  #x vector of observations
  #q high quantile
  #alfa % of contamination
  #d high size of clusters (outlier)
  #different values of d must be used accordingly to the theta values
  ss=NULL
  q=quantile(x,q)
  ss[1]=sample(1:150,1)
  n1=round(alfa*length(x)/10,0)
  for (i in 2:n1){
    ss[i]=ss[(i-1)]+200
    x[ss[i]:(ss[i]+(d-1))]=q+mean(x)/2}
  x
}
####################################