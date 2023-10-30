# This function computes the value of Extremal index 
#estimatives using a Negative Binomial regression model.
#The functions inside need the  robNB package
#depends on X, a matrix with N columns=number of samples, each sample of dimension n= number of rows.
#and a high quantile  or vector of high quantiles q
#example apply(X,FUN=Teta_NB,MARGIN=2,q=.8),

library(robNB)
Teta_NB=
  function(x,q){
    ordered= sort(x)
    n=length(x)
    thresholds = ordered[floor(q*n)]
    b=matrix(0,length(q),n)
    teta=NULL
    run = 0
    runs=NULL
    keepTeta=NULL
    for (u in thresholds) {
      run=run+1
      b[run,]=floor((sign(x-u)+1)/2)#sequence of zeros (non exceedances) and ones (exceedances)
      runs=rle(b[run,])#list with counts of consecutive zeros and ones  
      y=N.Exc.clust=runs$lengths[runs$values==1]
      #teta[run]=1/mean(N.Exc.clust)#reciprocal of sample mean cluster size
      clust=y[y>1]#clusters with more than one exeedance
      n2=length(y[y>1])#number of clusters with more than one exeedance
      M.cov=matrix(rep(1,n2),byrow=FALSE,nrow=n2)
      NB.fit=nb.glm.rob(clust,M.cov)
      
      teta[run]=exp(-NB.fit$coef[2])#exp(-intercept) of the regression model  
    }
    keepTeta=c(keepTeta,teta)
  }
