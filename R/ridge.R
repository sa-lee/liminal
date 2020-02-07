# ######
# ######  Code for FIG. 9 and 10
# ######
#
#
# library(mvtnorm)
#
# ess = function(X,h){
#   ## effective sample size
#   n = nrow(X)
#   D = ncol(X)
#   p = kernel(X,X,h)
#   m = n*p/(dnorm(0,0,h)*dnorm(0,0,h))
#   return(m)
# }
#
#
# ClusterFilter = function(X,m=20,b=.2){
#   ### m = min size of cluster
#   ### b = distance needed to join points in a cluster
#   n = nrow(X)
#   d = dist(X)
#   C = hclust(d,method="single")
#   o = cutree(C,h=b)
#   size = table(o)
#   I = (1:n)[size[o] >= m]
#   Y = X[I,]
#   return(Y)
# }
#
#
#
# bandwidth = function(X){
#   n = nrow(X)
#   d = ncol(X)
#   S = sqrt(mean(apply(X,2,var)))
#   h = S*(4/(d+2))^(1/(d+4))*n^(-1/(d+4))
#   return(h)
# }
#
#
#
# deriv.fun  = function(pt,dat,h) {
#   d       = ncol(dat)
#   sqdis   = rowSums(t(pt-t(dat))^2)
#   # square distance of point from data
#   wi      = (2*pi*h^2)^(-d/2)*exp(-sqdis/(2*h^2))
#   fhatx   = mean(wi)
#   msx     = colMeans(wi*dat)/(fhatx)
#   grad    = (pt - msx)/(h^2)
#   crosspr = matrix(0, nrow=d, ncol=d)
#   for(j in 1:d){
#     crosspr[,j]= colMeans(wi*dat[,j]*dat)/(h^4*fhatx)
#   }
#   hess = crosspr - (msx %*% t(msx))/(h^4) -
#     diag(1/h^2, nrow=d)
#   return(list(grad=grad,hess=hess))
# }
#
#
#
#
# kernel = function(x,grid,h){
#   m = nrow(grid)
#   p = rep(0,m)
#   d = ncol(x)
#   sigma = h^2*diag(d)
#   for(i in 1:m){
#     p[i] = mean(dmvnorm(x,grid[i,],sigma))
#   }
#   return(p)
# }
#
#
#
#
#
#
# scms.one.point.fun   = function(pt,dat,h,d,Nsteps){
#   # subspace constrained mean shift
#   # input:
#   #   pt   one point in the space (D-dim vector)
#   #   dat  data points (nXD data matrix)
#   #   h   bandwidth (scalar)
#   #   d  dimension of ridge to be detect
#   # output:
#   #   subspace constrained mean shift (D-dim vector)
#
#   D = ncol(dat)
#   n = nrow(dat)
#   ptseq = matrix(0, nrow=Nsteps+1, ncol=D)
#   ptseq[1,] = pt
#
#   if(d > 0){
#     for(i in 1:Nsteps){
#       deriv   = deriv.fun(pt,dat,h)
#       grad    = deriv$grad
#       hessian = deriv$hess
#
#       msx     = pt - h^2*grad
#       grad    = grad/sqrt(sum(grad^2))
#       eig     = eigen(hessian, symmetric=TRUE)
#       eigvec  = eig$vectors
#       V       = eigvec[,-(1:d)]
#       pt = c(V %*% t(V) %*% (msx - pt)) + pt
#       ptseq[i+1,]= pt
#     }
#   }
#   if(d == 0){
#     sigma = h^2*diag(D)
#     for(i in 1:Nsteps){
#       w  = dmvnorm(dat,pt,sigma)
#       w  = w/sum(w)
#       pt = apply(dat*matrix(w,n,D),2,sum)
#       ptseq[i+1,]= pt
#     }
#   }
#   return(list(pt=pt, ptseq=ptseq))
# }
#
#
# scms = function(grid,dat,h,d,Nsteps){
#   path = list()
#   n = nrow(grid)
#   D = ncol(grid)
#   for(i in 1:n){
#     tmp      = scms.one.point.fun(grid[i,],dat,h,d,Nsteps)
#     grid[i,] = tmp$pt
#     path[[i]]= tmp$ptseq
#   }
#   return(list(grid=grid, path=path))
# }
#
#
#
# signature = function(lambda){
#   # Computes signatures from a vector of Hessian eigenvalues
#   #
#   # Parameters:
#   #   lambda -- a d-dimensional vector of eigenvalues
#   # Returns:
#   #   A d-dimensional numeric vector of signatures
#
#   lambda = rev(sort(lambda))
#   d = length(lambda)
#   S = rep(0,d)
#   S[1] = abs(lambda[1])*abs(lambda[1]/lambda[d])*(lambda[1] < 0)
#   prod.term = 1
#   for (j in 2:d){
#     prod.term = (1- abs(min(lambda[j-1],0)/lambda[d]))*prod.term
#     S[j] = abs(lambda[j])*abs(lambda[j]/lambda[d])*prod.term*(lambda[j]<0)
#   }
#   return(S)
# }
#
#
#
# Hessian.signature = function(pt,dat,h){
#   ### signature of Hessian at a single point pt
#   d       = ncol(dat)
#   sqdis   = rowSums(t(pt-t(dat))^2) # square distance of
#   # point from data
#   wi      = (2*pi*h^2)^(-d/2)*exp(-sqdis/(2*h^2))
#   fhatx   = mean(wi)
#   tmp     = deriv.fun(pt,dat,h)
#   tmp     = fhatx * (tmp$hess + tmp$grad %*% t(tmp$grad))
#   ttmp    = eigen(tmp,symmetric=TRUE)$values
#   S       = signature(ttmp)
#   return(list(S=S,lambdas=ttmp))
# }
#
#
#
# GetSignatures = function(Y,X,h,d){
#   ### Y = evaluation points
#   ### X = data
#   m = nrow(Y)
#   s = rep(0,m)
#   for(i in 1:m){
#     s[i] = Hessian.signature(Y[i,],X,h)$S[d+1]
#   }
#   return(s)
# }
#
#
# Plotit = function(X,h,Nsteps,m,b,avec){
#   D = ncol(X)
#   E = mean(ess(X,h))
#   n   = nrow(X)
#   allridges=list()
#
#   ran    = apply(X=X, MARGIN=2, FUN=range)
#   midran = (ran[2,] + ran[1,])/2
#   maxran= ran[2,] - ran[1,]
#   maxran= max(ran[2,] - ran[1,])
#   plran = rbind(midran - 0.52*maxran, midran + 0.52*maxran)
#
#
#   d    = 0
#   S    = GetSignatures(X,X,h,d)
#   tmpp = scms(X,X,h,d,Nsteps)
#   SCMS = tmpp$grid
#   S    = GetSignatures(SCMS,X,h,d)
#   a    = avec[d+1]
#
#   pdf("FIG10Left.pdf")
#   hist(S,breaks=100,col="black",
#        yaxt="n",xlab="",ylab="", main="",axes=FALSE)
#   abline(v=a, lwd=3,lty=2)
#   axis(side=1,at=0,cex.axis=3)
#   axis(side=1,at=20,cex.axis=3)
#   axis(side=1,at=40,cex.axis=3)
#   dev.off()
#
#
#   pdf("FIG10CenterLeft.pdf")
#   plot.stepfun(S,xlab="",ylab="",vertical=TRUE,do.points=FALSE,axes=FALSE)
#   abline(v=a,lwd=3,lty=2)
#   abline(h=0)
#   axis(side=1,at=0,cex.axis=3)
#   axis(side=1,at=20,cex.axis=3)
#   axis(side=1,at=40,cex.axis=3)
#   dev.off()
#
#   pdf("FIG9Center.pdf")
#
#   PTS = SCMS[S > a,]
#   k   = nrow(PTS)
#
#   if(k>5){Y  = ClusterFilter(PTS,m=m[d+1],b=b)
#   kk = nrow(Y)
#   allridges[[d+1]]=Y
#   plot(allridges[[1]], pch=20,lwd=5, col="red",xlim=plran[,1],
#        ylim=plran[,2], xlab="", ylab="",xaxt="n",yaxt="n")
#   if(kk > 0){
#     for(i in 1:nrow(Y)) {
#       dd = sqrt((Y[,1] - Y[i,1])^2 + (Y[,2] - Y[i,2])^2)
#       friends = matrix(Y[dd <= b, ], ncol=2)
#       lines(friends, col="blue", lwd=5)
#     }
#   }
#   }
#
#   dev.off()
#
#
#
#   d    = 1
#   S    = GetSignatures(X,X,h,d)
#   tmpp = scms(X,X,h,d,Nsteps)
#   SCMS = tmpp$grid
#   S    = GetSignatures(SCMS,X,h,d)
#   a    = avec[d+1]
#
#
#   pdf("FIG10CenterRight.pdf")
#   hist(S,breaks=100,col="black",
#        yaxt="n",xlab="",ylab="", main="",axes=FALSE)
#   abline(v=a, lwd=3,lty=2)
#   axis(side=1,at=0,cex.axis=3)
#   axis(side=1,at=25,cex.axis=3)
#   axis(side=1,at=50,cex.axis=3)
#   dev.off()
#
#
#   pdf("FIG10Right.pdf")
#   plot.stepfun(S,xlab="",ylab="",vertical=TRUE,do.points=FALSE,axes=FALSE)
#   abline(v=a,lwd=3,lty=2)
#   abline(h=0)
#   axis(side=1,at=0,cex.axis=3)
#   axis(side=1,at=25,cex.axis=3)
#   axis(side=1,at=50,cex.axis=3)
#   dev.off()
#
#
#   pdf("FIG9Right.pdf")
#
#   PTS = SCMS[S > a,]
#   k   = nrow(PTS)
#   if(k>5){Y  = ClusterFilter(PTS,m=m[d+1],b=b)
#   kk = nrow(Y)
#   allridges[[d+1]]=Y
#   plot(allridges[[d+1]], pch=20,lwd=5, col="red",xlim=plran[,1],
#        ylim=plran[,2], xlab="", ylab="",xaxt="n",yaxt="n")
#   if(kk > 0){
#     for(i in 1:nrow(Y)) {
#       dd = sqrt((Y[,1] - Y[i,1])^2 + (Y[,2] - Y[i,2])^2)
#       friends = matrix(Y[dd <= b, ], ncol=2)
#
#     }
#   }
#   }
#   dev.off()
# }
#
#
# Makedata = function(r){
#   set.seed(r)
#   sig     = 0.03
#   nclump  = 25
#   ncircle = 300
#   nnoise  = 1000
#
#   n = 4*nclump + ncircle + nnoise
#   x0 = c(rnorm(nclump,-.7,sig),rnorm(nclump,.7,sig),
#          rnorm(nclump,.7,sig),rnorm(nclump,-.7,sig))
#   y0 = c(rnorm(nclump,.7,sig),rnorm(nclump,.7,sig),
#          rnorm(nclump,-.7,sig),rnorm(nclump,-.7,sig))
#   th = runif(ncircle,0,2*pi)
#   x1 = .5*cos(th) + rnorm(ncircle,0,sig)
#   y1 = .5*sin(th) + rnorm(ncircle,0,sig)
#   x2 = runif(nnoise,-1,1)
#   y2 = runif(nnoise,-1,1)
#   x = c(x0,x1,x2)
#   y = c(y0,y1,y2)
#   X = cbind(x,y)
#   return(X)
# }
#
#
# montecarlo.fun = function(data, bw, nrep){
#   n         = nrow(data)
#   D         = ncol(data)
#   maxsig    = matrix(0, nrow=nrep, ncol=D)
#   avgdist   = rep(0, nrep)
#   maxsize   = rep(0, nrep)
#   UN        = array(runif(n=n*D*nrep, min=-1, max=1), dim=c(n,D,nrep))
#   for(i in 1:nrep){
#     print(i)
#     for(d in 1:D) {maxsig[i,d] = max(GetSignatures(Y=UN[,,i],
#                                                    X=UN[,,i], h=bw, d=d-1))}
#     avgdist[i]= mean(get.knn(data=UN[,,i], k=1)$nn.dist)
#   }
#   meandist  = mean(avgdist)
#   for(i in 1:nrep){
#     C    = hclust(dist(UN[,,i]), method="single")
#     o    = cutree(C, h=meandist)
#     maxsize[i] = max(table(o))
#   }
#   return(list(maxsig=maxsig, avgdist=avgdist,
#               meandist=meandist, maxsize=maxsize))
# }
#
#
# #################################
#
# r = 12345
# X    = Makedata(r)
#
# ran    = apply(X=X, MARGIN=2, FUN=range)
# midran = (ran[2,] + ran[1,])/2
# maxran = ran[2,] - ran[1,]
# maxran = max(ran[2,] - ran[1,])
# plran  = rbind(midran - 0.52*maxran, midran + 0.52*maxran)
#
#
#
# plot(X,pch=20,xlim=plran[,1],ylim=plran[,2], xlab="", ylab="",xaxt="n",yaxt="n")
# dev.off()
#
# h      = bandwidth(X) # Silverman
# h      = h/2
# m      = c(10,50)
# b      = .06
# avec   = c(20, 25)
# Nsteps = 150
#
# Plotit(X,h,Nsteps,m,b,avec)
