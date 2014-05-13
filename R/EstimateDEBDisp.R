EstimateDEBDisp <-
function (y,m,groups,neff=NULL,S=NULL)
 {
   if( is.null(groups) ) groups = rep(1,ncol(y))
   targets <- rownames(y)
   groups <- as.factor(groups)
   K = length(unique(groups))

   if( is.null(neff) )
    {
       z <- y/m
       neff <- apply(z,1,FUN=function(vec) sum(vec!=1 & vec!=0,na.rm=TRUE))
       neff[neff<=K] <- K+1
    }

   if( is.null(S) ) S <- .DBS( y=y , m=m , groups=groups )
    names(S) <- as.character(neff)
   a = (neff-K)/2
   m.S = tapply( S , names(S) , mean )
   m.S = m.S[match( names(S) , names(m.S) )]
   pars = nlm( .nll.gbp2 , p = c( 1 , 1 ) , a = a , S = S , hessian = FALSE , iterlim = 1000 )$estimate
   disps <- (pars[1] + a)/(pars[2]+S)
   disps[disps==0] <- sqrt(.Machine$double.eps)
    names(disps) <- targets
   return(disps)
 }
