# Bernoulli Metropolis Algorithm


g <- function(data,theta,a,b){ # Fucntion for the bernoulli pdf
  theta^(sum(data)+a-1)*(1-theta)^(length(data)-sum(data)+b-1)
}

lng <- function(data,theta,a,b){ # logged pdf
  (sum(data)+a-1)*log(theta) + (length(data)-sum(data)+b-1)*log(1-theta)
}

# bernoulli metropolis algorithm from scratch
bern_metrop <- function(data,pra,prb,loops,candsig){
  cntr <- 0
  out <- NULL
  out[1] <- .5
  for(i in 2:loops){
    out[i] <- out[i-1]
    cand <- rnorm(1,out[i-1],candsig)
    if(cand>0 & cand<1){
      r <- lng(data,cand,pra,prb)-lng(data,out[i-1],pra,prb)
      if (r > log(runif(1,0,1))){
        out[i] <- cand
        cntr <- cntr + 1
      }
    }
  }
   return(list(out=out,cntr=cntr))
}
