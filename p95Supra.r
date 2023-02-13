### Some functions

## Henson fos
HC <- function(Sensitivity) min(6.0, exp(-0.081 * Sensitivity + 3.27))

## Aborts formula of FOS
fp <- 0.01
fn <- 0.01
fos <- function(x) fp + (1-fp-fn)*(1 - pnorm(0:40, x, HC(x)))

## Supra stim
supraStim <- function(dB,level) which.min(abs(fos(dB)- level))-1


## Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db,x,y) {
  s <- list(x=x, y=y, level=dbTocd(db, 4000/pi), size=0.43, color="white",
            duration=200, responseWindow=1500)
  class(s) <- "opiStaticStimulus"
  return(s)
}


## Plot VF function
plotvf <- function(vf){
  plot(1,1,xlim=c(1,8),ylim=c(1,9),type="n",xlab="",ylab="",axes=FALSE)
  for(x in 1:9){
    for(y in 1:8){
      if(!is.na(vf[y,x])){
        text(x,y,vf[y,x])
      }
    }
  }
}

## P95 test function
p95test <- function(tt,supraStimLevel,fnr,fpr){
  results <- tt
  for(x in 1:9){
    for(y in 1:8){
      if(!is.na(tt[y,x])){
        ## Showing stim 1st time
        rs <- opiPresent(stim=makeStim(db = supraStim(tt[y,x],supraStimLevel),
                                       x=x,
                                       y=y),
                         tt=tt[y,x],
                         fpr=fpr,
                         fnr=fpr)
        if(rs$seen){
          ## if seen first time make it one
          results[y,x] = 1
        }else{
          ## Showing stim 2nd time
          rs <- opiPresent(stim=makeStim(db = supraStim(tt[y,x],supraStimLevel),
                                         x=x,
                                         y=y),
                           tt=tt[y,x],
                           fpr=fpr,
                           fnr=fpr)
          if(rs$seen){
            ## if seen second time make it one
            results[y,x] = 1
          }else{
            ## Showing stim 3rd time
            rs <- opiPresent(stim=makeStim(db = supraStim(tt[y,x],supraStimLevel),
                                           x=x,
                                           y=y),
                             tt=tt[y,x],
                             fpr=fpr,
                             fnr=fpr)
            if(rs$seen){
              ## if seen third time make it one
              results[y,x] = 1
            }else{
              ## if NOT seen all three times make it one
              results[y,x] = 0
            }
          }
        }
      }
    }
  }
  return(results)
}



#############################
## Actual testing
#############################

## Load lib
require(OPI)

## Choose sim Henson
chooseOpi("SimHenson")

if (!is.null(opiInitialize(type="C", cap=6)))
  stop("opiInitialize failed")



## True threshold
tt <- matrix(c(
  NA, NA,  23,  23,  23,  23, NA, NA, NA,
  NA, 24,  24,  24,  24,  24, 24, NA, NA,
  23, 24,  25,  24,  24,  25, 24, 23, NA,
  23, NA,  24,  24,  24,  24, 24, 23,  4,
  23, NA,  24,  24,  24,  24, 24, 23,  4,
  23, 24,  25,  24,  24,  25, 24, 23, NA,
  NA, 24,  24,  24,  24,  24, 24, NA, NA,
  NA, NA,  23,  23,  23,  23, NA, NA, NA
), nrow=8, ncol=9, byrow=TRUE)




results1 <- p95test(tt,0.95,0,0)
results2 <- p95test(tt,0.95,0.05,0.10)
results3 <- p95test(tt,0.95,0.05,0.20)


plot(layout(matrix(c(1:4),1,4)))
par(oma=c(0.1,0.1,1,0),mai=c(4,0.5,0.1,0.5))
plotvf(tt)
title("Input / True Threshold")
plotvf(results1)
title("p95 test with 0% FP and 0%FN")
plotvf(results2)
title("p95 test with 10% FP and 5%FN")
plotvf(results3)
title("p95 test with 20% FP and 5%FN")
