# Simulate SituationB results from the Simmons et al paper. Reference- class notes

SituationB <- function(pval){ 
  
  #Group 1
  Liking1 <- rnorm(20, 0, 1) # Generate normal RV with mean = 0, var = 1
  WTP1 <- rnorm(20, 0, 1) # Generate normal RV with mean = 0, var = 1
  
  #Two-Sample t-tests
  t.test1 <- t.test(Liking1, WTP1, var.equal = TRUE) # Assumes equal var
  
  if(t.test1$p.value>pval){
    Liking2 <- rnorm(10, 0, 1) # Generate normal RV 10 more samples
    WTP2 <- rnorm(10,0, 1) # Generate normal RV 10 more samples
    Like<- c(Liking1, Liking2) #append all 30 samples 
    WTP<-c(WTP1,WTP2) # append all 30 samples
    t.test2 <- t.test( Like, WTP, var.equal = TRUE)# Assumes equal var
    signif2 <- ifelse(t.test2$p.value < pval,  
                      1,  # value of signif if condition is met
                      0)  # value of signif if condition not met
    return(signif2)
  }
  else{
    signif1 <- ifelse(t.test1$p.value < pval, 
                      1,  # value of signif if condition is met
                      0) 
    return(signif1)
   }
}

replicates <- replicate(15000, SituationB(.05)) # replicate for looping
SitBSim <- mean(replicates)
