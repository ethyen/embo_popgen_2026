# exercices day 4 afternoon

## Q1
### a) 1/2N = 1/100 = 0.01
### b) (1- 1/2N) * (1/2N) = 
twoN <- 100
(1- 1/(twoN)) * (1/(twoN))

## Q2
### a) E[dij] = 4Nmu
dij <- 21/10e3
mu <- 1e-9
N <- dij/(4*mu)
N
### b) T[MRCA] = 2N generations
2*N

## Q3
d12 <- 3
d13 <- 4
d23 <- 1
pi <-(d12 + d13 + d23)/3
pi / 20



###################################

#####   FASTSIMCOAL #####

plotTrees <- function(trees, popCol=c("black", "orange2", "purple")){
  maxHeight <- max(unlist(lapply(trees, function(x){ max(branching.times(x)) })))
  nCols <- ceiling(sqrt(length(trees)))
  nRows <- ceiling(length(trees) / nCols)
  par(mfrow = c(nRows, nCols), oma=c(0,4,0,0), las=1, xpd=NA)
  
  for(tr in 1:length(trees)){
    plotTree(trees[[tr]], direction="downwards", ylim=c(0, maxHeight),
             lwd=0.6, ftype="off", mar=c(0.1,0.7,0.1,0.7))
    
    if(tr %% nCols == 1){ axis(side = 2)}
    
    #add tips and color by population
    pop <- as.numeric(unlist(lapply(strsplit(trees[[1]]$tip.label, "[.]"), '[', 2) ))
    
    nTips <- length(trees[[tr]]$tip.label);
    symbols(1:nTips, rep(0, nTips), circles=rep(0.4, nTips), add=TRUE, inches=0.03, fg=NA, bg=popCol[pop])
  }
}


library(phytools)
trees <- read.nexus("constsize_1_true_trees.trees")
plotTrees(trees)

mean(unlist(lapply(trees, function(x){ max(branching.times(x)) })))

trees2 <- read.nexus("constsize2_1_true_trees.trees")
plotTrees(trees2)
mean(unlist(lapply(trees2, function(x){ max(branching.times(x)) })))

trees3 <- read.nexus("constsize3_1_true_trees.trees")
plotTrees(trees3)
mean(unlist(lapply(trees3, function(x){ max(branching.times(x)) })))

exp_trees <- read.nexus("expgrowth_1_true_trees.trees")
plotTrees(exp_trees)
mean(unlist(lapply(exp_trees, function(x){ max(branching.times(x)) })))
