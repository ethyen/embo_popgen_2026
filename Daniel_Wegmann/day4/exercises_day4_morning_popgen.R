# Day 4 morning exercises

## Q1

### a) Under WFE, 1

### b)p(no offspring) = (1-1/2N)^2N

## Q2

### a) P(Xt+1 = j | Xt = i) = (N choose j) * p^j(1-p)^(N-j), i = j = 10, n = 100 

N <- 100
j <- 10
i <- 10
p <- i/N

P <- choose(N, j) * p^j * (1-p)^(N-j)
P

# or

dbinom(j, N, p)

### b) j = 0
j <- 0
dbinom(j, N, p)

### c) j = 100

j <- 100
dbinom(j, N, p)

### d) j < i
pbinom(9, N, p, lower.tail = T)

### e) j > i
1-pbinom(10, N, p, lower.tail = T)

## Q3

par(mfrow = c(2, 2))

twoN <- c(10, 100, 1000, 10000)
f <- 0.1
 for (i in 1:4) {
   N <- twoN[i]/2
   p <- f
   x <- 0:twoN[i]
   y <- dbinom(x, twoN[i], p)
   plot(x, y, type = "p", main = paste("2N =", twoN[i]), xlab = "j", ylab = "P(Xt+1 = j | Xt = i)")
 }

## Q4)

simulateWF <- function(twoN, f, G){
  p <- numeric(G + 1)
  p[1] <- f
  for(i in 1:G){
    p[i+1] <- rbinom(1, size = twoN, prob = p[i]) / twoN
  }
  return(p)
}

## Q5)
dev.off()
trajectories <- replicate(1000, simulateWF(twoN = 100, f = 0.1, G = 1000))
plot(0, type='n', ylim=c(0,1), xlim=c(0, nrow(trajectories)))
invisible(apply(trajectories, 2, lines, type='l'))

print(paste("Allele was lost in", sum(trajectories[1000,] == 0), "/", ncol(trajectories), "cases."))

drift_plot <- function(Ntrajectories,twoN,f,G){
  trajectories <- replicate(Ntrajectories, simulateWF(twoN = twoN, f = f, G = G))
  plot(0, type='n', ylim=c(0,1), xlim=c(0, nrow(trajectories)))
  invisible(apply(trajectories, 2, lines, type='l'))
  
  print(paste("Allele was lost in", sum(trajectories[Ntrajectories,] == 0), "/", ncol(trajectories), "cases."))
  
}

par(mfrow = c(4,1))

for(i in c(10,100,1000,10000)){
  drift_plot(1000,twoN = i,f=0.1,G=1000)}
