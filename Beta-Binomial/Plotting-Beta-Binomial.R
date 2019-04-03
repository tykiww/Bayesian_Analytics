



# plot binomial likelihood for n = 10, theta = .9


rng <- seq(0,10)
pmf <- dbinom(rng,10,0.9)
pmf2 <- dbinom(rng,10,0.85)
plot(rng,pmf,type = "h", col = "forest green", )
lines(rng+.1,pmf2, type = "h", col = "red")

# if n= 10 and y = 7, which value of theta, 0.9 or 0.88 leads to a higher likelihood?

dbinom(7,10,c(.9,.85)) # .85 is more likely (for 7) to be the case. We see this in the graph also.


$ plot following pdfs and report mean, variance, and mode.


rng <- seq(0,1,length.out = 100)
# Beta(1,1)
pdf <- dbeta(rng,1,1)
# Beta(3,7)
pdf2 <- dbeta(rng,3,7)
# Beta(100,74)
pdf3 <- dbeta(rng,100,74)
# Beta(.2,5)
pdf4 <- dbeta(rng,.2,5)
# Beta(.5,.5)
pdf5 <- dbeta(rng,.5,.5)

plot(rng,pdf,type = "l", xlim = c(0,1),ylim = c(0,10),col = "red")
lines(rng,pdf2,type = "l", col = "blue")
lines(rng,pdf3,type = "l", col = "green")
lines(rng,pdf4,type = "l", col = "brown")
lines(rng,pdf5,type = "l", col = "orange")

rng <- seq(0,1,length.out = 100)
pdf <- dbeta(rng,1,1)
plot(rng,pdf,type = "l")
