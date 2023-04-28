#Figure 4 frequency histogram of the periods (yrs) of waves of expansion/stagnation/collapse

par(mfrow=c(1, 1))
par(mar=c(4,5,3,5))

hist(periodo$years, freq=TRUE, density = NULL, angle=45, col="gray65", breaks=9, xlim=c(100, 800), xlab="Period (years) of population growth rates pulses", main=NULL, tck=0.03)
mean(periodo$years)