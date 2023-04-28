#Figure 3 of the article

par(mfrow=c(2, 2))
par(mar=c(4,4,3,5))

plot(WASIAMOD1$X2, WASIAMOD1$R2, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, main="Near East (10860 - 10530 BPy)", cex.main=0.85, ylim=c(-0.01, 0.05), xlim=c(2.15, 2.6))
lines(x1b, y1b, lw=2, col="gray65", lty=1)
text(2.58, 0.047, "(A)", font=2)
abline(h=0, lty=3)

plot(EURMOD$X3, EURMOD$R3, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, main="Europe (7530 - 7080 BPy)", cex.main=0.85, ylim=c(-0.015, 0.075), xlim=c(4.32,4.9))
lines(x3c, y3c, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(4.85, 0.07, "(B)", font=2)

#plot(EASIAMOD$X3, EASIAMOD$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="East Asia (7080 - 6600 BPy)", cex.main=0.85, ylim=c(-0.02, 0.1), xlim=c(1.2, 2.0))
#lines(x4c, y4c, lw=2, col="gray65", lty=1)
#text(2.0, 0.095, "(C)", font=2)
#abline(h=0, lty=3)

plot(MODSAM$X4, MODSAM$R4, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, main="South America (4710 - 3990 BPy)", cex.main=0.85, ylim=c(-0.01, 0.06), xlim=c(2.68, 3.42))
lines(x55d, y55d, lw=2, col="gray65", lty=1)
text(3.4, 0.055, "(C)", font=2)
abline(h=0, lty=3)

plot(AUSTMOD$X1, AUSTMOD$R1, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, main="Australia (4290 - 3750 BPy)", cex.main=0.85, ylim=c(-0.01, 0.045), xlim=c(1.88, 2.22))
lines(x8a, y8a, lw=2, col="gray60", lty=1)
text(2.2, 0.04, "(D)", font=2)
abline(h=0, lty=3)

#plot(REVINDUST$LNE, REVINDUST$RE, bty="l", type="p", pch=19, col="black", xlab="X(t)", ylab="", tck=0.02, xlim=c(6.75, 9.15), ylim=c(0, 0.5),cex.main=0.85, main="Europe (1650 - 2022)")
#abline(h=0, lty=3)
#lines(XX, YY, col="gray60", lty=1, lw=2)
#text(9.15, 0.48, "(F)", font=2)


