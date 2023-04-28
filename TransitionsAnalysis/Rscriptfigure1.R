# Time series of SPD used in the article (Figure 1)

par(mfrow=c(4, 2))
par(mar=c(4,5,3,5))


plot(WESTASIA30$BPy, (WESTASIA30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(15000, 4000), ylim=c(), tck=0.03, xlab="", ylab="X(t)", main="Middle East", cex.main=0.9, tck=0.02)

abline(v=12150, lty=3)
abline(v=10440, lty=3,lwd=2, col="red")
abline(v=8280, lty=3)
text(4000, 40, "(A)", font=2)


plot(EUR30$BPy, (EUR30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(15000, 300), ylim=c(), tck=0.03, xlab="", ylab="", main="Europe", cex.main=0.9, tck=0.02)

abline(v=8220, lty=3)
abline(v=6390, lty=3,lwd=2, col="red")
abline(v=1320, lty=3)
text(350, 350, "(B)", font=2)

plot(EASTASIA30$BPy, (EASTASIA30$SPD),  type="l", bty="l", lwd=2, col="black", ylim=c(), xlim=c(15000, 2000), xlab="", ylab="X(t)", main="East Asia (China)", cex.main=0.9, tck=0.02)
abline(v=8910, lty=3)
abline(v=5700, lty=3,lwd=2, col="red")
abline(v=3480, lty=3)
text(2000, 49, "(C)", font=2)

plot(SOUTHAM30$BPy, (SOUTHAM30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(14000, 200), ylim=c(1, 46), tck=0.03, xlab="", ylab="", main="South America", cex.main=0.9, tck=0.02)
abline(v=6840, lty=3)
abline(v=4470, lty=3,lwd=2, col="red")
abline(v=600, lty=3)
text(170, 44, "(D)", font=2)

plot(SOUTHAFR30$BPy, (SOUTHAFR30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(14900, 100), ylim=c(), tck=0.03, xlab="", ylab="X(t)", main="South Africa", cex.main=0.9, tck=0.02)
abline(v=4710, lty=3)
abline(v=2610, lty=3,lwd=2, col="red")
abline(v=660, lty=3)
text(100, 52, "(E)", font=2)


plot(WESTAFR30$BPy, (WESTAFR30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(12000, 100), ylim=c(), tck=0.03, xlab="", ylab="", main="West Africa", cex.main=0.9, tck=.02)
abline(v=4680, lty=3)
abline(v=3900, lty=3,lwd=2, col="red")
abline(v=720, lty=3)
text(100, 30.5, "(F)", font=2)


plot(MNAM30$BPy, (MNAM30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(14900, 0), ylim=c(), tck=0.03, xlab="cal year BP", ylab="X(t)", main="North America ", cex.main=0.9, tck=.02)
abline(v=3660, lty=3)
abline(v=2700, lty=3,lwd=2, col="red")
abline(v=660, lty=3)
text(50, 900, "(G)", font=2)

plot(AUST30$BPy, (AUST30$SPD), bty="l", type="l", lwd=2, lty=1, col="black", xlim=c(12600, 0), ylim=c(), tck=0.03, xlab="cal year BP", ylab="", main="Australia", cex.main=0.9)
abline(v=4290, lty=3)
abline(v=2280, lty=3,lwd=2, col="red")
abline(v=600, lty=3)
text(50, 62, "(H)", font=2)
