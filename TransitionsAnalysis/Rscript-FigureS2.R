# Graficos funciones R transiciones demográficas

par(mfrow=c(4, 2))
par(mar=c(3.99,5,1.2,5))

data1<-(RREV1)

#Phase portraits or reproduction curves 

plot(log(WESTASIA30$SPD), WESTASIA30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.12, 0.18), tck=0.03, xlab="", ylab="X(t+1)-X(t)", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="Middle East", cex.main=0.9)
abline(h=0, lty=3)
#text(2.8, 0.062, "12210 - 8190 years cal yr BP", cex=0.7)
text(3.7, 0.17, "(A)", cex=1, font=2)
arrows(1, 0.13, 3.6, 0.13, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(EUR30$SPD), EUR30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.14, 0.16), tck=0.03, xlab="", ylab="", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="Europe", cex.main=0.9,)
abline(h=0, lty=3)
#text(1.8, 0.0675, "12270 - 4890 cal yr BP", cex=0.7)
text(5.9, 0.15, "(B)", cex=1, font=2)

arrows(3.8, 0.13, 5.8, 0.13, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(EASTASIA30$SPD), EASTASIA30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.18, 0.25), tck=0.03, xlab="", ylab="X(t+1)-X(t)", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="East Asia (China)", cex.main=0.9)
abline(h=0, lty=3)
#text(5, 0.047, "8340-2010 cal yr BP", cex=0.7)
text(4, 0.245, "(C)", cex=1, font=2)

arrows(0.81, 0.21, 4, 0.21, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(SOUTHAM30$SPD), SOUTHAM30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.25, 0.4), tck=0.03, xlab="", ylab="", pch=19, cex=0.45, cex.axis=0.8, main="South America", cex.main=0.9)
abline(h=0, lty=3)
#text(25, 0.047, "Europe (12210-8190 years bp)", cex=0.75)
text(3.7, 0.4, "(D)", cex=1, font=2)

arrows(1.05, 0.3, 3.4, 0.3, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(SOUTHAFR30$SPD), SOUTHAFR30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.15, 0.25), tck=0.03, xlab="", ylab="X(t+1)-X(t)", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="South Africa", cex.main=0.9)
abline(h=0, lty=3)
#text(25, 0.047, "Europe (12210-8190 years bp)", cex=0.75)
text(4, 0.23, "(E)", cex=1, font=2)

arrows(1.6, 0.19, 4, 0.19, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(WESTAFR30$SPD), WESTAFR30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.25, 0.8), tck=0.03, xlab="", ylab="", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="West Africa", cex.main=0.9)
abline(h=0, lty=3)
#text(25, 0.047, "Europe (12210-8190 years bp)", cex=0.75)
text(3, 0.78, "(F)", cex=1, font=2)

arrows(1, 0.4, 3.6, 0.4, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(MNAM30$SPD), MNAM30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.25, 0.25), tck=0.03, xlab="X(t)", ylab="X(t+1)-X(t)", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="North America", cex.main=0.9)
abline(h=0, lty=3)
#text(25, 0.047, "Europe (12210-8190 years bp)", cex=0.75)
text(6.8, 0.22, "(G)", cex=1, font=2)

arrows(4, 0.16, 7, 0.16, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

plot(log(AUST30$SPD), AUST30$R, bty="l", type="o", lty=1, col="black", xlim=c(), ylim=c(-0.15, 0.3), tck=0.03, xlab="X(t)", ylab="", pch=19, cex=0.45, cex.axis=0.8, cex.lab=0.8, main="Australia", cex.main=0.9)
abline(h=0, lty=3)
#text(25, 0.047, "Europe (12210-8190 years bp)", cex=0.75)
text(4.2, 0.275, "(H)", cex=1, font=2)
arrows(1, 0.18, 4.3, 0.18, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)


