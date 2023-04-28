#Models of cooperation in pre-historic demographic transitions (Central Andes)
# Figure S6

dat1<-(MODSAM)
#  Cooperation

modANDES1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modANDES1)
cor(predict(modANDES1), dat1$R1[1:19])^2

modANDES2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modANDES2)
cor(predict(modANDES2), dat1$R2[1:10])^2

modANDES3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modANDES3)
cor(predict(modANDES3), dat1$R3[1:9])^2

modANDES4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modANDES4)
cor(predict(modANDES4), dat1$R4[1:25])^2

modANDES5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=1, e=22), trace=TRUE)
summary(modANDES5)
cor(predict(modANDES5), dat1$R5[1:12])^2

modANDES6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=1, e=22), trace=TRUE)
summary(modANDES6)
cor(predict(modANDES6), dat1$R6[1:9])^2

modANDES7<-nls(R7~b-k*exp(X7)-e*exp(-X7), data=dat1, start=c(b=0.3, k=1, e=22), trace=TRUE)
summary(modANDES7)
cor(predict(modANDES7), dat1$R7[1:8])^2

modANDES8<-nls(R8~b-k*exp(X8)-e*exp(-X8), data=dat1, start=c(b=0.3, k=1, e=22), trace=TRUE)
summary(modANDES8)
cor(predict(modANDES8), dat1$R8[1:10])^2

modANDES9<-nls(R9~b-k*exp(X9)-e*exp(-X9), data=dat1, start=c(b=0.3, k=1, e=22), trace=TRUE)
summary(modANDES9)
cor(predict(modANDES9), dat1$R9[1:11])^2


par(mar=c(4,5,3,5))
par(mfrow=c(5, 2))

x55a<-seq(2.16,5.1,0.01)
y55a<-predict(modANDES1, list(X1=x55a))
x55b<-seq(2.49,2.78,0.01)
y55b<-predict(modANDES2, list(X2=x55b))
x55c<-seq(2.75, 2.92,0.01)
y55c<-predict(modANDES3, list(X3=x55c))
x55d<-seq(2.7, 3.4,0.01)
y55d<-predict(modANDES4, list(X4=x55d))
x55e<-seq(3.06, 3.48,0.01)
y55e<-predict(modANDES5, list(X5=x55e))
x55f<-seq(3.35, 3.6,0.01)
y55f<-predict(modANDES6, list(X6=x55f))
x55g<-seq(3.3, 3.52,0.01)
y55g<-predict(modANDES7, list(X7=x55g))
x55h<-seq(2.09, 3.09,0.01)
y55h<-predict(modANDES8, list(X8=x55h))
x55i<-seq(2.785, 3.275,0.01)
y55i<-predict(modANDES9, list(X9=x55i))

plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(7230 - 6690 BPy)", cex.main=0.8, ylim=c(-0.01, 0.045), xlim=c(2.16, 2.51))
lines(x55a, y55a, lw=2, col="gray65", lty=1)
text(2.50, 0.04, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(6090 - 5820 BPy)", cex.main=0.8, ylim=c(-0.01, 0.05), xlim=c(2.45, 2.8))
lines(x55b, y55b, lw=2, col="gray65", lty=1)
text(2.80, 0.045, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(5790 - 5550 BPy)", cex.main=0.8, ylim=c(-0.01, 0.036), xlim=c(2.74, 2.925))
lines(x55c, y55c, lw=2, col="gray65", lty=1)
text(2.925, 0.033, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4710 - 3990 BPy)", cex.main=0.8, ylim=c(-0.01, 0.06), xlim=c(2.72, 3.4))
lines(x55d, y55d, lw=2, col="gray65", lty=1)
text(3.4, 0.055, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3660 - 3330 BPy)", cex.main=0.8, ylim=c(-0.01, 0.075), xlim=c(3.06, 3.48))
lines(x55e, y55e, lw=2, col="gray65", lty=1)
text(3.48, 0.07, "(E)", font=2)
abline(h=0, lty=3)

plot(dat1$X6, dat1$R6, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3030 - 2790 BPy)", cex.main=0.8, ylim=c(-0.01, 0.06), xlim=c(3.34, 3.62))
lines(x55f, y55f, lw=2, col="gray65", lty=1)
text(3.62, 0.055, "(F)", font=2)
abline(h=0, lty=3)

plot(dat1$X7, dat1$R7, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2430 - 2220 BPy)", cex.main=0.8, ylim=c(-0.01, 0.04), xlim=c(3.28, 3.52))
lines(x55g, y55g, lw=2, col="gray65", lty=1)
text(3.51, 0.035, "(G)", font=2)
abline(h=0, lty=3)

plot(dat1$X8, dat1$R8, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1470 - 1200 BPy)", cex.main=0.8, ylim=c(-0.01, 0.08), xlim=c(2.46, 2.83))
lines(x55h, y55h, lw=2, col="gray65", lty=1)
text(2.82, 0.078, "(H)", font=2)
abline(h=0, lty=3)

plot(dat1$X9, dat1$R9, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1140 - 840 BPy)", cex.main=0.8, ylim=c(-0.01, 0.11), xlim=c(2.78, 3.275))
lines(x55i, y55i, lw=2, col="gray65", lty=1)
text(3.27, 0.1, "(I)", font=2)
abline(h=0, lty=3)
