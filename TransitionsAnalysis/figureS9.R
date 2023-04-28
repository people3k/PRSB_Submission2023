#Models of cooperation in pre-historic demographic transitions (North America)


dat1<-(MODNAM)
#  Cooperation

modNAM1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modNAM1)
cor(predict(modNAM1), dat1$R1[1:10])^2

modNAM2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modNAM2)
cor(predict(modNAM2), dat1$R2[1:15])^2

modNAM3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modNAM3)
cor(predict(modNAM3), dat1$R3[1:22])^2

modNAM4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modNAM4)
cor(predict(modNAM4), dat1$R4[1:10])^2

modNAM5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modNAM5)
cor(predict(modNAM5), dat1$R5[1:15])^2


par(mar=c(4,5,3,5))
par(mfrow=c(3, 2))

x7a<-seq(4.45,4.78,0.01)
y7a<-predict(modNAM1, list(X1=x7a))
x7b<-seq(4.79,5.2,0.01)
y7b<-predict(modNAM2, list(X2=x7b))
x7c<-seq(5.22, 5.98,0.01)
y7c<-predict(modNAM3, list(X3=x7c))
x7d<-seq(5.9, 6.45,0.01)
y7d<-predict(modNAM4, list(X4=x7d))
x7e<-seq(6.36, 6.87,0.01)
y7e<-predict(modNAM5, list(X5=x7e))


plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3660 - 3390 BPy)", cex.main=0.8, ylim=c(-0.015, 0.065), xlim=c(4.45, 4.8))
lines(x7a, y7a, lw=2, col="gray65", lty=1)
text(4.8, 0.062, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3240 - 2820 BPy)", cex.main=0.8, ylim=c(-0.015, 0.08), xlim=c(4.77, 5.19))
lines(x7b, y7b, lw=2, col="gray65", lty=1)
text(5.18, 0.08, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2190 - 1560 BPy)", cex.main=0.8, ylim=c(-0.015, 0.055), xlim=c(5.2, 5.92))
lines(x7c, y7c, lw=2, col="gray65", lty=1)
text(5.9, 0.055, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1530 - 1260 BPy)", cex.main=0.8, ylim=c(-0.015, 0.1), xlim=c(5.9, 6.46))
lines(x7d, y7d, lw=2, col="gray65", lty=1)
text(6.45, 0.095, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1080 - 660 BPy)", cex.main=0.8, ylim=c(-0.015, 0.1), xlim=c(6.36, 6.86))
lines(x7e, y7e, lw=2, col="gray65", lty=1)
text(6.85, 0.095, "(D)", font=2)
abline(h=0, lty=3)

