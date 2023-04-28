#Models of cooperation in pre-historic demographic transitions (Near East-Levante)
#Figure S3


dat1<-(WASIAMOD1)
#  Cooperation


modWASIA1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA1)

modWASIA2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA2)

modWASIA3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA3)

modWASIA4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA4)

modWASIA5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA5)

modWASIA6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWASIA6)

cor(predict(modWASIA6), dat1$R6[1:16])^2

par(mar=c(4,5,3,5))
par(mfrow=c(3, 2))

x1a<-seq(1.5,2.6,0.01)
y1a<-predict(modWASIA1, list(X1=x1a))
x1b<-seq(2.2,2.52,0.01)
y1b<-predict(modWASIA2, list(X2=x1b))
x1c<-seq(2.55,2.9,0.01)
y1c<-predict(modWASIA3, list(X3=x1c))
x1d<-seq(2.75,3.3,0.01)
y1d<-predict(modWASIA4, list(X4=x1d))
x1e<-seq(2.7,3.26,0.01)
y1e<-predict(modWASIA5, list(X5=x1e))
x1f<-seq(3.16,3.8,0.01)
y1f<-predict(modWASIA6, list(X6=x1f))

plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, main="12030 - 11310 BPy", cex.main=0.9, ylim=c(-0.02, 0.1), xlim=c(1.45, 2.7))
lines(x1a, y1a, lw=2, col="gray65", lty=1)
text(2.6, 0.1, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, main="10860 - 10530 BPy", cex.main=0.9, ylim=c(-0.015, 0.05), xlim=c(2.15, 2.6))
lines(x1b, y1b, lw=2, col="gray65", lty=1)
text(2.6, 0.047, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, main="10500 - 10200 BPy", cex.main=0.9, ylim=c(-0.015, 0.1), xlim=c(2.45, 2.95))
lines(x1c, y1c, lw=2, col="gray65", lty=1)
text(2.9, 0.09, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, main="9990 - 9570 BPy", cex.main=0.9, ylim=c(-0.015, 0.12), xlim=c(2.7, 3.45))
lines(x1d, y1d, lw=2, col="gray65", lty=1)
text(3.42, 0.115, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, main="9210 - 8910 BPy", cex.main=0.9, ylim=c(-0.015, 0.1), xlim=c(2.65, 3.3))
lines(x1e, y1e, lw=2, col="gray65", lty=1)
text(3.25, 0.1, "(E)", font=2)
abline(h=0, lty=3)

plot(dat1$X6, dat1$R6, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, main="8730 - 8280 BPy", cex.main=0.9, ylim=c(-0.015, 0.075), xlim=c(3.15, 3.8))
lines(x1f, y1f, lw=2, col="gray65", lty=1)
text(3.8, 0.06, "(F)", font=2)
abline(h=0, lty=3)

