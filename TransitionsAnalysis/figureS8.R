#Models of cooperation in pre-historic demographic transitions (West Africa)


dat1<-(MODWAFR)
#  Cooperation

modWAFR1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR1)
cor(predict(modWAFR1), dat1$R1[1:12])^2

modWAFR2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR2)
cor(predict(modWAFR2), dat1$R2[1:8])^2

modWAFR3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR3)
cor(predict(modWAFR3), dat1$R3[1:8])^2

modWAFR4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR4)
cor(predict(modWAFR4), dat1$R4[1:8])^2

modWAFR5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR5)
cor(predict(modWAFR5), dat1$R5[1:19])^2

modWAFR6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR6)
cor(predict(modWAFR6), dat1$R6[1:15])^2

modWAFR7<-nls(R7~b-k*exp(X7)-e*exp(-X7), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modWAFR7)
cor(predict(modWAFR7), dat1$R7[1:13])^2


par(mar=c(4,5,3,5))
par(mfrow=c(4, 2))

x11a<-seq(1.36,1.98,0.01)
y11a<-predict(modWAFR1, list(X1=x11a))
x11b<-seq(1.86, 2.2,0.01)
y11b<-predict(modWAFR2, list(X2=x11b))
x11c<-seq(2.04, 2.28,0.01)
y11c<-predict(modWAFR3, list(X3=x11c))
x11d<-seq(2.28, 2.46,0.01)
y11d<-predict(modWAFR4, list(X4=x11d))
x11e<-seq(2.28, 3.04,0.01)
y11e<-predict(modWAFR5, list(X5=x11e))
x11f<-seq(2.24, 3.16,0.01)
y11f<-predict(modWAFR6, list(X6=x11f))
x11g<-seq(3, 3.58,0.01)
y11g<-predict(modWAFR7, list(X7=x11g))


plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4680 - 4350 BPy)", cex.main=0.8, ylim=c(-0.015, 0.12), xlim=c(1.37, 1.98))
lines(x11a, y11a, lw=2, col="gray65", lty=1)
text(1.98, 0.11, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4170 - 3960 BPy)", cex.main=0.8, ylim=c(-0.015, 0.035), xlim=c(1.86, 2.04))
lines(x11b, y11b, lw=2, col="gray65", lty=1)
text(2.04, 0.03, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3930 - 3720 BPy)", cex.main=0.8, ylim=c(-0.015, 0.045), xlim=c(2.04, 2.28))
lines(x11c, y11c, lw=2, col="gray65", lty=1)
text(2.28, 0.04, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3690 - 3480 BPy)", cex.main=0.8, ylim=c(-0.015, 0.045), xlim=c(2.27, 2.46))
lines(x11d, y11d, lw=2, col="gray65", lty=1)
text(2.46, 0.04, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3210 - 2670 BPy)", cex.main=0.8, ylim=c(-0.015, 0.2), xlim=c(2.26, 3.045))
lines(x11e, y11e, lw=2, col="gray65", lty=1)
text(3.04, 0.185, "(E)", font=2)
abline(h=0, lty=3)


plot(dat1$X6, dat1$R6, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1650 - 1230 BPy)", cex.main=0.8, ylim=c(-0.015, 0.1), xlim=c(2.24, 3.03))
lines(x11f, y11f, lw=2, col="gray65", lty=1)
text(3.015, 0.09, "(F)", font=2)
abline(h=0, lty=3)

plot(dat1$X7, dat1$R7, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1080 - 720 BPy)", cex.main=0.8, ylim=c(-0.015, 0.07), xlim=c(3, 3.45))
lines(x11g, y11g, lw=2, col="gray65", lty=1)
text(3.44, 0.065, "(G)", font=2)
abline(h=0, lty=3)


