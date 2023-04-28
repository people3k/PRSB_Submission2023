#Models of cooperation in pre-historic demographic transitions (Australia)


dat1<-(AUSTMOD)
#  Cooperation

modAUST1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST1)
cor(predict(modAUST1), dat1$R1[1:19])^2

modAUST2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST2)
cor(predict(modAUST2), dat1$R2[1:9])^2

modAUST3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST3)
cor(predict(modAUST3), dat1$R3[1:11])^2

modAUST4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST4)
cor(predict(modAUST4), dat1$R4[1:11])^2

modAUST5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST5)
cor(predict(modAUST5), dat1$R5[1:19])^2

modAUST6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modAUST6)
cor(predict(modAUST6), dat1$R6[1:17])^2


par(mar=c(4,5,3,5))
par(mfrow=c(3, 2))

x8a<-seq(1.9,2.22,0.01)
y8a<-predict(modAUST1, list(X1=x8a))
x8b<-seq(2.08,2.46,0.01)
y8b<-predict(modAUST2, list(X2=x8b))
x8c<-seq(2.3, 2.58,0.01)
y8c<-predict(modAUST3, list(X3=x8c))
x8d<-seq(2.57, 2.85,0.01)
y8d<-predict(modAUST4, list(X4=x8d))
x8e<-seq(2.84, 3.4,0.01)
y8e<-predict(modAUST5, list(X5=x8e))
x8f<-seq(3.3, 4.2,0.01)
y8f<-predict(modAUST6, list(X6=x8f))


modsmootAUST<-smooth.spline(AUST30$BPy, log(AUST30$SPD), spar=0.5)
#plot(AUST30$BPy, log(AUST30$SPD), bty="l", type="l", lwd=1, lty=1, col="red", xlim=c(12600, 0), ylim=c(), tck=0.03, xlab="years cal. bp", ylab="log (SPD)", cex.lab=0.8, cex.axis=0.8)
#plot(modsmootAUST, type="l", lwd=3, col="orange1", xlim=c(12600, 0), bty="l", lty=1, ylim=c(), tck=0.03, xlab="Years cal. bp", ylab="X(t)", cex.lab=0.8, cex.axis=0.8, main="Australia")
#text(2000, 4.8, "2700 - 480 yrs bp", cex=0.6)
#abline(v=2700, lty=3)
#abline(v=480, lty=3)
#write.csv(predict(modsmootAUST), file="AUST.csv")

plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4290 - 3750 BPy)", cex.main=1, ylim=c(-0.01, 0.045), xlim=c(1.88, 2.22))
lines(x8a, y8a, lw=2, col="gray60", lty=1)
text(2.2, 0.04, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3090 - 2850 BPy)", cex.main=1, ylim=c(-0.01, 0.065), xlim=c(2.08, 2.46))
lines(x8b, y8b, lw=2, col="gray60", lty=1)
text(2.46, 0.058, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2580 - 2280 BPy)", cex.main=1, ylim=c(-0.01, 0.065), xlim=c(2.28, 2.6))
lines(x8c, y8c, lw=2, col="gray60", lty=1)
text(2.6, 0.062, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2190 - 1890 BPy)", cex.main=1, ylim=c(-0.01, 0.06), xlim=c(2.56, 2.86))
lines(x8d, y8d, lw=2, col="gray60", lty=1)
text(2.85, 0.058, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1830 - 1290 BPy)", cex.main=1, ylim=c(-0.01, 0.06), xlim=c(2.82, 3.4))
lines(x8e, y8e, lw=2, col="gray60", lty=1)
text(3.4, 0.058, "(E)", font=2)
abline(h=0, lty=3)

plot(dat1$X6, dat1$R6, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1080 - 600 BPy)", cex.main=1, ylim=c(-0.01, 0.1), xlim=c(3.32, 4.18))
lines(x8f, y8f, lw=2, col="gray60", lty=1)
text(4.18, 0.1, "(F)", font=2)
abline(h=0, lty=3)

