#Models of cooperation in pre-historic demographic transitions (South Africa)


dat1<-(MODSAFR)
#  Cooperation

modSAFR1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR1)
cor(predict(modSAFR1), dat1$R1[1:13])^2

modSAFR2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR2)
cor(predict(modSAFR2), dat1$R2[1:8])^2

modSAFR3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR3)
cor(predict(modSAFR3), dat1$R3[1:14])^2

modSAFR4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR4)
cor(predict(modSAFR4), dat1$R4[1:8])^2

modSAFR5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR5)
cor(predict(modSAFR5), dat1$R5[1:11])^2

modSAFR6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR6)
cor(predict(modSAFR6), dat1$R6[1:11])^2

modSAFR7<-nls(R7~b-k*exp(X7)-e*exp(-X7), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR7)
cor(predict(modSAFR7), dat1$R7[1:13])^2

modSAFR8<-nls(R8~b-k*exp(X8)-e*exp(-X8), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modSAFR8)
cor(predict(modSAFR8), dat1$R8[1:14])^2


par(mar=c(4,5,3,5))
par(mfrow=c(4, 2))

x6a<-seq(0.8,1.335,0.01)
y6a<-predict(modSAFR1, list(X1=x6a))
x6b<-seq(1.26, 1.5,0.01)
y6b<-predict(modSAFR2, list(X2=x6b))
x6c<-seq(1.48, 1.81,0.01)
y6c<-predict(modSAFR3, list(X3=x6c))
x6d<-seq(1.69, 1.97,0.01)
y6d<-predict(modSAFR4, list(X4=x6d))
x6e<-seq(1.98, 2.47,0.01)
y6e<-predict(modSAFR5, list(X5=x6e))
x6f<-seq(2.44, 3.16,0.01)
y6f<-predict(modSAFR6, list(X6=x6f))
x6g<-seq(3.05, 3.58,0.01)
y6g<-predict(modSAFR7, list(X7=x6g))
x6h<-seq(3.32, 3.85,0.01)
y6h<-predict(modSAFR8, list(X8=x6h))


plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4710 - 4350 BPy)", cex.main=0.8, ylim=c(-0.015, 0.075), xlim=c(0.8, 1.32))
lines(x6a, y6a, lw=2, col="gray65", lty=1)
text(1.32, 0.07, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(4080 - 3870 BPy)", cex.main=0.8, ylim=c(-0.015, 0.05), xlim=c(1.26, 1.48))
lines(x6b, y6b, lw=2, col="gray65", lty=1)
text(1.47, 0.047, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3750 - 3360 BPy)", cex.main=0.8, ylim=c(-0.015, 0.085), xlim=c(1.47, 1.82))
lines(x6c, y6c, lw=2, col="gray65", lty=1)
text(1.82, 0.082, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3240 - 3030 BPy)", cex.main=0.8, ylim=c(-0.015, 0.065), xlim=c(1.68, 1.97))
lines(x6d, y6d, lw=2, col="gray65", lty=1)
text(1.96, 0.062, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(3000 - 2700 BPy)", cex.main=0.8, ylim=c(-0.015, 0.088), xlim=c(1.98, 2.47))
lines(x6e, y6e, lw=2, col="gray65", lty=1)
text(2.46, 0.08, "(E)", font=2)
abline(h=0, lty=3)


plot(dat1$X6, dat1$R6, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2580 - 2280 BPy)", cex.main=0.8, ylim=c(-0.015, 0.16), xlim=c(2.44, 3.16))
lines(x6f, y6f, lw=2, col="gray65", lty=1)
text(3.14, 0.15, "(F)", font=2)
abline(h=0, lty=3)

plot(dat1$X7, dat1$R7, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(2040 - 1680 BPy)", cex.main=0.8, ylim=c(-0.015, 0.07), xlim=c(3.05, 3.58))
lines(x6g, y6g, lw=2, col="gray65", lty=1)
text(3.56, 0.065, "(G)", font=2)
abline(h=0, lty=3)

plot(dat1$X8, dat1$R8, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="(1050 - 660 BPy)", cex.main=0.8, ylim=c(-0.015, 0.075), xlim=c(3.32, 3.86))
lines(x6h, y6h, lw=2, col="gray65", lty=1)
text(3.84, 0.065, "(H)", font=2)
abline(h=0, lty=3)

