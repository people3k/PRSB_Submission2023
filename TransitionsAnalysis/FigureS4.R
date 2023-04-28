#Models of cooperation in pre-historic demographic transitions (Europa)
# Figure S4


dat1<-(EURMOD)

#  Cooperation

modEUR1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR1)
cor(predict(modEUR1), dat1$R1[1:13])^2

modEUR2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR2)
cor(predict(modEUR2), dat1$R2[1:10])^2

modEUR3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR3)
cor(predict(modEUR3), dat1$R3[1:16])^2

modEUR4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR4)
cor(predict(modEUR4), dat1$R4[1:14])^2

modEUR5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR5)
cor(predict(modEUR5), dat1$R5[1:11])^2

modEUR6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR6)
cor(predict(modEUR6), dat1$R6[1:13])^2

modEUR7<-nls(R7~b-k*exp(X7)-e*exp(-X7), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR7)
cor(predict(modEUR7), dat1$R7[1:9])^2

modEUR8<-nls(R8~b-k*exp(X8)-e*exp(-X8), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR8)
cor(predict(modEUR8), dat1$R8[1:16])^2

modEUR9<-nls(R9~b-k*exp(X9)-e*exp(-X9), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR9)
cor(predict(modEUR9), dat1$R9[1:11])^2

modEUR10<-nls(R10~b-k*exp(X10)-e*exp(-X10), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modEUR10)
cor(predict(modEUR10), dat1$R10[1:11])^2

par(mar=c(4,5,2,4))
par(mfrow=c(5, 2))

x3a<-seq(3.78,4.12,0.01)
y3a<-predict(modEUR1, list(X1=x3a))
x3b<-seq(4.1,4.36,0.01)
y3b<-predict(modEUR2, list(X2=x3b))
x3c<-seq(4.33, 4.9,0.01)
y3c<-predict(modEUR3, list(X3=x3c))
x3d<-seq(4.76,4.98,0.01)
y3d<-predict(modEUR4, list(X4=x3d))
x3e<-seq(5.25,5.855,0.01)
y3e<-predict(modEUR5, list(X5=x3e))
x3f<-seq(5.235,5.42,0.01)
y3f<-predict(modEUR6, list(X6=x3f))
x3g<-seq(5.34,5.6,0.01)
y3g<-predict(modEUR7, list(X7=x3g))
x3h<-seq(5.52,5.85,0.01)
y3h<-predict(modEUR8, list(X8=x3h))
x3i<-seq(5.2,5.7,0.01)
y3i<-predict(modEUR9, list(X9=x3i))
x3j<-seq(5.48,5.88,0.01)
y3j<-predict(modEUR10, list(X10=x3j))



plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="8220 - 7860 BPy", cex.main=0.8, ylim=c(-0.015, 0.058), xlim=c(3.75,4.12))
lines(x3a, y3a, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(4.1, 0.05, "(A)", font=2)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="7830 - 7560 BPy", cex.main=0.8, ylim=c(-0.015, 0.045), xlim=c(4.1,4.35))
lines(x3b, y3b, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(4.35, 0.037, "(B)", font=2)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="7530 - 7080 BPy", cex.main=0.8, ylim=c(-0.015, 0.075), xlim=c(4.32,4.9))
lines(x3c, y3c, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(4.9, 0.065, "(C)", font=2)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="6840 - 6450 BPy", cex.main=0.8, ylim=c(-0.015, 0.045), xlim=c(4.75, 5))
lines(x3d, y3d, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5, 0.035, "(D)", font=2)

plot(dat1$X5, dat1$R5, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main=" 5880 - 5580 BPy", cex.main=0.8, ylim=c(-0.015, 0.12), xlim=c(5.22, 5.85))
lines(x3e, y3e, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.85, 0.11, "(E)", font=2)

plot(dat1$X6, dat1$R6, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="5160 - 4800 BPy", cex.main=0.8, ylim=c(-0.015, 0.08), xlim=c(5.22, 5.425))
lines(x3f, y3f, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.42, 0.075, "(F)", font=2)

plot(dat1$X7, dat1$R7, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="4680 - 4440 BPy", cex.main=0.8, ylim=c(-0.015, 0.065), xlim=c(5.32, 5.6))
lines(x3g, y3g, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.6, 0.0585, "(G)", font=2)

plot(dat1$X8, dat1$R8, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="4290 - 3840 BPy", cex.main=0.8, ylim=c(-0.015, 0.035), xlim=c(5.5, 5.85))
lines(x3h, y3h, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.84, 0.032, "(H)", font=2)

plot(dat1$X9, dat1$R9, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="2580 - 2280 BPy", cex.main=0.8, ylim=c(-0.015, 0.12), xlim=c(5.2, 5.7))
lines(x3i, y3i, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.7, 0.11, "(I)", font=2)

plot(dat1$X10, dat1$R10, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", pch=19, cex=0.8, main="1620 - 1320 BPy", cex.main=0.8, ylim=c(-0.015, 0.07), xlim=c(5.46, 5.88))
lines(x3j, y3j, lw=2, col="gray65", lty=1)
abline(h=0, lty=3)
text(5.87, 0.065, "(J)", font=2)



abline(h=0, lty=3)

