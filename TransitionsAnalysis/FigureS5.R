#Models of cooperation in pre-historic demographic transitions (China)
#Figure S5

dat1<-(EASIAMOD)
#  Cooperation

modCHINA1<-nls(R1~b-k*exp(X1)-e*exp(-X1), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA1)
cor(predict(modCHINA1), dat1$R1[1:17])^2

modCHINA2<-nls(R2~b-k*exp(X2)-e*exp(-X2), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA2)
cor(predict(modCHINA2), dat1$R2[1:13])^2

modCHINA3<-nls(R3~b-k*exp(X3)-e*exp(-X3), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA3)
cor(predict(modCHINA3), dat1$R3[1:17])^2

modCHINA4<-nls(R4~b-k*exp(X4)-e*exp(-X4), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA4)
cor(predict(modCHINA4), dat1$R4[1:13])^2

modCHINA5<-nls(R5~b-k*exp(X5)-e*exp(-X5), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA5)
cor(predict(modCHINA5), dat1$R5[1:27])^2

modCHINA6<-nls(R6~b-k*exp(X6)-e*exp(-X6), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA6)
cor(predict(modCHINA6), dat1$R6[1:9])^2

modCHINA7<-nls(R7~b-k*exp(X7)-e*exp(-X7), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA7)
cor(predict(modCHINA7), dat1$R7[1:9])^2

modCHINA8<-nls(R8~b-k*exp(X8)-e*exp(-X8), data=dat1, start=c(b=0.3, k=10, e=22), trace=TRUE)
summary(modCHINA8)
cor(predict(modCHINA8), dat1$R8[1:11])^2

par(mar=c(4,5,3,5))
par(mfrow=c(4, 2))

x4a<-seq(0.12,0.9,0.01)
y4a<-predict(modCHINA1, list(X1=x4a))
x4b<-seq(0.88,2.06,0.01)
y4b<-predict(modCHINA2, list(X2=x4b))
x4c<-seq(1.26, 2.0,0.01)
y4c<-predict(modCHINA3, list(X3=x4c))
x4d<-seq(1.97,2.62,0.01)
y4d<-predict(modCHINA4, list(X4=x4d))
x4e<-seq(2.6,3.55,0.01)
y4e<-predict(modCHINA5, list(X5=x4e))
x4f<-seq(3.5,3.66,0.01)
y4f<-predict(modCHINA6, list(X6=x4f))
x4g<-seq(3.575,3.75,0.01)
y4g<-predict(modCHINA7, list(X7=x4g))
x4h<-seq(3.55,3.95,0.01)
y4h<-predict(modCHINA8, list(X8=x4h))



plot(dat1$X1, dat1$R1, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(8910 - 8430 BPy)", cex.main=0.8, ylim=c(-0.02, 0.14), xlim=c(0.08, 0.92))
lines(x4a, y4a, lw=2, col="gray65", lty=1)
text(0.88, 0.13, "(A)", font=2)
abline(h=0, lty=3)

plot(dat1$X2, dat1$R2, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(8280 - 7920 BPy)", cex.main=0.8, ylim=c(-0.02, 0.225), xlim=c(0.8, 2.12))
lines(x4b, y4b, lw=2, col="gray65", lty=1)
text(2.1, 0.21, "(B)", font=2)
abline(h=0, lty=3)

plot(dat1$X3, dat1$R3, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(7080 - 6600 BPy)", cex.main=0.8, ylim=c(-0.02, 0.1), xlim=c(1.2, 2.0))
lines(x4c, y4c, lw=2, col="gray65", lty=1)
text(2.0, 0.095, "(C)", font=2)
abline(h=0, lty=3)

plot(dat1$X4, dat1$R4, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(6570 - 6210 BPy)", cex.main=0.8, ylim=c(-0.02, 0.1), xlim=c(1.95, 2.65))
lines(x4d, y4d, lw=2, col="gray65", lty=1)
text(2.64, 0.095, "(D)", font=2)
abline(h=0, lty=3)

plot(dat1$X5, dat1$R5, type="p", xlab="", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(6030 - 5250 BPy)", cex.main=0.8, ylim=c(-0.02, 0.1), xlim=c(2.6, 3.6))
lines(x4e, y4e, lw=2, col="gray65", lty=1)
text(3.6, 0.0965, "(E)", font=2)
abline(h=0, lty=3)

plot(dat1$X6, dat1$R6, type="p", xlab="", ylab="", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(5160 - 4920 BPy)", cex.main=0.8, ylim=c(-0.02, 0.04), xlim=c(3.5, 3.67))
lines(x4f, y4f, lw=2, col="gray65", lty=1)
text(3.66, 0.037, "(F)", font=2)
abline(h=0, lty=3)

plot(dat1$X7, dat1$R7, type="p", xlab="X(t)", ylab="R(t) = X(t+1) - X(t)", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(4290 - 4050 BPy)", cex.main=0.8, ylim=c(-0.01, 0.03), xlim=c(3.575, 3.75))
lines(x4g, y4g, lw=2, col="gray65", lty=1)
text(3.75, 0.0287, "(G)", font=2)
abline(h=0, lty=3)

plot(dat1$X8, dat1$R8, type="p", xlab="X(t)", ylab="", tck=0.03, bty="l", col="black", cex=.8, pch=19, main="(3780 - 3480 BPy)", cex.main=0.8, ylim=c(-0.01, 0.066), xlim=c(3.55, 3.93))
lines(x4h, y4h, lw=2, col="gray65", lty=1)
text(3.92, 0.06, "(H)", font=2)
abline(h=0, lty=3)


