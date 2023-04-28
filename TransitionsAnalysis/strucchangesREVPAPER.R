## SPD data with breakpoints: Innovations and Agriculture 
# Use the package strucchange

par(mfrow=c(4, 2))
par(mar=c(5,6,3,5))

data<-(WESTASIA30)
spdts<-ts(data$SPD, start=c(1, 1), end=c(339, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs.agr <- Fstats(spdts ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs.agr)
#lines(breakpoints(fs.agr))

## or
bp.agr <- breakpoints(spdts ~ 1, breaks = 1)
summary(bp.agr)


## fit null hypothesis model and model with 1 breakpoint
fm0 <- lm(spdts ~ 1)
fm1 <- lm(spdts ~ breakfactor(bp.agr, breaks = 1))
plot(spdts, col="black", lwd=2, tck=-0.025, ylab="SPD", xlab="", main="Near East", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm0), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm1), start=0), col = "blue", lty=3, lwd=2)
lines(bp.agr)
text(340, 40, "(A)", font=2)
arrows(95, 20, 95, 10, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci.agr <- confint(bp.agr)
lines(ci.agr)

sctest(fs.agr)

data1<-(EUR30)
spdts1<-ts(data1$SPD, start=c(1, 1), end=c(467, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs1.agr <- Fstats(spdts1 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs1.agr)
lines(breakpoints(fs1.agr))

## or
bp1.agr <- breakpoints(spdts1 ~ 1, breaks = 1)
summary(bp1.agr)


## fit null hypothesis model and model with 1 breakpoint
fm2 <- lm(spdts1 ~ 1)
fm3 <- lm(spdts1 ~ breakfactor(bp1.agr, breaks = 1))
plot(spdts1, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="Europe", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm2), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm3), start=0), col = "blue", lty=3, lwd=2)
lines(bp1.agr)
text(469, 355, "(B)", font=2)
arrows(220, 160, 220, 60, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci1.agr <- confint(bp1.agr)
lines(ci1.agr)

data2<-(EASTASIA30)
spdts2<-ts(data2$SPD, start=c(1, 1), end=c(389, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs2.agr <- Fstats(spdts2 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs2.agr)
lines(breakpoints(fs2.agr))

## or
#bp2.agr <- breakpoints(spdts2 ~ 1, breaks = 1)
#summary(bp2.agr)


## fit null hypothesis model and model with 1 breakpoint
fm4 <- lm(spdts2 ~ 1)
fm5 <- lm(spdts2 ~ breakfactor(bp2.agr, breaks = 1))
plot(spdts2, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="East Asia (China)", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm4), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm5), start=0), col = "blue", lty=3, lwd=2)
lines(bp2.agr)
text(393, 48, "(C)", font=2)
arrows(250, 22, 250, 6, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci2.agr <- confint(bp2.agr)
lines(ci2.agr)

data3<-(SOUTHAM30)
spdts3<-ts(data3$SPD, start=c(1, 1), end=c(471, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs3.agr <- Fstats(spdts3 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs3.agr)
lines(breakpoints(fs3.agr))

## or
#bp3.agr <- breakpoints(spdts3 ~ 1, breaks = 1)
#summary(bp3.agr)


## fit null hypothesis model and model with 1 breakpoint
fm6 <- lm(spdts3 ~ 1)
fm7 <- lm(spdts3 ~ breakfactor(bp3.agr, breaks = 1))
plot(spdts3, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="South America", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm6), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm7), start=0), col = "blue", lty=3, lwd=2)
lines(bp3.agr)
text(449, 22, "(D)", font=2)
arrows(270, 25, 270, 14, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci3.agr <- confint(bp3.agr)
lines(ci3.agr)

data4<-(SOUTHAFR30)
spdts4<-ts(data4$SPD, start=c(1, 1), end=c(473, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs4.agr <- Fstats(spdts4 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs4.agr)
lines(breakpoints(fs4.agr))

## or
bp4.agr <- breakpoints(spdts4 ~ 1, breaks = 1)
summary(bp4.agr)


## fit null hypothesis model and model with 1 breakpoint
fm8 <- lm(spdts4 ~ 1)
fm9 <- lm(spdts4 ~ breakfactor(bp4.agr, breaks = 1))
plot(spdts4, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="South Africa", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm8), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm9), start=0), col = "blue", lty=3, lwd=2)
lines(bp4.agr)
text(449, 50, "(E)", font=2)
arrows(350, 18, 350, 6, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci4.agr <- confint(bp4.agr)
lines(ci4.agr)


data5<-(WESTAFR30)
spdts5<-ts(data5$SPD, start=c(1, 1), end=c(371, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs5.agr <- Fstats(spdts5 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs5.agr)
lines(breakpoints(fs5.agr))

## or
bp5.agr <- breakpoints(spdts5 ~ 1, breaks = 1)
summary(bp5.agr)


## fit null hypothesis model and model with 1 breakpoint
fm10 <- lm(spdts5 ~ 1)
fm11 <- lm(spdts5 ~ breakfactor(bp5.agr, breaks = 1))
plot(spdts5, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="West Africa", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm10), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm11), start=0), col = "blue", lty=3, lwd=2)
lines(bp5.agr)
text(350, 30, "(F)", font=2)
arrows(190, 14, 190, 4, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci5.agr <- confint(bp5.agr)
lines(ci5.agr)


data6<-(MNAM30)
spdts6<-ts(data6$SPD, start=c(1, 1), end=c(470, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs6.agr <- Fstats(spdts6 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs6.agr)
lines(breakpoints(fs6.agr))

## or
bp6.agr <- breakpoints(spdts6 ~ 1, breaks = 1)
summary(bp6.agr)


## fit null hypothesis model and model with 1 breakpoint
fm12 <- lm(spdts6 ~ 1)
fm13 <- lm(spdts6 ~ breakfactor(bp6.agr, breaks = 1))
plot(spdts6, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="North America", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm12), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm13), start=0), col = "blue", lty=3, lwd=2)
lines(bp6.agr)
text(440, 870, "(G)", font=2)
arrows(310, 290, 310, 100, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
#ci6.agr <- confint(bp6.agr)
#lines(ci6.agr)

data7<-(AUST30)
spdts7<-ts(data7$SPD, start=c(1, 1), end=c(400, 1), frequency=1)
#plot(spdts, type="l")

## F statistics indicates breakpoints
fs7.agr <- Fstats(spdts7 ~ 1)
#plot(fs.agr, alpha=0.01)
breakpoints(fs7.agr)
lines(breakpoints(fs7.agr))

## or
bp7.agr <- breakpoints(spdts7 ~ 1, breaks = 1)
summary(bp7.agr)


## fit null hypothesis model and model with 1 breakpoint
fm14 <- lm(spdts7 ~ 1)
fm15 <- lm(spdts7 ~ breakfactor(bp7.agr, breaks = 1))
plot(spdts7, col="black", lwd=2, tck=-0.025, ylab="", xlab="", main="Australia", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm14), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm15), start=0), col = "blue", lty=3, lwd=2)
lines(bp7.agr)
text(380, 65, "(H)", font=2)
arrows(260, 30, 260, 10, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci7.agr <- confint(bp7.agr)
lines(ci7.agr)

breakdates(bp.agr)


