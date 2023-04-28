
#Plotting the results of strucchange package (breakpoints)

par(mfrow=c(4, 2))
par(mar=c(5,6,3,5))


plot(spdts, col="black", lwd=1, tck=-0.025, ylab="SPD", xlab="", main="Middle East", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm0), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm1), start=0), col = "red", lty=3, lwd=2)
lines(bp.agr)
text(340, 40, "(A)", font=2)
arrows(95, 20, 95, 10, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci.agr <- confint(bp.agr)
lines(ci.agr)

plot(spdts1, col="black", lwd=1, tck=-0.025, ylab="", xlab="", main="Europe", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm2), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm3), start=0), col = "red", lty=3, lwd=2)
lines(bp1.agr)
text(469, 355, "(B)", font=2)
arrows(220, 160, 220, 60, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci1.agr <- confint(bp1.agr)
lines(ci1.agr)

plot(spdts2, col="black", lwd=1, tck=-0.025, ylab="SPD", xlab="", main="East Asia (China)", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm4), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm5), start=0), col = "red", lty=3, lwd=2)
lines(bp2.agr)
text(393, 48, "(C)", font=2)
arrows(220, 22, 220, 6, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci2.agr <- confint(bp2.agr)
lines(ci2.agr)

plot(spdts3, col="black", lwd=1, tck=-0.025, ylab="", xlab="", main="South America", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm6), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm7), start=0), col = "red", lty=3, lwd=2)
lines(bp3.agr)
text(480, 38, "(D)", font=2)
arrows(270, 22, 270, 14, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci3.agr <- confint(bp3.agr)
lines(ci3.agr)

plot(spdts4, col="black", lwd=1, tck=-0.025, ylab="SPD", xlab="", main="South Africa", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm8), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm9), start=0), col = "red", lty=3, lwd=2)
lines(bp4.agr)
text(449, 50, "(E)", font=2)
arrows(350, 18, 350, 6, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci4.agr <- confint(bp4.agr)
lines(ci4.agr)

plot(spdts5, col="black", lwd=1, tck=-0.025, ylab="", xlab="", main="West Africa", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm10), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm11), start=0), col = "red", lty=3, lwd=2)
lines(bp5.agr)
text(350, 30, "(F)", font=2)
arrows(190, 14, 190, 4, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci5.agr <- confint(bp5.agr)
lines(ci5.agr)

plot(spdts6, col="black", lwd=1, tck=-0.025, ylab="SPD", xlab="Time", main="North America", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm12), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm13), start=0), col = "red", lty=3, lwd=2)
lines(bp6.agr)
text(440, 870, "(G)", font=2)
arrows(310, 290, 310, 100, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)


plot(spdts7, col="black", lwd=1, tck=-0.025, ylab="", xlab="Time", main="Australia", tck=0.03, cex.main=0.9)
lines(ts(fitted(fm14), start = 0), col = "blue", lty=2)
lines(ts(fitted(fm15), start=0), col = "red", lty=3, lwd=2)
lines(bp7.agr)
text(380, 65, "(H)", font=2)
arrows(260, 30, 260, 10, length = 0.075, angle = 30,code = 2, col = "red", lwd=3)

## confidence interval
ci7.agr <- confint(bp7.agr)
lines(ci7.agr)



