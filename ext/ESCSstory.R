library(PISA2003lite)
library(ggplot2)

student2003g <- na.omit(student2003[student2003$CNT == "DEU",c("ESCS","W_FSTUWT")])
student2003p <- na.omit(student2003[student2003$CNT == "POL",c("ESCS","W_FSTUWT")])

dp2003 <- density(student2003p$ESCS, weights=student2003p$W_FSTUWT, bw=.1, from=-3, to=3)
dg2003 <- density(student2003g$ESCS, weights=student2003g$W_FSTUWT, bw=.1, from=-3, to=3)

library(PISA2009lite)
student2009g <- na.omit(student2009[student2009$CNT == "Germany",c("ESCS","W_FSTUWT")])
student2009p <- na.omit(student2009[student2009$CNT == "Poland",c("ESCS","W_FSTUWT")])

dp2009 <- density(student2009p$ESCS, weights=student2009p$W_FSTUWT, bw=.1, from=-3, to=3)
dg2009 <- density(student2009g$ESCS, weights=student2009g$W_FSTUWT, bw=.1, from=-3, to=3)

library(PISA2012lite)
student2012g <- na.omit(student2012[student2012$CNT == "Germany",c("ESCS","W_FSTUWT")])
student2012p <- na.omit(student2012[student2012$CNT == "Poland",c("ESCS","W_FSTUWT")])

dp2012 <- density(student2012p$ESCS, weights=student2012p$W_FSTUWT, bw=.1, from=-3, to=3)
dg2012 <- density(student2012g$ESCS, weights=student2012g$W_FSTUWT, bw=.1, from=-3, to=3)



#
# graphs
#


png("POL2003.png", 720, 300)
par(mar=c(5,0,5,0))
plot(dp2003$x, dp2003$y/sum(dp2003$y), type="s", col="black", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Polska 2003")
dev.off()
png("POL2009.png", 720, 300)
par(mar=c(5,0,5,0))
plot(dp2009$x, dp2009$y/sum(dp2009$y), type="s", col="black", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Polska 2009")
dev.off()
png("POL2012.png", 720, 300)
par(mar=c(5,0,5,0))
plot(dp2012$x, dp2012$y/sum(dp2012$y), type="s", col="black", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Polska 2012")
dev.off()



png("POL2012.png", 720, 300)
par(mar=c(5,0,5,0))
plot(dp2009$x, dp2009$y/sum(dp2009$y), type="s", col="grey40", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Polska 2003 / 2009 / 2012", ylim=c(0,0.008))
lines(dp2003$x, dp2003$y/sum(dp2003$y), type="s", col="grey", lwd=2)
lines(dp2012$x, dp2012$y/sum(dp2012$y), type="s", col="black", lwd=2)
legend("topleft", c("2003", "2009", "2012"), col=c("grey40", "grey", "black"), lwd=3, bty="n")


plot(dg2009$x, dg2009$y/sum(dg2009$y), type="s", col="#aa000070", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Niemcy 2003 / 2009 / 2012", ylim=c(0,0.006))
lines(dg2003$x, dg2003$y/sum(dg2003$y), type="s", col="#aa000020", lwd=2)
lines(dg2012$x, dg2012$y/sum(dg2012$y), type="s", col="#aa0000ff", lwd=2)
legend("topleft", c("2003", "2009", "2012"), col=c("#aa000020", "#aa000070", "#aa0000ff"), lwd=3, bty="n")



plot(dp2009$x, dp2009$y/sum(dp2009$y), type="s", col="grey40", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Polska 2003 / 2009 / 2012", ylim=c(0,0.008))
lines(dp2003$x, dp2003$y/sum(dp2003$y), type="s", col="grey", lwd=2)
lines(dp2012$x, dp2012$y/sum(dp2012$y), type="s", col="black", lwd=2)
legend("topleft", c("2003", "2009", "2012"), col=c("grey40", "grey", "black"), lwd=3, bty="n")


plot(dg2009$x, dg2009$y/sum(dg2009$y), type="s", col="#aa000070", xlab="ESCS", bty="n", yaxt="n", ylab="", lwd=2, main="Niemcy i Polska 2003 / 2009 / 2012", ylim=c(0,0.008))
lines(dg2003$x, dg2003$y/sum(dg2003$y), type="s", col="#aa000020", lwd=2)
lines(dg2012$x, dg2012$y/sum(dg2012$y), type="s", col="#aa0000ff", lwd=2)
lines(dp2009$x, dp2009$y/sum(dp2009$y), type="s", col="grey40", lwd=2)
lines(dp2003$x, dp2003$y/sum(dp2003$y), type="s", col="grey", lwd=2)
lines(dp2012$x, dp2012$y/sum(dp2012$y), type="s", col="black", lwd=2)

