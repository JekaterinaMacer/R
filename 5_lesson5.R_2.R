mypar(1,1)
head(.tidy_set)
dim(.tidy_set)
set.seed(1)
ind<-sample(seq(1,nrow(.tidy_set)),100)
ind
ts<-tidy_set[ind,]
head(ts)
dim(ts)
plot(ts$ap_lo,ts$ap_hi,xlim = c(0,120),ylim = c(0,200), col="red",lwd=2)

fitm<-lm(ts$ap_hi~ts$ap_lo)
fitm
hi_hat<- 12.554 + 1.411*ts$ap_lo
hi_hat

as.numeric(predict(fitm,ts))

?predict

signif(fitm$coefficients,10)

plot(seq(1:length(ts$ap_hi)),ts$ap_hi, col="red", type = "l")
lines(seq(1:length(hi_hat)),hi_hat, col="blue",type = "l")
legend("topleft", c("ts$ap_i","hi_hat"), col=c(2,3), lty=c(1,1), lwd=2)

summary(fitm)

mypar(1,2)
qqnorm(fitm$residuals)
qqline(fitm$residuals)
boxplot(fitm$residuals)

rse <- sqrt( sum((fitm$residuals)^2) / fitm$df.residual )
rse


Rs<-(cor(ts$ap_hi,ts$ap_lo))^2
Rs
R.adj<- 1-((1-Rs)*((100-1)/(100-1-1)))
R.adj
