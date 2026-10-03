# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 6.10: tips는 R 기본 내장자료가 아닌 reshape2 배포자료입니다.
d <- read_data("tips.csv"); stopifnot(nrow(d)==244,!anyNA(d))
d$day <- factor(d$day,levels=c("Sun","Thur","Fri","Sat"))
print(summary(d)); print(table(d$day))
simple <- lm(tip~total_bill,data=d)
model <- lm(tip~total_bill+size+day,data=d)
expanded <- lm(tip~total_bill+size+day+sex+smoker+time,data=d)
export(coef_table(model),"coefficients")
export(data.frame(model=c("simple","chapter-model","expanded"),r2=sapply(list(simple,model,expanded),function(m) summary(m)$r.squared),adjusted_r2=sapply(list(simple,model,expanded),function(m) summary(m)$adj.r.squared)),"fit-comparison")
picture("bill-tip")
plot(d$total_bill,d$tip,pch=16,col="#236b6660",xlab="Total bill (USD)",ylab="Tip (USD)")
abline(simple,col="#c26c35",lwd=2); dev.off()
# 2. 잔차 진단과 다중공선성. 범주형은 더미열별 R제곱으로 확인합니다.
picture("diagnostics",1200,850); par(mfrow=c(2,2)); plot(model); dev.off()
X <- model.matrix(model)
vifs <- sapply(2:ncol(X),function(j) 1/(1-summary(lm(X[,j]~X[,-c(1,j),drop=FALSE]))$r.squared))
export(data.frame(term=colnames(X)[-1],vif=vifs),"dummy-vif")
# 3. 교재 장말 과제: 이분산에 민감한 표준오차를 HC3와 비교
bread <- solve(crossprod(X)); weights <- residuals(model)^2/(1-hatvalues(model))^2
hc3 <- bread %*% crossprod(X,X*weights) %*% bread
se <- sqrt(diag(hc3)); tcrit <- qt(.975,df.residual(model))
export(data.frame(term=names(coef(model)),estimate=coef(model),usual_se=coef(summary(model))[,2],hc3_se=se,hc3_low=coef(model)-tcrit*se,hc3_high=coef(model)+tcrit*se),"hc3")
# 4. 심화: 평균의 신뢰구간과 한 테이블의 예측구간은 다릅니다.
new <- data.frame(total_bill=20,size=2,day=factor("Sun",levels=levels(d$day)))
export(data.frame(interval=c("mean-confidence","individual-prediction"),rbind(predict(model,new,interval="confidence"),predict(model,new,interval="prediction"))),"prediction-intervals")
cat("Intervals above use the ordinary lm assumptions; inspect heteroskedasticity before relying on their width.
")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
