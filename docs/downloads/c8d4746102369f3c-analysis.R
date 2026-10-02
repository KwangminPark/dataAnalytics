# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 교재 4.4의 동일 자료: 브랜드별 30일, 총 90행
raw <- read_data("ch4_data.csv")
stopifnot(nrow(raw)==90,!anyNA(raw))
d <- data.frame(date=as.Date(raw$날짜),brand=factor(raw$브랜드),sales=raw$일매출,visitors=raw$방문객수,buyers=raw$구매고객수)
print(table(d$brand)); print(table(d$date)); stopifnot(all(d$buyers<=d$visitors))
z <- summary_by(d$sales,d$brand)
export(data.frame(brand=rownames(z),z,row.names=NULL),"brand-summary")
# 2. 모든 집단이 같은가? 전체 검정 다음에 쌍별 차이를 봅니다.
m <- aov(sales~brand,data=d)
print(summary(m)); post <- TukeyHSD(m)$brand
export(data.frame(comparison=rownames(post),post,check.names=FALSE,row.names=NULL),"tukey")
# 분포와 잔차. 좋은 그림은 관측의 독립성을 증명하지 않습니다.
picture("anova-check",1200,800); par(mfrow=c(2,2))
boxplot(sales~brand,d,ylab="Daily sales (KRW)",names=c("Mega","Starbucks","Compose"))
plot(fitted(m),resid(m),xlab="Fitted sales",ylab="Residual"); abline(h=0,lty=2)
qqnorm(resid(m)); qqline(resid(m))
plot(TukeyHSD(m),las=1,cex.axis=0.7)
dev.off()
cat("Residual normality diagnostic:
"); print(shapiro.test(resid(m)))
cat("Welch ANOVA sensitivity:
"); print(oneway.test(sales~brand,data=d,var.equal=FALSE))
# 3. 심화: 매출 차이를 바로 브랜드 효과라고 부르기 전에 구매 비율 확인
x <- aggregate(cbind(sales,visitors,buyers)~brand,d,sum)
x$purchase_rate <- x$buyers/x$visitors
x$sales_per_buyer <- x$sales/x$buyers
export(x,"business-check")
cat("30 repeated days per brand/store; inference assumes independence not guaranteed by these rows.
")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
