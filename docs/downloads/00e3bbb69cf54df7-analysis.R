# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 5.5: 두 요인의 조합별 표본 수와 평균부터 확인
raw <- read_data("ch5_data2.csv")
d <- data.frame(type=factor(raw$매장유형,levels=c("가맹점","직영점")),day=factor(raw$요일,levels=c("평일","주말")),sales=raw$일매출)
stopifnot(nrow(d)==520,!anyNA(d))
print(table(d$type,d$day))
z <- summary_by(d$sales,interaction(d$type,d$day))
export(data.frame(cell=rownames(z),z,row.names=NULL),"day-type-means")
full <- aov(sales~type*day,data=d)
print(summary(full))
# 불균형 자료의 순차 제곱합은 입력 순서에 영향을 받습니다.
print(summary(aov(sales~day*type,data=d)))
print(anova(lm(sales~type+day,data=d),lm(sales~type*day,data=d)))
# 2. 5.6: 주효과가 작아도 조합 차이는 클 수 있습니다.
e <- read_data("ch5_data3.csv")
e$type <- factor(e$매장유형); e$time <- factor(e$시간대,levels=c("점심","저녁"))
print(summary(aov(고객당매출~time*type,data=e)))
z <- summary_by(e$고객당매출,interaction(e$type,e$time))
export(data.frame(cell=rownames(z),z,row.names=NULL),"time-type-means")
picture("interactions",1200,650); par(mfrow=c(1,2))
interaction.plot(d$day,d$type,d$sales,main="5.5: weekday / weekend",ylab="Sales (KRW)",xlab="Day",lwd=2)
interaction.plot(e$time,e$type,e$고객당매출,main="5.6: lunch / dinner",ylab="Spend (KRW)",xlab="Time",lwd=2)
dev.off()
# 3. 심화: 주말 증가액의 차이(차이의 차이)를 직접 계산
means <- tapply(d$sales,list(d$type,d$day),mean)
weekend_gain <- means[,"주말"]-means[,"평일"]
export(data.frame(type=names(weekend_gain),weekend_gain=as.numeric(weekend_gain)),"weekend-gain")
cat("Difference of gains (franchise minus owned):",weekend_gain["가맹점"]-weekend_gain["직영점"],"
")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
