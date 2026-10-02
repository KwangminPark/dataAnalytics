# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 2.8.3~2.8.5: 숫자로 저장된 코드와 실제 수량 구분하기
cars <- read_data("mtcars.csv")
stopifnot(nrow(cars)==32)
str(cars)
print(table(cars$am))
cars$am <- factor(cars$am,levels=c(0,1),labels=c("automatic","manual"))
cars$cyl_group <- factor(cars$cyl)
export(data.frame(type=names(table(cars$am)),n=as.vector(table(cars$am)),proportion=as.vector(prop.table(table(cars$am)))),"transmission")
z <- summary_by(cars$mpg,cars$cyl_group)
export(data.frame(cyl=rownames(z),z,row.names=NULL),"mpg-by-cyl")
# 교재의 p값 읽기 활동입니다. ANOVA의 원리와 사후검정은 4장에서 배웁니다.
print(summary(aov(mpg~cyl_group,data=cars)))
# 2. 2.8.6: 외식자료에서 분석할 수 있는 질문과 필요한 변수 찾기
d <- read_data("ch2_data.csv")
stopifnot(nrow(d)==500,!anyNA(d))
str(d); print(summary(d))
export(data.frame(variable=names(d),r_class=sapply(d,class),missing=colSums(is.na(d))),"variable-audit")
z <- summary_by(d$avg_spend,d$location)
export(data.frame(location=rownames(z),z,row.names=NULL),"spend-by-location")
print(table(d$membership,d$satisfaction))
print(prop.table(table(d$membership,d$satisfaction),margin=1))
print(summary(aov(avg_spend~factor(location),data=d)))
cat("visit_time / avg_spend correlation:",cor(d$visit_time,d$avg_spend),"
")
picture("variables",1200,800); par(mfrow=c(2,2))
boxplot(mpg~cyl_group,cars,xlab="Cylinders (groups)",ylab="MPG")
barplot(table(d$membership),ylab="Rows")
boxplot(satisfaction~membership,d,ylab="Satisfaction",xlab="Membership")
plot(d$visit_time,d$avg_spend,xlab="Visit time (min)",ylab="Average spend (KRW)",pch=16,col="#236b6655")
dev.off()
# 3. 심화: 전체에서 VIP가 차지하는 비율과 VIP 중 4점 이상 비율은 다릅니다.
export(data.frame(question=c("VIP share of all rows","Score >= 4 among VIP"),numerator=c(sum(d$membership=="VIP"),sum(d$membership=="VIP" & d$satisfaction>=4)),denominator=c(nrow(d),sum(d$membership=="VIP"))),"denominators")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
