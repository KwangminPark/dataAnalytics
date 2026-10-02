# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 3.2.5: 평균 이전에 분포 확인
r <- read_data("ch3_data1.csv")
z <- summary_by(r$score,r$region)
export(data.frame(region=rownames(z),z,row.names=NULL),"region-summary")
picture("distributions",1200,800); par(mfrow=c(2,2))
hist(r$score,breaks=5,main="5 requested bins",xlab="Score")
hist(r$score,breaks=20,main="20 requested bins",xlab="Score")
boxplot(score~region,r,ylab="Score")
plot(density(r$score),main="Density (bounded scores)",xlab="Score")
dev.off()
# 2. 3.5: A와 기준 4.0 / A와 B는 서로 다른 질문입니다.
d <- read_data("ch3_data2.csv")
stopifnot(nrow(d)==1000,!anyNA(d),all(table(d$group)==500))
a <- d$score[d$group=="A"]; b <- d$score[d$group=="B"]
one <- t.test(a,mu=4); two <- t.test(a,b)
print(one); print(two); print(var.test(a,b)); print(t.test(a,b,var.equal=TRUE))
export(data.frame(question=c("A minus 4","A minus B"),difference=c(mean(a)-4,mean(a)-mean(b)),low=c(one$conf.int[1]-4,two$conf.int[1]),high=c(one$conf.int[2]-4,two$conf.int[2]),p=c(one$p.value,two$p.value)),"mean-differences")
# 3. 대응자료는 별도 원본이 필요합니다. 다른 사람을 임의로 짝짓지 않습니다.
pair <- read_data("ch3_data3.csv")
stopifnot(nrow(pair)==300,!anyNA(pair),!anyDuplicated(pair$employee_id))
paired <- t.test(pair$after,pair$before,paired=TRUE)
print(paired)
export(data.frame(n=nrow(pair),mean_change=mean(pair$after-pair$before),low=paired$conf.int[1],high=paired$conf.int[2],p=paired$p.value),"paired-change")
picture("paired-change")
plot(pair$before,pair$after,pch=16,col="#236b6655",xlab="Before",ylab="After"); abline(0,1,lty=2); dev.off()
# 4. 심화: 효과 크기와 실무 기준을 함께 읽기
pooled_sd <- sqrt(((length(a)-1)*var(a)+(length(b)-1)*var(b))/(length(a)+length(b)-2))
export(data.frame(difference=mean(a)-mean(b),cohens_d=(mean(a)-mean(b))/pooled_sd,discussion_threshold=0.2),"effect-size")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
