# 결과를 output 폴더에 저장합니다. 원본 data 파일은 바꾸지 않습니다.
options(digits=7)
dir.create("output",showWarnings=FALSE)
read_data <- function(name) read.csv(file.path("data",name),fileEncoding="UTF-8-BOM",check.names=FALSE,stringsAsFactors=FALSE)
export <- function(x,name) { print(x); write.csv(x,file.path("output",paste0(name,".csv")),row.names=FALSE) }
coef_table <- function(m) { z <- coef(summary(m)); ci <- confint(m); data.frame(term=rownames(z),estimate=z[,1],se=z[,2],p=z[,4],low=ci[,1],high=ci[,2],row.names=NULL) }
summary_by <- function(x,g) do.call(rbind,lapply(split(x,g),function(y) data.frame(n=length(y),mean=mean(y),median=median(y),sd=sd(y),IQR=IQR(y))))
picture <- function(name,w=1100,h=720) png(file.path("output",paste0(name,".png")),width=w,height=h,res=120)
sink("output/results.txt",split=TRUE)
# 1. 먼저 질문: 사람이 더 필요한 날을 방문객 수만으로 고를 수 있을까?
d <- read_data("decision-example.csv")
stopifnot(nrow(d)==2,all(d$buyers<=d$visitors))
print(d)
# 2. 분자와 분모를 읽으며 계산하기. 이익은 비용자료가 없어 계산할 수 없습니다.
d$buy_rate <- d$buyers/d$visitors
d$sales_per_buyer <- d$sales/d$buyers
d$sales_per_labor_hour <- d$sales/d$labor_hours
export(d,"decision-table")
# 3. 설치 확인: 표와 그림을 파일로 만들 수 있나요?
picture("first-chart")
barplot(d$sales,names.arg=d$day,ylab="Sales (KRW)",col=c("#236b66","#db934b"),main="Two illustrative days: not a staffing experiment")
dev.off()
cat("Friday/Saturday are a constructed teaching example. Labor hours are not labor cost.
")

sink()
capture.output(sessionInfo(),file="output/session-info.txt")
