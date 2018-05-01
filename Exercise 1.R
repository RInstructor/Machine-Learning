#### Machine Learning Exercise 1 #####

# 1) 데이터 불러오기
require(readr)
USvideos <- data.frame(read_csv("C:/Users/class desk/Desktop/USvideos.csv"))

# 컨텐츠의 고유여부 및 카테고리 조합 확인
length(unique(USvideos$video_id))
length(unique(USvideos$category_id))

# 기초 탐색
str(USvideos)
summary(USvideos)
names(USvideos)
head(USvideos)

# 범주형 변수를 factor 유형으로 변환해 빈도 파악
USvideos_sub1<-apply(USvideos[,13:15],2,function(x) as.factor(x))
summary(USvideos_sub1)
summary(apply(USvideos[,13:15],2,function(x) as.factor(x)))

# str(as.Date('2017-12-30'))
# df$var1<-(df$var1+1)

# 날짜 데이터 정리
USvideos$trending_date<-as.Date(USvideos$trending_date,'%y.%d.%m')
USvideos$publish_time<-as.Date(USvideos$publish_time)

#2) 카테고리 정보 병합
require(jsonlite)
category<-fromJSON("US_category_id.json",flatten = T)
category1<-category$items
# category[[3]]

category2<-category1[,c("id","snippet.title")]
names(category2)[1]<-"category_id"
names(USvideos)

# USvideos<-merge(USvideos,category2,all.x=T,by="category_id")
USvideos_cate<-merge(USvideos,category2,all.x=T,by="category_id")

#3)
summary(USvideos)
options(digits = 2,scipen = 10)
require(pastecs)
stat.desc(USvideos)

#4-5)
#1) 에서 처리

#6)
aggregate(views~snippet.title,data=USvideos_cate,mean)

require(doBy)
a<-summaryBy(views~snippet.title,data=USvideos_cate,FUN=c(mean,median,length))
# a[order(-a$views.mean),]$snippet.title[1:5]
# b<-a[order(-a$views.mean),]$snippet.title[1:5]

# 시각화 접근법
require(ggplot2)
dev.off()

# 카테고리의 조회수 시각화
ggplot(data=USvideos_cate,aes(x=snippet.title,y=views,color=snippet.title))+geom_boxplot()+theme_bw()

# 카테고리의 조회수 시각화 (보정) 
ggplot(data=subset(USvideos_cate,views<25000000,select=(),aes(x=snippet.title,y=views,color=snippet.title))+geom_boxplot()+theme_bw()

# 7)
names(USvideos_cate)
USvideos_modelling<-subset(USvideos_cate,select=c(views,likes,dislikes,comment_count,snippet.title))
head(USvideos_modelling)

#8)
fit<-lm(views~.,data=USvideos_modelling)
summary(fit)
fit$fitted.values

#9)
# hist(USvideos_modelling$views)
