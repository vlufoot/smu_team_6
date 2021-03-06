# project : Lpoint data analysis
# author : jun

#system.time(shop <-read.table("쇼핑업종상품구매.txt",
#                              sep=",",
#                              header=TRUE,
#                              encoding="UTF-8",
#                              stringsAsFactors=FALSE))

# data.frame 보다 약 100배빠른 data.table package 쓰자
# 시간측정은 system.time()


  # 데이터 로드

install.packages("data.table")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(data.table)

shop <- fread("쇼핑업종상품구매.txt",
              sep=",",
              header=TRUE,
              encoding="UTF-8",
              stringsAsFactors = FALSE)

customer<-fread("고객DEMO.txt",
                sep=",",
                header=TRUE,
                encoding="UTF-8",
                stringsAsFactors=FALSE)

category<-fread("쇼핑업종상품분류.txt",
                sep=",",
                header=TRUE,
                encoding="UTF-8",
                stringsAsFactors=FALSE)

nonshop <-fread("쇼핑외업종상품구매.txt",
                sep=",",
                header=TRUE,
                encoding="UTF-8",
                stringsAsFactors=FALSE)

###########################################################
###########################################################


# 각 데이터를 우선 살펴보자 ( customer )

str(customer)
head(customer,10)
summary(customer)

##gender, age_prd 속성 범주화
customer$GENDER <- as.factor(customer$GENDER)
customer$AGE_PRD <- as.factor(customer$AGE_PRD)

##우리는 지역 고려하지 않으므로 삭제
customer <- customer %>% select(-HOM_PST_NO)


table(customer$HOM_PST_NO)

###########################################################
###########################################################


# 각 데이터를 우선 살펴보자 ( shop )

str(shop)
head(shop,10)
summary(shop)

## 점포코드 삭제
shop <- shop %>% select(-BR_C)

## biz_unit+pd_s_c로 봐야 어떤상품인지 알 수 있다/ 코드통합

shop <- shop %>% 
  mutate(P_CODE=paste(BIZ_UNIT,PD_S_C,sep="_"))

## biz_unit + rct_no 로 봐야 영수증별로 알 수 있다 / 코드통합

shop <- shop %>%
  mutate(R_NUM=paste(BIZ_UNIT,RCT_NO,sep="_"))

## rct_no, biz_unit, pd_s_c 삭제

shop <- shop %>% select(-RCT_NO,-BIZ_UNIT,-PD_S_C)

## P_CODE,R_NUM 범주화(아직)
#shop$P_CODE <- as.factor(shop$P_CODE)
#shop$R_NUM <- as.factor(shop$R_NUM)

###########################################################
###########################################################


# 각 데이터를 우선 살펴보자 ( category )
str(category)
head(category,10)  

# 각각의 카테고리가 몇 개씩 있을까?
length(unique(category$PD_H_NM))
length(unique(category$PD_M_NM))
length(unique(category$PD_S_NM))

## biz_unit마다 겹치는 중분류가 있는듯
head(category$PD_M_NM,100)
category %>% filter(PD_M_NM =="도시락") %>% head(100)

## biz_unit+pd_s_c로 봐야 어떤상품인지 알 수 있다/ 코드통합

category <- category %>% 
  mutate(P_CODE=paste(BIZ_UNIT,PD_S_C,sep="_")) %>% 
  select(P_CODE,PD_H_NM,PD_M_NM,PD_S_NM)

head(category,200)
str(category)

###########################################################
###########################################################

# 각 데이터를 우선 살펴보자 ( nonshop )
summary(nonshop)
str(nonshop)
head(nonshop)

## BIZ_UNIT 범주화
nonshop$BIZ_UNIT <- as.factor(nonshop$BIZ_UNIT)

## 전체고객중 noshop을 이용한 고객은?(약88%의 고객)
length(unique(nonshop$ID))/length(customer$ID) * 100

## 알아보기 쉽게 코드를 바꿔보자
nonshop$BIZ_UNIT <- ifelse(nonshop$BIZ_UNIT=="B01","Hotel",
                    ifelse(nonshop$BIZ_UNIT=="B02","Travel",
                    ifelse(nonshop$BIZ_UNIT=="B03","Taxfree",
                    ifelse(nonshop$BIZ_UNIT=="C01","Theater",
                    ifelse(nonshop$BIZ_UNIT=="C02","Themepark",
                    ifelse(nonshop$BIZ_UNIT=="C03","baseball",
                    ifelse(nonshop$BIZ_UNIT=="D01","Fastfood",
                    ifelse(nonshop$BIZ_UNIT=="D02","Familyrestaurant",
                    ifelse(nonshop$BIZ_UNIT=="D03","Cafe",nonshop$BIZ_UNIT)))))))))


## 이용건수가 지속적으로 1=일인가구 2=신혼부부 4=가족 ?
## 테마파크에서 3인이상 긁은게 많으면 가족단위고객일것이다??
#nonshop %>% filter(BIZ_UNIT=="C02"&U_CT>3) %>% 
#  group_by(ID) %>% 
#  summarise(n_use=n()) %>% 
#  filter(n_use>3) %>% 
#  arrange(desc(n_use))

## 계속 1건씩만 긁으면 혼밥족일것이다  

#nonshop %>% filter(U_CT == 1) %>% 
#  group_by(ID) %>% 
#  summarise(n_use=n()) %>% 
#  filter(n_use>15) %>% 
#  arrange(desc(n_use))

###########################################################
###########################################################



# 데이터 조인

str(customer)
str(shop)
str(category)
str(nonshop)

## customer + shop + category
shopping <- merge(customer, shop, by="ID")
shopping <- merge(shopping, category, by="P_CODE")

## 필요한 컬럼만 남기자
shopping <- shopping %>% 
  select(ID,GENDER,AGE_PRD,DE_DT,DE_HR,
         R_NUM, BUY_CT,BUY_AM,P_CODE,PD_S_NM,PD_M_NM,PD_H_NM)

## customer + nonshop
notshopping <- merge(customer, nonshop, by="ID")

head(shopping)
head(notshopping)

###########################################################################################
####################################################################################3333333


#RFM 분석

shopping %>%  head()

## R

s_R <- shopping %>% group_by(ID) %>% 
  summarise(Recency = max(DE_DT)) 

# 2014년 구매고객 제외(2건)
s_R <- s_R %>% filter(!Recency < 20150101)

# 날짜의 차이를 구하기 위해 데이터 변환  
s_R$Recency <- as.character(s_R$Recency)
s_R$Recency <- as.Date(s_R$Recency, format="%Y%m%d")

lastday <- "20151231"
lastday <- as.Date(lastday, format="%Y%m%d")
s_R$Recency <- lastday - s_R$Recency + 1
s_R$Recency <- as.numeric(s_R$Recency)

## F
## 횟수만 알면 되니 영수증 중복 삭제
shopping_tmp <- shopping[!duplicated(shopping$R_NUM)]
length(unique(shopping_tmp$R_NUM))

s_F<- shopping_tmp %>% group_by(ID) %>% 
  summarise(Frequency = n())

## M
s_M <- shopping %>% group_by(ID) %>% 
  summarise(Monetary = sum(BUY_AM))

head(s_M)


## RFM 병합
s_RFM <- merge(s_R,s_F, by="ID")
s_RFM <- merge(s_RFM, s_M, by="ID")
s_RFM %>% head(10)

s_RFM %>% arrange(desc(Monetary)) %>% head(10)

summary(s_RFM)

## id char로
s_RFM$ID <- as.character(s_RFM$ID)
summary(s_RFM)

## s_RFM은 상당히 한곳으로 치우쳐진 분포를 보임
hist(s_RFM$Monetary)
hist(s_RFM$Frequency)
hist(s_RFM$Recency)

## 산점도 그려보자

plot(s_RFM[,2:4])

## RFM에 로그를 취해서 다시 봐보자


s_RFM_log <- s_RFM
s_RFM_log$Monetary<- log(s_RFM$Monetary)
s_RFM_log$Recency <- log(s_RFM$Recency)
s_RFM_log$Frequency <- log(s_RFM$Frequency)

head(s_RFM_log)

hist(s_RFM_log$Monetary)
hist(s_RFM_log$Frequency)
hist(s_RFM_log$Recency)

## 산점도도 다시 그려보자

plot(s_RFM_log[,2:4])
summary(s_RFM_log)



## R,F,M 클래스를 나눠보자
## 4분위수로 나누는건 다른분께서 해주심!

summary(s_RFM_log)

s_RFM_log <- s_RFM_log %>% mutate(R = ifelse(Recency<1.609, 4,
                                             ifelse(Recency<2.631, 3,
                                                    ifelse(Recency<3.761, 2, 1))))

s_RFM_log <- s_RFM_log %>% mutate(F = ifelse(Frequency<2.398, 1,
                                             ifelse(Frequency<3.284 , 2,
                                                    ifelse(Frequency<4.407, 3, 4))))

s_RFM_log <- s_RFM_log %>% mutate(M = ifelse(Monetary<13.096, 1,
                                             ifelse(Monetary<14.148 , 2,
                                                    ifelse(Monetary<15.400, 3, 4))))

## LTV를 구해보자 (가중치는 업종마다 크게 다르기때문에 없이!!)

s_RFM_log <- s_RFM_log %>% mutate(LTV = R+F+M)
s_RFM_log %>% arrange(desc(LTV))
table(s_RFM_temp$LTV)

#고객생애가치가 3부터 12까지 적절하게 분배되있는것을 보아
#어디에 전략을 구사할지 택할 수 있을듯 보임
#하지만 우리는 R,F,M각각의 특성을 활용한 마케팅을 할 것이기 때문에 클러스터링 해보자



## K-MEANS를 통해 클러스터링 해보자
## 우선 다섯개의 클러스터로 나눠서 3차원 시각화해서 보자

install.packages("car")
install.packages("rgl")
library(car)
library(rgl)

viz_3d <- s_RFM_log
RFM_km <- kmeans(s_RFM_log[2:4],
                 centers = 5)

## 이런것들을 통해 클러스터의 특징을 확인할 수 있다
RFM_km$centers
RFM_km$size
RFM_km$cluster

## 기존의 데이터프레임에 cluster를 추가하자
viz_3d[,("cluster")] <- RFM_km$cluster
viz_3d$cluster <- as.factor(viz_3d$cluster)
table(viz_3d$cluster)


colors <- c('red','orange','green3','deepskyblue',
            'blue','darkorchid4','violet','pink1','tan3','black')

scatter3d(x=viz_3d$Recency,
          y=viz_3d$Frequency,
          z=viz_3d$Monetary,
          groups=viz_3d$cluster,
          xlab="Frequency(log)",
          ylab="Monetary(log)",
          zlab="Recency(log)",
          surface.col=colors,
          axis.scales = FALSE,
          surface = FALSE, 
          fit = "smooth",
          grid = TRUE,
          axis.col = c("black", "black", "black"))



## cluster를 몇개로쪼개야할지는 elbow curve를 그려보던지 비즈니스상 요구를 확인해야하지만
## 다양한 레벨로 나누기 위해 우리는 15개로 가정하고 해보자

seg <- s_RFM_log
RFM_km2 <- kmeans(seg[2:4],
                 centers = 15)

seg[,("cluster")] <- RFM_km2$cluster
seg$cluster <- as.factor(seg$cluster)
table(seg$cluster)

cluster_model <- RFM_km2$centers  
cluster_model

cluster_name <- data.frame(cluster=c(1:15))
cluster_name

cluster_size <- as.data.frame(RFM_km2$size)
cluster_size <- rename(cluster_size, size = `RFM_km2$size`)

cluster_model <- cbind(cluster_model,
                       cluster_name)

cluster_model <- cbind(cluster_model,
                       cluster_size)

cluster_model <- cluster_model %>% select(cluster, size, Recency, Frequency, Monetary)
cluster_model <- rename(cluster_model, R = Recency)
cluster_model <- rename(cluster_model, F = Frequency)
cluster_model <- rename(cluster_model, M = Monetary)

## 클러스터별 특징을 지정

summary(cluster_model)

# VIP, BEST, SHOPPER, NEW, CHURN, SPENDERS, UNCERTAIN 

cluster_model <- cluster_model %>% mutate(type= ifelse(R<quantile(cluster_model$R, probs = 0.4) &
                                                       F>quantile(cluster_model$R, probs = 0.6)&
                                                       M<quantile(cluster_model$R, probs = 0.4),"SHOPPER",
                                                ifelse(R<quantile(cluster_model$R, probs = 0.1) & 
                                                       F>quantile(cluster_model$R, probs = 0.9)&
                                                       M>quantile(cluster_model$R, probs = 0.9), "VIP",
                                                ifelse(R<quantile(cluster_model$R, probs = 0.5) &
                                                       F>quantile(cluster_model$R, probs = 0.5) &
                                                       M>quantile(cluster_model$R, probs = 0.5), "BEST",
                                                ifelse(R<quantile(cluster_model$R, probs = 0.25) &
                                                       F<quantile(cluster_model$R, probs = 0.25) &
                                                       M<quantile(cluster_model$R, probs = 0.25), "NEW",
                                                ifelse(R>quantile(cluster_model$R, probs = 0.5) &
                                                       F>quantile(cluster_model$R, probs = 0.5)  &
                                                       M>quantile(cluster_model$R, probs = 0.5), "CHURN",
                                                ifelse(R>quantile(cluster_model$R, probs = 0.3) &
                                                       F<quantile(cluster_model$R, probs = 0.3)&
                                                       M>quantile(cluster_model$R, probs = 0.8), "SPENDER","UNCERTAIN")))))))

cluster_model



############################################################################################
############################################################################################

# 장바구니분석
library(arules)

## 영수증별로 그룹(장바구니분석을 위한)
length(unique(shopping$R_NUM)) # 현재 영수증 100만개


### 중분류
reciept_M <- shopping %>% select(R_NUM, PD_M_NM)

rM_list<- split(reciept_M$PD_M_NM,reciept_M$R_NUM)
rM_list[3]
rM_trans <- as(rM_list,"transactions")
summary(rM_trans)

# 가장 많이팔린건?
itemFrequencyPlot(rM_trans,topN=20)
itemFrequencyPlot(rM_trans,topN=20,type="absolute")

rM_rules <- apriori(rM_trans,
                    parameter = list(supp=0.001,
                                     conf=0.5))

inspect(sort(rM_rules,by="lift")[1:30])

### 소분류

reciept_S <- shopping %>% select(R_NUM, PD_S_NM)
rS_list<- split(reciept_S$PD_S_NM,reciept_S$R_NUM)
rS_list[5]
rS_trans <- as(rS_list,"transactions")
summary(rS_trans)
image(sample(rS_trans,2000,replace=FALSE))

itemFrequencyPlot(rS_trans,topN=20,type="absolute")

rS_rules <- apriori(rS_trans, parameter =
                      list(support=0.00005,
                           confidence = 0.5))

rS_rules <- subset(rS_rules, !items %pin% c("공병"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("상품군미지정"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("김밥"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("햄"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("김"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("상추"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("아이스크림"))
rS_rules_3 <- subset(rS_rules_2, items %pin% c("옷"))

inspect(sort(rS_rules,by="lift")[1:30])

### 나눠서 봐야함 너무 당연함


### shopping, notshopping 합쳐서 장바구니 분석해보자

head(notshopping)
head(shopping)

ns <- notshopping %>% select(ID, BIZ_UNIT)
ns <- rename(ns, PD_H_NM = BIZ_UNIT)
ss <- shopping %>% select(ID, PD_H_NM)
both <- bind_rows(ns,ss)
str(both)
head(both)

both_list<- split(both$PD_H_NM,both$ID)
both_trans <- as(both_list,"transactions")
itemFrequencyPlot(both_trans,topN=20)

new_rules <- apriori(both_trans,
                     parameter = list(supp=0.2,
                                      conf=0.7))

new_rules <- subset(new_rules, items %pin% c("Themepark"))

inspect(sort(new_rules,by="lift")[1:30])
inspect(new_rules)


## 와.. split을 몰라서 3일을 찾았네

prac <- data.frame(r_num= as.factor(c("a","a","b","b","b","c")),
                   product=as.factor( c("milk","candy","milk",
                                        "coffee","coffee",
                                        "pencil")))

prac
prac.list = split(prac$r_num, prac$product)
prac.list2 = split(prac$product,prac$r_num)
prac.list2["a"]
prac.trans = as(prac.list2,"transactions")
image(prac.trans)
inspect(prac.trans)

market.rules = apriori(prac.trans)
inspect(market.rules)


###########################################################

## 특정클러스터 내에서 장바구니분석을 해보쟛
## 1. Spenders
head(try1)
head(shopping)
Spenders <- merge(try1,shopping,by="ID")
Spenders <- Spenders %>% filter(cluster==1)
Spenders <- Spenders %>% select(-Recency, -Frequency, -Monetary)
head(Spenders)

length(unique(Spenders$R_NUM))  # 약 3만개의 영수증
length(unique(Spenders$ID))  # 1382명

Spenders_R <- Spenders %>% select(R_NUM, PD_S_NM)
SR_list <- split(Spenders_R$PD_S_NM,Spenders_R$R_NUM)
SR_list[0:5]

#
Spenders_R <- Spenders %>% select(ID, PD_S_NM)
SR_list <- split(Spenders_R$PD_S_NM,Spenders_R$ID)
SR_list[0:5]
#

SR_trans <- as(SR_list,"transactions")
summary(SR_trans)
image(sample(SR_trans,1000,replace=FALSE))
itemFrequencyPlot(SR_trans,topN=20,type="absolute")


SR_rules <- apriori(SR_trans, parameter =
                      list(support=0.05,
                           confidence = 0.5))

summary(SR_rules)
inspect(sort(SR_rules,by="lift")[0:50])
# 오오 여성화장품을 많이구입하는것같음 WHY???
head(Spenders)

# 확실히 여성들이 많다
Spenders %>% group_by(GENDER) %>% summarise(n=n())
# 30,40,50대 여성들이 많음?
Spenders %>% group_by(AGE_PRD) %>% summarise(n=n())



## 2. Shopper

head(try1)
head(shopping)
Shopper <- merge(try1,shopping,by="ID")
Shopper <- Shopper %>% filter(cluster==8)
Shopper <- Shopper %>% select(-Recency, -Frequency, -Monetary)

length(unique(Shopper$ID))  # 712명

#
Shopper_R <- Shopper %>% select(ID, PD_S_NM)
SR_list <- split(Shopper_R$PD_S_NM,Shopper_R$ID)
SR_list[0:5]

#

SR_trans <- as(SR_list,"transactions")
summary(SR_trans)
image(sample(SR_trans,1000,replace=FALSE))
itemFrequencyPlot(SR_trans,topN=20,type="absolute")


SR_rules <- apriori(SR_trans, parameter =
                      list(support=0.1,
                           confidence = 0.7))

summary(SR_rules)
inspect(sort(SR_rules,by="lift")[0:50])

# 갑자기 술이등장하고 간단해보이는 음식이 많아졌다
head(Shopper)

# 확실히 여성들이 많다
Shopper %>% group_by(GENDER) %>% summarise(n=n())
# 30,40대 여성들이 많음?
Shopper %>% group_by(AGE_PRD) %>% summarise(n=n())


## 3. VIP

seg <- seg %>% filter(cluster==3)
VIP <- merge(seg,shopping,by="ID")
VIP <- VIP %>% select(-Recency, -Frequency, -Monetary)
length(unique(VIP$ID))  # 895명

#
VIP_R <- VIP %>% select(ID, PD_S_NM)
SR_list <- split(VIP_R$PD_S_NM,VIP_R$ID)
SR_list[0:5]

#
SR_trans <- as(SR_list,"transactions")
summary(SR_trans)
image(sample(SR_trans,1000,replace=FALSE))
itemFrequencyPlot(SR_trans,topN=20,type="absolute")


SR_rules <- apriori(SR_trans, parameter =
                      list(support=0.5,
                           confidence = 0.8))

summary(SR_rules)
inspect(sort(SR_rules,by="lift"))

# 편의식이 없어짐;; 신선식품이 많아지고 식당이용이 많아짐
# 확실히 여성들이 많다
VIP %>% group_by(GENDER) %>% summarise(n=n())
# 50대 여성들이 많음?
VIP %>% group_by(AGE_PRD) %>% summarise(n=n())







## 6월까지의 RFM을 통해 3,4분기 구매액을 예측해보자

shopping %>%  head()
shopping2 <- shopping %>% filter(DE_DT<20150701)
shopping4 <- shopping %>% filter(DE_DT>=20150701)
shopping4 <- shopping4 %>% group_by(ID) %>% summarise(amount_4=sum(BUY_AM))

## R
## 2014년 구매고객 제외(2건)

R2 <- shopping2 %>% group_by(ID) %>% 
  summarise(Recency = max(DE_DT)) 

R2 <- R2 %>% filter(!Recency < 20150101)

head(R2)

R2$Recency <- as.character(R2$Recency)
R2$Recency <- as.Date(R2$Recency, format="%Y%m%d")

lastday <- "20150701"
lastday <- as.Date(lastday, format="%Y%m%d")
R2$Recency <- lastday - R2$Recency
R2$Recency <- as.numeric(R2$Recency)

## F
## 횟수만 알면 되니 영수증 중복 삭제

shopping2 <- shopping[!duplicated(shopping$R_NUM)]
shopping2 <- shopping2 %>% filter(DE_DT<20150701)
length(unique(shopping2$R_NUM))

F2 <- shopping2 %>% group_by(ID) %>% 
  summarise(Frequency = n()) 

## M
M2 <- shopping2 %>% group_by(ID) %>% 
  summarise(Monetary = sum(BUY_AM))

## RFM 병합
RFM2 <- merge(R2,F2, by="ID")
RFM2 <- merge(RFM2, M2, by="ID")
RFM2 %>% head(10)

summary(RFM2)
RFM2$ID <- as.character(RFM2$ID)

RFM2 <- merge(RFM2, shopping4, by="ID")
summary(RFM2)

## 오 2분기까지 구매한고객중 아예이탈한 고객은 없음
RFM2 <- rename(RFM2, future = amount_4)

##  나이도 포함시키자
RFM2 <- merge(RFM2,customer,by="ID")
RFM2$AGE_PRD <- gsub("PRD","",RFM2$AGE_PRD)
RFM2$AGE_PRD <- as.numeric(RFM2$AGE_PRD)
RFM2 <- RFM2 %>%
  dplyr::select(ID, AGE_PRD, Recency, Frequency, Monetary, future)


RFM2$ID <- as.character(RFM2$ID)
RFM2$Frequency <- as.numeric(RFM2$Frequency)
RFM2$Monetary <- as.numeric(RFM2$Monetary)
RFM2$future <- as.numeric(RFM2$future)



## 우선 age, R,F,M,future 상관관계를 살펴보자

head(RFM2,10)
summary(RFM2)
RFM2$AGE_PRD <- as.factor(RFM2$AGE_PRD)

ggplot(RFM2, aes(Monetary,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(AGE_PRD,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(Recency,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(Frequency,future)) + geom_jitter() + geom_smooth(method = "lm")

cor(RFM2$Monetary, RFM2$future)
with(RFM2, cor(Monetary, future))
with(RFM2, cor(Monetary, future,method = "kendall"))
with(RFM2, cor(Monetary, future,method = "spearman"))
# 기본적으로 피어슨상관계수를 계산해줌
# 이상치의 영향을 많이 받는데 켄달의 타우나 스피어맨의 로를 사용하면
# 아무리 값이 극단적이여도 값의 순위를 매긴뒤 순위 내로 제한시키므로 영향을 덜받는 로버스트 통계량이 됨
# 솔직히 뭔소린지 모르겠음
# 때려치자일단



## 로그취해서 상관관계 봐보자

plot(RFM2[,2:6])

head(RFM2)
hist(RFM2$Monetary)
hist(RFM2$future)
hist(RFM2$Recency)
hist(RFM2$Frequency)

RFM2$Monetary<- log(RFM2$Monetary)
RFM2$Recency <- log(RFM2$Recency)
RFM2$Frequency <- log(RFM2$Frequency)
RFM2$future <- log(RFM2$future)

head(RFM2)
plot(RFM2[,2:6])



install.packages("corrplot")
library(corrplot)

col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))

RFM2$AGE_PRD <- as.numeric(RFM2$AGE_PRD)
cor(RFM2[,2:6])
cor <- cor(RFM2[,2:6])
corrplot(cor,method="color",type="lower",addCoef.col = "black",tl.col="black")

# 나이는 큰 상관이 없어보인다..???

ggplot(RFM2, aes(Monetary,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(AGE_PRD,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(Recency,future)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(RFM2, aes(Frequency,future)) + geom_jitter() + geom_smooth(method = "lm")


## 트레이닝셋과 테스트셋으로 나눠보자(우선 validation set은 나누지 않았다.. 다음에)

head(RFM2)
str(RFM2)

set.seed(129)
n <- nrow(RFM2)
idx <- 1:n
training_idx <- sample(idx, n*0.7)
test_idx <-setdiff(idx,training_idx)
training <- RFM2[training_idx,]
test <- RFM2[test_idx,]

training <- training[2:6]
test <- test[2:6]

str(training)
str(test)

data_lm_full <- lm(future ~., data=training)
summary(data_lm_full)

predict(data_lm_full, newdata=test[1:5,])


# 네가지 변수를 모두사용하는 것이 옳을까? 예측력을 올리기위해 변수선택을 해보자
library(MASS)

data_step <- stepAIC(data_lm_full, scope=list(upper = ~.^2, lower=~1))
data_step
summary(data_step)
length(coef(data_step))
# 여러 변수간 상호작용까고려한결과 10개가 됐고 Adjusted R-squared가 조금 상승했다

y_obs <- test$future
yhat_lm <- predict(data_lm_full, newdata=test)
yhat_step <- predict(data_step, newdata=test)


install.packages("caret")
library(caret)

RMSE(y_obs, yhat_lm)
RMSE(y_obs, yhat_step)


# 나무모형을 적용해보자
library(rpart)
data_tr <- rpart(future ~., data= training)
data_tr
printcp(data_tr)
summary(data_tr)

opar <- par(mfrow = c(1,1), xpd= NA)
plot(data_tr)
text(data_tr, use.n= TRUE)
par(opar)

# 으응.. 왜 frequency가 작아야 구매액이 크지??
# 미래에 높은 구매를 하는 고객은 가성비를 추구하는 shopper들보다 팍팍쓰는애들이라는 뜻..?
# recency,age는 그렇게 큰 영향을 주지 않는것같다
# 나무치고 예측력이 꽤 높은데..?

yhat_tr <- predict(data_tr, test)
RMSE(y_obs,yhat_tr)


  # 랜덤포레스트를 해보자
  # 확실히 오래걸린다;;

install.packages("randomForest")
library(randomForest)

set.seed(184)
data_rf <- randomForest(future ~., training)
data_rf

  #나무의 갯수에 따른 mse의 감소를 알려준다
  #적당히 30개쯤 해도될듯???
plot(data_rf)

  # 변수의 중요도를 알 수 있다
  # 확실히 현재까지 구매액과 얼마나 자주구매해왔는지가 가장 큰 영향을 끼친다

varImpPlot(data_rf)

  #RMSE는?
yhat_rf <- predict(data_rf, newdata = test)
RMSE(y_obs, yhat_rf)



# 선형모형, 스텝변수 선택한 선형 모형, 트리모델, 랜덤포레스트의 성능을 비교해보자
library(caret)
y_obs <- test$future
yhat_lm <- predict(data_lm_full, newdata=test)
yhat_step <- predict(data_step, newdata=test)
yhat_tr <- predict(data_tr, test)
yhat_rf <- predict(data_rf, newdata = test)

data.frame(lm = RMSE(y_obs, yhat_lm),
            step = RMSE(y_obs, yhat_step),
              tree = RMSE(y_obs, yhat_tr),
                rf = RMSE(y_obs, yhat_rf))
  
  # 스텝변수를 선택한 선형모델이 가장 예측력이 좋은 것을 알 수 있다 (최종모형사용)

  # 5개만 예측해보자
predict(data_step, newdata=test[1240:1250,])

final <- test[1240:1250,]

  # 자연로그를 풀어보자 exp()

pred <- data.frame(prediction = c(14.57220, 14.02942, 11.49969, 13.47236,
                                  13.97726, 14.40966, 14.45955, 13.68011,
                                  13.02384, 12.93759, 13.45594 ))

final <- cbind(final,pred)
final$Recency <- exp(final$Recency) 
final$Frequency <- exp(final$Frequency) 
final$Monetary <- exp(final$Monetary) 
final$future <- exp(final$future) 
final$prediction <- exp(final$prediction) 

final

# ㅎㅎ.. 생각보다 많이차이난당^^!






## references
# https://towardsdatascience.com/apply-rfm-principles-to-cluster-customers-with-k-means-fef9bcc9ab16
# 실리콘밸리 데이터과학자가 알려주는 따라하며 배우는 데이터과학 / 권재명
# Data Mining Using RFM Analysis / Derya Birant / Dokuz Eylul University / Turkey

