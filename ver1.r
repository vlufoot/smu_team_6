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
library(data.table)
library(dplyr)
library(ggplot2)
library(arules)

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

length(unique(category$PD_H_NM))
length(unique(category$PD_M_NM))
length(unique(category$PD_S_NM))

## 중분류에 겹치나?
head(category$PD_M_NM,100)
category %>% filter(PD_M_NM =="도시락") %>% head(100)

  ## biz_unit+pd_s_c로 봐야 어떤상품인지 알 수 있다/ 코드통합

category <- category %>% 
  mutate(P_CODE=paste(BIZ_UNIT,PD_S_C,sep="_")) %>% 
  select(P_CODE,PD_M_NM,PD_S_NM)

head(category,200)
str(category)

###########################################################
###########################################################

# 각 데이터를 우선 살펴보자 ( nonshop )
summary(nonshop)
str(nonshop)

  ## BIZ_UNIT 범주화
nonshop$BIZ_UNIT <- as.factor(nonshop$BIZ_UNIT)


  ## 전체고객중 noshop을 이용한 고객은?
length(unique(nonshop$ID))/length(customer$ID) * 100

  ## 이용건수가 지속적으로 1=일인가구 2=신혼부부 4=가족 ?

  ## 테마파크에서 3인이상 긁은게 많으면 가족단위고객일것이다??
nonshop %>% filter(BIZ_UNIT=="C02"&U_CT>3) %>% 
  group_by(ID) %>% 
  summarise(n_use=n()) %>% 
  filter(n_use>3) %>% 
  arrange(desc(n_use))

customer %>% filter(ID==10087)

  ## 계속 1건씩만 긁으면 혼밥족일것이다  

nonshop %>% filter(U_CT == 1) %>% 
  group_by(ID) %>% 
  summarise(n_use=n()) %>% 
  filter(n_use>15) %>% 
  arrange(desc(n_use))

###########################################################
###########################################################



# 데이터 조인

str(customer)
str(shop)
str(category)
str(nonshop)

  ## customer + shop + category

shopping <- merge(customer, shop, by="ID")
head(shopping)
shopping <- merge(shopping, category, by="P_CODE")
head(shopping)

shopping <- shopping %>% 
  select(ID,GENDER,AGE_PRD,DE_DT,DE_HR,R_NUM, BUY_CT,BUY_AM,P_CODE,PD_S_NM,PD_M_NM)

  ## customer + nonshop
notshopping <- merge(customer, nonshop, by="ID")
head(notshopping)

###########################################################################################
####################################################################################3333333

# 분석

## 영수증별로 그룹(장바구니분석을 위한)

length(unique(shopping$R_NUM)) # 현재 영수증 100만개


### 중분류

reciept_M <- shopping %>% select(R_NUM, PD_M_NM)

rM_list<- split(reciept_M$PD_M_NM,reciept_M$R_NUM)
rM_list[3]
rM_trans <- as(rM_list,"transactions")
summary(rM_trans)

itemFrequencyPlot(rM_trans,topN=20)
itemFrequencyPlot(rM_trans,topN=20,type="absolute")

rM_rules <- apriori(rM_trans,
                   parameter = list(supp=0.001,
                                    conf=0.7))
inspect(rM_rules)

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

rS_rules_2 <- subset(rS_rules, !items %pin% c("공병"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("상품군미지정"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("김밥"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("햄"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("김"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("상추"))
rS_rules_2 <- subset(rS_rules_2, !items %pin% c("아이스크림"))
rS_rules_3 <- subset(rS_rules_2, items %pin% c("옷"))

inspect(sort(rS_rules_3,by="lift"))

### 시간대별로 나누든지 해야할듯.. 너무 당연함



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
###########################################################







###########
setkey(shop,ID)
setkey(customer,ID)
temp1 <- merge(shop, customer, by="ID")
shop[PD_S_C==1]
shop[BIZ_UNIT=="A01"&BUY_AM>100000000]
write.csv(merge3, "shopping.csv")
write.csv(merge4, "notshopping.csv")
