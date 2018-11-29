import pandas as pd

# 그래프, 한글 설정
# 출처 : https://wikidocs.net/4767
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import font_manager, rc

font_name = font_manager.FontProperties(fname="c:/Windows/Fonts/malgun.ttf").get_name()
rc('font', family=font_name)

#####################################################################

# EUC-KR 은 KS X 1001와 KS X 1003을 사용하는 8비트 문자 인코딩, EUC의 일종이며 대표적인 한글 완성형 인코딩임
# 해당 파일을 utf-8로 설정 시 오류가 나서 euc-kr로 설정
# 분석파일은 준석님이 전처리한 파일 사용..
shopping = pd.read_csv('shopping.csv', encoding = 'euc-kr')
notshopping = pd.read_csv('notshopping.csv')

# 필요한 컬럼들만 선택
#shopping = raw_shopping[['ID', 'GENDER', 'AGE_PRD', 'DE_DT', 'DE_HR', 'BIZ_UNIT.x', 'PD_S_C', 'PD_H_NM', 'PD_M_NM', 'PD_S_NM', 'BUY_CT', 'BUY_AM']]
#notshopping = raw_notshopping[['ID', 'GENDER', 'AGE_PRD', 'BIZ_UNIT', 'CRYM', 'U_AM', 'U_CT']]


#####################################################################

## 성별로 구분

# 남자만
male = shopping[shopping['GENDER'] == 1]
n_male = notshopping[notshopping['GENDER'] == 1]

#여자만
female = shopping[shopping['GENDER'] == 2]
n_female = notshopping[notshopping['GENDER'] == 2]

#성별간 나이대로 구분
#남자 20대~60대
M_20 = male[male['AGE_PRD'] == '20PRD']
M_30 = male[male['AGE_PRD'] == '30PRD']
M_40 = male[male['AGE_PRD'] == '40PRD']
M_50 = male[male['AGE_PRD'] == '50PRD']
M_60 = male[male['AGE_PRD'] == '60PRD']

n_M_20 = n_male[n_male['AGE_PRD'] == '20PRD']
n_M_30 = n_male[n_male['AGE_PRD'] == '30PRD']
n_M_40 = n_male[n_male['AGE_PRD'] == '40PRD']
n_M_50 = n_male[n_male['AGE_PRD'] == '50PRD']
n_M_60 = n_male[n_male['AGE_PRD'] == '60PRD']

#여자 20대~60대
F_20 = female[female['AGE_PRD'] == '20PRD']
F_30 = female[female['AGE_PRD'] == '30PRD']
F_40 = female[female['AGE_PRD'] == '40PRD']
F_50 = female[female['AGE_PRD'] == '50PRD']
F_60 = female[female['AGE_PRD'] == '60PRD']

n_F_20 = n_female[n_female['AGE_PRD'] == '20PRD']
n_F_30 = n_female[n_female['AGE_PRD'] == '30PRD']
n_F_40 = n_female[n_female['AGE_PRD'] == '40PRD']
n_F_50 = n_female[n_female['AGE_PRD'] == '50PRD']
n_F_60 = n_female[n_female['AGE_PRD'] == '60PRD']

# 전체 연령별
total_20 = shopping[shopping['AGE_PRD'] == '20PRD']
total_30 = shopping[shopping['AGE_PRD'] == '30PRD']
total_40 = shopping[shopping['AGE_PRD'] == '40PRD']
total_50 = shopping[shopping['AGE_PRD'] == '50PRD']
total_60 = shopping[shopping['AGE_PRD'] == '60PRD']

n_total_20 = notshopping[notshopping['AGE_PRD'] == '20PRD']
n_total_30 = notshopping[notshopping['AGE_PRD'] == '30PRD']
n_total_40 = notshopping[notshopping['AGE_PRD'] == '40PRD']
n_total_50 = notshopping[notshopping['AGE_PRD'] == '50PRD']
n_total_60 = notshopping[notshopping['AGE_PRD'] == '60PRD']

#####################################################################

## 선택 성별 그룹화하여 더한 뒤 기준에 맞춰 정렬하는 함수
# gender_age = 성별 연령 선택, group = 어느 기준으로 그룹화할건지, criteria = 어느 요소기준으로 정렬할건지
def group_sum(gender_age, group, criteria):
    result = gender_age.groupby(group).sum().sort_values(by=criteria, ascending=False)
    return result[criteria]

#####################################################################

### 총판매금액
## shopping
# 남자 나이대별 총구매액
group_sum(male, 'AGE_PRD', 'BUY_AM')

# 여자 나이대별 총구매액
group_sum(female, 'AGE_PRD', 'BUY_AM')

# 남자+여자 나이대별 총구매액
group_sum(shopping, 'AGE_PRD', 'BUY_AM')


## notshopping
# 남자 나이대별 총구매액
group_sum(n_male, 'AGE_PRD', 'U_AM')

# 여자 나이대별 총구매액
group_sum(n_female, 'AGE_PRD', 'U_AM')

# 남자+여자 나이대별 총구매액
group_sum(notshopping, 'AGE_PRD', 'U_AM')


#####################################################################

### 어떤 품목이 많이 팔렸나?
## shopping만
# 남자 모든연령 많이 팔린 품목
group_sum(male, 'PD_S_NM', 'BUY_CT')

# 여자 모든연령 많이 팔린 품목
group_sum(female, 'PD_S_NM', 'BUY_CT')

# 남자+여자 모든연령 많이 팔린 품목
group_sum(shopping, 'PD_S_NM', 'BUY_CT')

# 남자 연령별 많이 팔린 제품  -> 코드 다시 짜봐야할것같음..
#male.groupby(['AGE_PRD', 'PD_S_NM']).sum().sort_values(by=['AGE_PRD', 'BUY_CT'],ascending=False)
#group_sum(male, ['AGE_PRD', 'PD_S_NM'], ['AGE_PRD', 'BUY_CT'])

# 남자 연령별 많이 팔린 품목
group_sum(M_20, 'PD_S_NM', 'BUY_CT')
group_sum(M_30, 'PD_S_NM', 'BUY_CT')
group_sum(M_40, 'PD_S_NM', 'BUY_CT')
group_sum(M_50, 'PD_S_NM', 'BUY_CT')
group_sum(M_60, 'PD_S_NM', 'BUY_CT')

# 여자 연령별 많이 팔린 품목
group_sum(F_20, 'PD_S_NM', 'BUY_CT')
group_sum(F_30, 'PD_S_NM', 'BUY_CT')
group_sum(F_40, 'PD_S_NM', 'BUY_CT')
group_sum(F_50, 'PD_S_NM', 'BUY_CT')
group_sum(F_60, 'PD_S_NM', 'BUY_CT')

# 남자+여자 연령별 많이 팔린 품목
group_sum(total_20, 'PD_S_NM', 'BUY_CT')
group_sum(total_30, 'PD_S_NM', 'BUY_CT')
group_sum(total_40, 'PD_S_NM', 'BUY_CT')
group_sum(total_50, 'PD_S_NM', 'BUY_CT')
group_sum(total_60, 'PD_S_NM', 'BUY_CT')


#####################################################################

### 어떤 업종에서 많이 팔렸나 (shopping, notshopping)

# gender = 성별 선택, group = 어느 기준으로 그룹화할건지, criteria = 어느 요소기준으로 정렬할건지
# def group_count(gender, group, criteria):
#    result = gender.groupby(group).count().sort_values(by=criteria, ascending=False)
#    return result[criteria]

# 선택 성별 그룹화하여 해당 기준 요소 개수 카운트하는 함수
# gender_age = 성별 연령 선택, criteria = 카운트할 기준 요소 선택
def count(gender_age, criteria):
    return gender_age[criteria].value_counts()

## shopping
# 남자 모든연령 많이 이용한 업종
count(male, 'BIZ_UNIT.x')

# 여자 모든연령 많이 이용한 업종
count(female, 'BIZ_UNIT.x')

# 남자+여자 모든연령 많이 이용한 업종
count(shopping, 'BIZ_UNIT.x')

## notshopping
# 남자 모든연령 많이 이용한 업종
count(n_male, 'BIZ_UNIT')

# 여자 모든연령 많이 이용한 업종
count(n_female, 'BIZ_UNIT')

# 남자+여자 모든연령 많이 이용한 업종
count(notshopping, 'BIZ_UNIT')

# 남자 연령대별로 많이 이용한 업종
count(M_20, 'BIZ_UNIT.x')
count(M_30, 'BIZ_UNIT.x')
count(M_40, 'BIZ_UNIT.x')
count(M_50, 'BIZ_UNIT.x')
count(M_60, 'BIZ_UNIT.x')

# 여자 연령대별로 많이 이용한 업종
count(F_20, 'BIZ_UNIT.x')
count(F_30, 'BIZ_UNIT.x')
count(F_40, 'BIZ_UNIT.x')
count(F_50, 'BIZ_UNIT.x')
count(F_60, 'BIZ_UNIT.x')

# 남자+여자 연령대별로 많이 이용한 업종
count(total_20, 'BIZ_UNIT.x')
count(total_30, 'BIZ_UNIT.x')
count(total_40, 'BIZ_UNIT.x')
count(total_50, 'BIZ_UNIT.x')
count(total_60, 'BIZ_UNIT.x')


#####################################################################

## 그래프?

# 그래프 테스트 - 많이 팔린품목 상위 10개 막대 그래프
# .iloc[:10] 는 0~9번째까지(10개), .plot(kind='bar') 는 막대그래프로
# 그래프 보고싶으면 구해놓은 코드 뒤에 .plot() 만 붙이면됨
group_sum(male, 'PD_S_NM', 'BUY_CT').iloc[:10].plot(kind='bar')
