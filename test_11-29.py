import pandas as pd
#import numpy as np

# EUC-KR 은 KS X 1001와 KS X 1003을 사용하는 8비트 문자 인코딩, EUC의 일종이며 대표적인 한글 완성형 인코딩임
# 해당 파일을 utf-8로 설정 시 오류가 나서 euc-kr로 설정
shopping = pd.read_csv('shopping.csv', encoding = 'euc-kr')
notshopping = pd.read_csv('notshopping.csv')

# 필요한 컬럼들만 선택
#shopping = raw_shopping[['ID', 'GENDER', 'AGE_PRD', 'DE_DT', 'DE_HR', 'BIZ_UNIT.x', 'PD_S_C', 'PD_H_NM', 'PD_M_NM', 'PD_S_NM', 'BUY_CT', 'BUY_AM']]
#notshopping = raw_notshopping[['ID', 'GENDER', 'AGE_PRD', 'BIZ_UNIT', 'CRYM', 'U_AM', 'U_CT']]

## 성별로 구분

# 남자만
male = shopping[shopping['GENDER'] == 1]
n_male = notshopping[notshopping['GENDER'] == 1]

#여자만
female = shopping[shopping['GENDER'] == 2]
n_female = notshopping[notshopping['GENDER'] == 2]


"""
#######################################################

## 나이대별로 구분

# 남자 나이대별로 그룹화
M_age = male.groupby('AGE_PRD')
n_M_age = n_male.groupby('AGE_PRD')

# 여자 나이대별로 그룹화
F_age = female.groupby('AGE_PRD')
n_F_age = n_female.groupby('AGE_PRD')

# 남자+여자 나이대별로 그룹화
MF_age = shopping.groupby('AGE_PRD')
n_MF_age = notshopping.groupby('AGE_PRD')

#######################################################


"""

# 전에 올렸던 과정이 반복많아서 함수로 구해봄

# gender = 성별 선택, group = 어느 기준으로 그룹화할건지, criteria = 어느 요소기준으로 정렬할건지
def group_sum(gender, group, criteria):
    result = gender.groupby(group).sum().sort_values(by=criteria, ascending=False)
    return result[criteria]


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


#################################################################3


### 어떤 품목이 많이 팔렸나?
## shopping만
# 남자 모든연령 많이 팔린 품목
group_sum(male, 'PD_S_NM', 'BUY_CT')

# 여자 모든연령 많이 팔린 품목
group_sum(female, 'PD_S_NM', 'BUY_CT')

# 남자+여자 모든연령 많이 팔린 품목
group_sum(shopping, 'PD_S_NM', 'BUY_CT')

# 남자 연령별 많이 팔린 제품  -> 코드 다시 짜봐야할것같음..
male.groupby(['AGE_PRD', 'PD_S_NM']).sum().sort_values(by=['AGE_PRD', 'BUY_CT'], ascending=False)
#group_sum(male, ['AGE_PRD', 'PD_S_NM'], ['AGE_PRD', 'BUY_CT'])


######################################################################################


### 어떤 업종에서 많이 팔렸나 (shopping, notshopping)

# gender = 성별 선택, group = 어느 기준으로 그룹화할건지, criteria = 어느 요소기준으로 정렬할건지
def group_count(gender, group, criteria):
    result = gender.groupby(group).count().sort_values(by=criteria, ascending=False)
    return result[criteria]

## shopping
# 남자 모든연령 많이 이용한 업종
group_count(male, 'BIZ_UNIT.x', 'ID')

# 여자 모든연령 많이 이용한 업종
group_count(female, 'BIZ_UNIT.x', 'ID')

# 남자+여자 모든연령 많이 이용한 업종
group_count(shopping, 'BIZ_UNIT.x', 'ID')

## notshopping
# 남자 모든연령 많이 이용한 업종
group_count(n_male, 'BIZ_UNIT', 'ID')

# 여자 모든연령 많이 이용한 업종
group_count(n_female, 'BIZ_UNIT', 'ID')

# 남자+여자 모든연령 많이 이용한 업종
group_count(notshopping, 'BIZ_UNIT', 'ID')
