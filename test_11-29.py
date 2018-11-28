# pyhton 2.7 version
import pandas as pd
#import numpy as np

raw_notshopping = pd.read_csv('notshopping.csv')
# notshopping_code = notshopping[['ID', 'GENDER', 'AGE_PRD', 'BIZ_UNIT', 'CRYM', 'U_AM', 'U_CT']]

# EUC-KR 은 KS X 1001와 KS X 1003을 사용하는 8비트 문자 인코딩, EUC의 일종이며 대표적인 한글 완성형 인코딩임
# 해당 파일을 utf-8로 설정 시 오류가 나서 euc-kr로 설정
raw_shopping = pd.read_csv('shopping.csv', encoding = 'euc-kr')

# 필요한 컬럼들만 선택
shopping = raw_shopping[['ID', 'GENDER', 'AGE_PRD', 'DE_DT', 'DE_HR', 'BIZ_UNIT.x', 'PD_S_C', 'PD_H_NM', 'PD_M_NM', 'PD_S_NM', 'BUY_CT', 'BUY_AM']]
shopping.head()

#######################################################

# 성별로 구분
# 남자만
male = shopping[shopping['GENDER'] == 1]

#여자만
female = shopping[shopping['GENDER'] == 2]

#######################################################

# 남자 나이대별로 그룹화
M_age = male.groupby('AGE_PRD')

# 남자 나이대별 총 구매액과 총구매액 높은 순서대로
M_age['BUY_AM'].sum().sort_values(ascending=False)

# 여자 나이대별로 그룹화
F_age = female.groupby('AGE_PRD')

# 남자 나이대별 총 구매액과 총구매액 높은 순서대로
F_age['BUY_AM'].sum().sort_values(ascending=False)

# 남자+여자 나이대별로 그룹화
MF_age = shopping.groupby('AGE_PRD')

# 남자+여자 나이대별 총 구매액과 총구매액 높은 순서대로
MF_age['BUY_AM'].sum().sort_values(ascending=False)


#######################################################


# 많이 팔린 품목 구하기
# 모든 연령 남자 많이 팔린 품목
d = dict()
for i in male['PD_S_NM']:
    if i not in d:
        d[i]=1
    else:
        d[i]=d[i]+1
sort_d = sorted(d.items(), key = lambda t : t[1], reverse=True)

print "<남자가 많이 산 품목>"
for i in range(len(sort_d)):
    print "{0}위 : ".format(i+1),
    for j in range(2):
        print sort_d[i][j],
    print ''
    
