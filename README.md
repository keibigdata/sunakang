# sunakang

기후변화에 따른 감염성 질환 발생 예측
============

본 연구에서는 질병 발생의 시･공간 분석 및 심층신경망(Deep Neural Network)을 이용하여 시군구 단위로 기후 및 대기요소, 인구 통계적 요소를 고려한 감염성 질환 패턴 분석 및 예측 알고리듬을 구축한다. 또한 해당 질병에 영향을 미치는 주요 변수를 파악하기 위해 민감도 분석(sensitivity analysis)을 추가로 수행한다.

## 코드설명
--------------

### 질병건수  산출  및 데이터 전처리

  * 1-1. Data read_extract_save.txt: 건강보험공단에서 구매한 데이터를 sas로 읽어들인 후 특정 질병과 관련된 데이터만 추출하여 재저장하는 코드
  * 1-2. Episode of care_0day.rtf: 진료 에피소드를 0일이라고 가정하고 데이터 질병 건수를 산출하는 코드
  * 1-3. Episode of care_14days.rtf: 진료 에피소드를 14일이라고 가정하고 데이터 질병 건수를 산출하는 코드

  * 2-1. Sigungu_min_max_mean_extract.R: 시군구별 대기오염 데이터의 최소, 최대, 평균값을 구하는 코드
  * 2-2. air_population_disease_matching.R: 대기오염 데이터, 인구 데이터, 질병 데이터에 대해 시군구를 기준으로 매칭하는 코드


### 질병건수  예측  알고리즘  구축

  * 3-1. stratified sampling.R: 층화추출법을 이용하여 데이터 샘플링 코드
  * 3-2. OLS_Lasso_DNN_analysis.R: 질병을 예측하기 위해 사용된 머신러닝 기법(OLS, Lasso, DNN) 분석 코드


### 민감도 분석

  * 4-1. sensitivity analysis.R: 질병에 중요하게 영향을 미치는 변수 추출을 위한 민감도 분석 코드
