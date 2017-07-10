# suna-kang

딥러닝을 활용한 기후 및 대기요소에 따른 전염성 질환 발생 패턴 분석
============


본 연구는 기상 데이터 및 대기오염물질 데이터를 이용하여 전염성 질환 발생 패턴 분석 및 
RNN(Recurrent Neural Networ)를 이용한 질병 예측에 관한 연구입니다.

## 코드설명
- air data_mean median max min.R: 대기오염물질의 시군구별 평균, 중앙값, 최대값, 최소값을 구하는 코드
- lm_gam analysis&ggplot.R: 기상 및 대기오염 데이터와 전염성질환의 발생건수에 대한 OLS 분석 및 GAM 분석 코드/ 월별, 시군구별 시계열 그래프를 그리는 코드
- Sas studio code.txt: 국민건강보험공단 코호트 데이터 전처리를 위한 table join 및 data transpose 코드
- weather_coding.R: 기상데이터와 대기오염 데이터의 측정소 매칭을 위한 기상데이터 전처리 코드
