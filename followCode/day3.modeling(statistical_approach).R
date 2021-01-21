# rm(list=ls())

# 용어설명, 단순회귀 분석

# 데이터가 적으면(의학통계) 회귀분석이 어려움
# 편향없이 랜덤하게 추출하여 통계결과를 도출할 수 있는 시대가 됨됨

# 1. 회귀분석의 주요 용어
 # a. 잔차(벗어난 값), 기울기(모수) 도출, deviance(일탈도)
 # b. 상관 계수

# 2. 단순(simplest) 회귀 분석을 이해
 # a. 1개 numeric variable
 # b. 1개 categorical variable (범주형 : 분산분석)

library(tidyverse)  # subpackage를 한번에 불러옴
library(tidymodels) 
library(plotly)

# 잔차(residual) 보기 -----------------------------------------------------------------

# 데이터 생성
set.seed(2020) #난수 추출

data <- tibble(
    x = c(0:10), 
    y = c(5:15) + rnorm(11, 0, 1)
)

# 데이터 표현(선을 긋는 이유: 예측)
ggplot(data, aes(x, y)) + 
    geom_point() +
    geom_smooth(method = lm)

# 모형 작성, y = a + bx : 수학적 표기 (== y ~ x : R 표기) b:모수(parameter/모집단평균)
model <- lm(y ~ x, data = data)
model
#>   Coefficients:
#>  (Intercept:절편)     x:기울기    
#>            5.4209       0.9725  

# a, b는 어디?
a <- model$coefficients[[1]] ; a
b <- model$coefficients[[2]] ; b

# 예측값 
data$yhat <- predict(model)
data

# 새로운 데이터 예측
predict(model, newdata=tibble(x = 1.5)) # 새로운 데이터 x를 넣으면 예측 값 도출

# 잔차 표현
ggplot(data, aes(x, y)) +
    geom_point() +
    geom_smooth(method = lm, col = "blue", se = F) +
    geom_segment(aes(xend = x, yend = yhat), col = "red")


# 기울기 도출 과정 ---------------------------------------------------------------
# yhat = a + bx, y = yhat + e
# 잔차의 최소가 되는 값을 찾아감

# 기울기 범위 임의 지정
bs <- seq(0.1, 2, 0.01)

data$y

# 잔차 계산 함수 작성
sse <- function(i) {
    # e = y - yhat
    # e = y - a - bx, sum(e^2) 
    sum((data$y - 5.4 - bs[i] * data$x)^2)  # 5.4는 모델에서 a값
}

# 기울기와 잔차 제곱합 데이터
mydata <- tibble(
    slope = bs,
    sse = map_dbl(1:length(bs),sse) )

mydata %>% arrange(sse)

ggplot(mydata, aes(slope, sse)) +
    geom_line() +
    xlim(c(0, 2.5))


# 일탈도(deviance) 정의 --------------------------------------------------------
# 회귀분석에서 잔차 제곱합과 동일함
data(iris)

ggplot(iris, aes(Petal.width, Petal.Length)) +
    geom_point() +
    geom_smooth(method = "lm")

mod1 <- lm(Petal.Length ~ Petal.Width, data = iris)

# 잔차 제곱합
residuals(mod1) %>% sum(.^2)
sum(resid(mod1)^2)
mod1 %>% deviance()

# 상관 계수 도출 ----------------------------------------------------------------
# https://en.wikipedia.org/wiki/Correlation_and_dependence#/media/File:Correlation_examples2.svg
# 상관관계: 움직임의 변동성, p_vlaue로 확인(확률값)
# 귀무가설: 기존의 주장을 받아들이기 어렵지 않나?(법정에서 이 피의자는 살인자가 아니다)
# 대립가설: 새로운 주장(p-value가 0.05보다 작으면 대립가설 채택)
# 귀무가설 참 > p 통계량이 나올 가능성 0에 가깝다
# 정규분포: 밀도분포의 계급을 나눠 집계했을 때 / 밀도분포를 적분

iris %>% 
    select(Petal.Width, Petal.Length) %>% 
    as_tibble() %>% 
    summarise(corr = cor(Petal.Width, Petal.Length))

cor(iris$Petal.Width, iris$Petal.Length) 
cor.test(iris$Petal.Width, iris$Petal.Length) # p-value 값 뽑아서 검증

y <- iris$Petal.Length
x <- list(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width)

x %>% 
    map(~ cor(.x, y)) %>% 
    set_names(nm = c("x1 and y", "x2 and y", "x3 and y")) %>% 
    bind_rows() %>% 
    gather(두변수, 상관계수)

# 데이터 재정의 -----------------------------------------------------------------
mydata <- iris %>% 
    select(revenue = Petal.Length,
           cost    = Petal.Width,
           ads     = Species)

mydata <- mydata %>% 
    mutate(ads = case_when(ads == "setosa" ~ "잡지",
                           ads == "versicolor" ~ "라디오",
                           TRUE ~ "TV") %>% as.factor())
summary(mydata)

# 단순회귀 : 수치형 독립변수 1개일 때 (y = a + bx) -----------------------------------
# 이 모형으로 두 변수의 관계를 설명할 수 없음 > 귀무가설 : a = 0 and b = 0 (y = a + bx / 0 = 0)
# 이 모형으로 두 변수의 관계를 설명할 수 있음 > 대립가설 : a != 0 or b != 0 (y = a + bx)

m1 <- lm(revenue ~ cost, data = mydata)
summary(m1)
# p-value 0.05 미만: 귀무가설 기각, 대립가설 채택 
# R^2: 얼마나 설명할 수 있나? (92%)
# Coefficients: (a, b 값 모두 유의하다)
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.08356    0.07297   14.85   <2e-16 ***
#     cost     2.22994    0.05140   43.39   <2e-16 ***

# 예측
predict(m1, tibble(cost = 3)) %>% round(2)

# 계수이해: [1]절편, [2]기울기 : 위와 같음
(coef(m1)[[1]] + coef(m1)[[2]] * 3) %>%  round(2) 

# 모델 확인
graph1 <- ggplot(mydata, aes(cost, revenue)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F)

graph1


# 단순회귀 : 범주형 변수 1개일 때(anova) ----------------------------------------------
# anova: 두 그룹 평균값의 차이가 있는지 없는지 확인

# 범주형 변수 레벨 재정의
mydata %>% 
    group_by(ads) %>% 
    summarise(광고비_평균 = mean(cost))

# 데이터는 방향을 먼저봐야 함(저가 -> 고가)
mydata$ads <- factor(mydata$ads, levels = c("잡지", "라디오", "TV"))

# 모형 작성
m2 <- lm(revenue ~ ads, data = mydata)
summary(m2)
# mydata %>% group_by(ads) %>% summarise(means_revenue = mean(revenue)) 
# 잡지 매출 평균, 나머지는 평균차이(1.46 + 2.79 = 4.26(라디오 매출 평균))
# (Intercept)  1.46200    0.06086   24.02   <2e-16 ***
# ads라디오    2.79800    0.08607   32.51   <2e-16 ***
# adsTV        4.09000    0.08607   47.52   <2e-16 ***

# 예측
predict(m2, tibble(ads = "라디오")) %>% round(2)

# 매출 예측
# 잡지는 1.46, 라디오는 1.46 + 2.80, TV는 1.46 + 4.09

# 계수이해: 기준 범주(잡지)와 평균 차이
(coef(m2)[[1]] + coef(m2)[[2]]) %>% round(2)

predict(m2, tibble(ads = "TV")) %>% round(2)
(coef(m2)[[1]] + coef(m2)[[3]]) %>% round(2)


# 매체별 광고비 평균 
means_revenue <- mydata %>% 
    group_by(ads) %>% 
    summarise(means = mean(revenue))

means_revenue

# 모델 확인
graph_2 <- ggplot(mydata, aes(cost, revenue)) + 
    geom_point(aes(col = ads)) +
    geom_hline(yintercept = means_revenue$means)

ggplotly(graph_2)

# 모델 비교(m1 vs m2): m2의 잔차가 더 적으므로 m1에 비해 나은 모델
deviance(m1)
deviance(m2)

# 1) 회귀 모델링 하는 목적 예측
# 2) 시각화로 개념 이해
# 3) 잔차로 모델비교

# 분산분석 테이블 요약 및 이해 --------------------------------------------------------
anova_table <- summary(aov(revenue ~ ads, data = mydata)) #data:모집단, sample:표본
anova_table
#              Df Sum Sq Mean Sq F value Pr(>F)    
# ads           2  437.1  218.55    1180 <2e-16 ***
# Residuals   147   27.2    0.19                   
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 별이 3개면 유의함

# Df(Degree of freedom: 자유도): 자유스러움의 정도 / 3개 중에 2개가 변동가능하면 자유도 2
# 식: n - k(모수의 수)
# y = a + bx ; n - 2 (모수 a, b)
# 분산: sum((y - ybar)^2) * 1/n-1(자유도 n-1로 나눔) => 루트 씌우면 표준편차 
# 표본의 분산을 가지고 이게 모집단의 분산과 유사(대변)할 것이라고 생각함
# 변수의 개수보다 자유도의 개수가 더 대표를 잘 하더라 

# Mean Sq = Sum Sq / Df

# 총제곱합(TSS:total sum of square)
TSS <- sum((mydata$revenue - mean(mydata$revenue))^2)

# 그룹내 제곱합(SSE: sum of square error)
잡지   <- mydata[mydata$ads == "잡지",   ]
라디오 <- mydata[mydata$ads == "라디오", ]
TV     <- mydata[mydata$ads == "TV",     ]

SSE_잡지   <- sum((잡지$revenue - mean(잡지$revenue))^2)
SSE_라디오 <- sum((라디오$revenue - mean(라디오$revenue))^2)
SSE_TV     <- sum((TV$revenue - mean(TV$revenue))^2)

SSE <- SSE_잡지 + SSE_라디오 + SSE_TV

# 그룹간 제곱합(SSR, 회귀 제곱합: residual sum of sqaure)
SSR <- TSS - SSE
SSR
SSE

# F-통계량 : MSR(회귀제곱합 평균) / MSE(오차 제곱합 평균)
(SSR / 2) / (SSE / 147) 

# 자유도 
# ads : 잡지, 라디오, TV 중 2개 레벨이 정해지면 나머지 1개는 제약, 자유도 2 (변수)
# Residuals : 각 그룹별 모수(평균)에 대한 자유도는 n - k, 각 49 * 3 = 147   (데이터)

# 회귀분석은 숫자 예측을 위함(x 1개, y 1개)

#########################################################################################
# 학습목표

# 1. 공분산 분석 : y = a + b1x1 + b2x2, x1은 연속형 자료, x2는 범주형 자료
#   a. 독립변수 2개 이상 (parallel slope : 기울기 평행)
#   b. 독립변수 2개 이상 (interaction : 기울기 평행 X) 혼합효과를 보여준다...?
# 2. 다항 회귀 : y = a + b1x1 + b2x1^2 (2차식 이상)
# 3. 다중 회귀 : y = a + b1x1 + b2x2 + ... + bnxn

# 변수를 2개 이상 넣을 때 
# 교호항?(graph4)

names(mydata)

# 독립변수 2개 (또는 이상일 때) : parallel slope model -------------------------------
m3 <- lm(revenue ~ cost + ads, data = mydata)
summary(m3)

# 예측
predict(m3, tibble(cost = 3, ads = "라디오")) %>% round(2)

# 계수이해: 절편은 다르고 기울기는 동일
(coef(m3)[[1]] + coef(m3)[[3]] + coef(m3)[[2]] * 3) %>% round(2)

# 기울기와 절편
coef(m3)[[1]] + coef(m3)[[3]] # 라디오 절편
coef(m3)[[2]] # 기울기
predict(m3, tibble(cost = 0, ads = "라디오")) %>% round(2)

library(moderndive)

# 모델 확인
graph_3 <- ggplot(mydata, aes(cost, revenue, col = ads)) +
    geom_point() +
    moderndive::geom_parallel_slopes(se = F)

ggplotly(graph_3)

# 독립변수 2개 (또는 이상일 때) : interaction term model -----------------------------
# interaction 모형
m4 <- lm(revenue ~ cost * ads, data = mydata)
summary(m4)

# 예측
predict(m4, data.frame(cost = 3, ads = "라디오")) %>% round(2)

# 계수이해 : 절편과 기울기 둘다 차이
(coef(m4)[[1]] + coef(m4)[[3]] + coef(m4)[[2]] * 3 + coef(m4)[[5]]  * 3) %>% round(2)

# 기울기와 절편
coef(m4)[[1]] + coef(m4)[[3]] # 라디오 절편
predict(m4, tibble(cost = 0, ads = "라디오")) %>% round(2)
coef(m4)[[2]] + coef(m4)[[5]] # 라디오 기울기 

# 모델 확인
graph_4 <- ggplot(mydata, aes(cost, revenue, col = ads)) +
    geom_point() +
    # theme(text = element_text(family = "AppleGothic")) +
    geom_smooth(method = "lm", se = F)

ggplotly(graph_4)

# 모델 비교 : 그래프
gridExtra::grid.arrange(graph_1, graph_2, graph_3, graph_4, ncol = 4)

# 모델 비교 : 잔차비교를 통한 성능 비교
models <- list(m1, m2, m3, m4)

models %>% 
    map(~ sum(resid(.x)^2)) %>% 
    set_names(c("simple_linear", "anova", "parallel_slope", "interaction")) %>% 
    as_tibble()

# 설명력 비교
models %>% 
    map(summary) %>% 
    map("r.squared") %>% 
    set_names(c("simple_linear", "anova", "parallel_slope", "interaction")) %>% 
    as_tibble()

# 다항회귀(2차식 이상) ------------------------------------------------------------
m5 <- lm(revenue ~ cost + I(cost^2), data = mydata)
summary(m5)

# 모델 확인
graph_5 <- ggplot(mydata, aes(cost, revenue)) +
    geom_point() +
    theme(text = element_text(family = "AppleGothic")) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)

ggplotly(graph_5)

# 모델 비교 ----
models <- list(m1, m2, m3, m4, m5)

models %>% 
    map(~ broom::glance(.x)) %>% 
    set_names(c("simple_linear", "anova", "parallel_slope", "interaction", "poly")) %>% 
    bind_rows(.id = "model_name") %>% 
    select(model_name, deviance)

# 다중 회귀 -------------------------------------------------------------------
data(state, package = "datasets")
states <- as_tibble(state.x77)
glimpse(states)

# 변수 정리
states <- janitor::clean_names(states)
states <- states %>% select(1:3, 5:7)
glimpse(states)

m1 <- lm(murder ~ ., data = states) # . : all(placeholder, 이전 결과물)
# (murder ~ population + ... + frost)
summary(m1)
# 44 degrees of freedom: 50 - 6 

# 유의미한 특성만 뽑아서 모델에 다시 적용: 설명력이 조금 오름
temp_m <- lm(murder ~ population + illiteracy, data = states)
summary(temp_m)

# 자동으로 뽑기 위한 실험
# default fit
m1 <- lm(murder ~ ., data = states)
summary(m1)

temp_m <- lm(murder ~ population + illiteracy, data = states)
summary(temp_m)

# auto fit
m2 <- stats::step(m1, direction = "both") # backward(후진소거법), forward(전진선택법)
summary(m2)
# 자동으로 유의한 변수를 선택함
# AIC는 작을수록 좋음

# 분산 분석 테이블
anova(m2, m1) # 두 모델의 차이가 없을 때는 간결한 모델을 사용한다.

#### 숫자 값 예측(numeric prediction) 회귀모형
# 4 ~ 5가지 비교, 잔차 적은 것 비교(1개 변수 ~ 다중회귀까지 만듬)

# 2차변수 포함(interaction term을 몇 개 만드는게 나은가? 영국 vs 미국 상충)
# 2차변수의 경우 혼자 해보고 질문 받음 

#########################################################################################
# 로지스틱 회귀(logistic regression): 분류 문제
dp <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/datamining/DeathPenalty.csv")
str(dp)

# 변수정의 : 변수 정의를 보면 무슨 생각이 드나요?
# Agg(범죄, 1은 경범죄, 6은 흉악범죄) 
# VRace(피해자, 유색인 = 0, 백인 = 1) 
# Death(사형 = 1)

# 용어 정리
# 1. odds(성공/실패) => p/(1-p) = (2/3)/(1/3) = 2/1 = 2, odds ratio  => odds vs odds 
# 낚시 3일, 잡은날 2일, 놓친날 1일 => odds = 2 / 1, 성공 prob = 2 / 3
# 2. logit transformation (로짓변환): odds에 log 취함 => log((2/3)/(1/3))
   # log(odds)
   # 이유: [a,b](유한) =! a+bx(무한) 둘다 무한하게 만드려면, log 씌움 => log(odds) = a+bx 

names(dp)

# 데이터 유형 변환
dp <- dp %>% 
    setNames(c("agg", "vrace", "death")) %>% 
    mutate(vrace = factor(vrace, levels = c(0, 1), labels = c("others", "white")),
           death = factor(death, levels = c(0, 1), labels = c("alive", "dead")))

# 모델 작성(glm: Generalized Linear Model)
# 1) 모든 모형은 예측을 해줘야 함

m <- glm(death ~ agg + vrace, family=binomial, data = dp)
# glm(formula, family=family(link=function), data)
# family = 정규분포 gaussian/이항분포 binomial/포아송분포 poisson/
         # 역정규분포 inverse.gaussian/감마분포 gamma/
         # 응답분포가 확실하지 않은 때, 유사가능도 모형 quasi
summary(m)
exp(coef(m))

dp2 <- tibble(dp, prob = fitted(m))
head(dp2)

ggplot(dp2, aes(agg, prob, group = vrace)) + 
    geom_line(aes(color = vrace)) 

predict(m, tibble(agg = 4, vrace = "white"), type = "response")
predict(m, tibble(agg = 4, vrace = "others"), type = "response")

dp2 <- dp2 %>% mutate(label = ifelse(prob > 0.5, 1, 0))
head(dp2)


