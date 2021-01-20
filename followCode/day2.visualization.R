# rm(list=ls())

# ggplot2 문법
# 데이터
ggplot(data = iris)

# x축과 y축 지정: aes
ggplot(iris, aes(Petal.Length, Petal.Width))

# 그래픽 요소 표현: geom_*
ggplot(iris, aes(Petal.Length, Petal.Width)) + 
    geom_point()

# layer 개념: +
ggplot(iris, aes(Petal.Length, Petal.Width)) +
    geom_point() +
    geom_smooth(method = "lm")

# 그래픽 요소 : alpha 외
ggplot(iris, aes(Petal.Length, Petal.Width)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = 'lm')

# 그래픽 요소 : alpha, col (그룹별 모형을 만들기 위해서 col 사용)
ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
    geom_point(alpha = 0.7)

# gather 함수
mtcars %>% 
    as_tibble() %>% # 10개씩 보고싶어서 tibble
    rownames_to_column(var = "car_model") %>% # 변수로 만들어 줌
    gather(key = "variables", value = "values", -car_model) #str빼고 수치 key하위로

# 여러 화면: facet_wrap, facet_grid
iris %>% 
    as_tibble() %>% 
    gather(key, value, -Species) %>% 
    ggplot(aes(factor(0), value, fill = Species)) + # factor(0):그냥, fill: 종별 면적 색
    geom_boxplot() +
    facet_wrap(~ key, scales = "free_y") # ~ : 변수명(key)별, 그림그리는 약속
    # free_y

# 행과 열을 표현..?
iris %>% 
    as_tibble() %>% 
    gather(key, value, -Species) %>% 
    ggplot(aes(factor(0), value, fill = Species)) +
    geom_boxplot() +
    facet_grid(key ~ Species, scales = "free_y")

# wrap: 통으로 넣어서 그리기 grid: 행-키(key), 열-종(Species)


#####################################################################################
# install.packages("plotly")
# rm(list=ls())

library(tidyverse)
library(plotly)

# 분포 그래프 ----------------------------------
# 히스토그램 / distplot: count, 빈도 / bin값으로 잘라서 몇 번 나오는지?

data(iris)

ggplot(iris, aes(x = Petal.Length)) +
    geom_histogram(color = "grey")

ggplot(iris, aes(x = Petal.Length, fill = Species)) +
    geom_histogram(color = "grey")

# 계급구간 조정(1.bin 조정, 2.구간조정)

# 분포는 데이터의 커짐의 정도를 말함
# 이 데이터가 퍼져 있다는 것은 중간으로부터 어디로 퍼져있는지 확인할 수 있음
# 평균, 중앙값, 분산, 표준편차 
# 종 형태의 분포 == 정규분포

# 전제) 분포가 끊어져 있어 적어도 2개 이상의 데이터 특성으로 나눠져 있다
ggplot(iris, aes(Petal.Length)) + 
    geom_histogram(col = "grey", binwidth = 0.5)

# 검토) 종을 기준으로 색상 채우면 전제 확인 가능
ggplot(iris, aes(Petal.Length, fill=Species)) +     
    geom_histogram(col = "grey", binwidth = 0.5)

# 여러개의 히스토그램
iris2 <- iris %>% 
    as_tibble() %>% 
    gather(key, value, -Species)

# 종별로 표현
ggplot(iris2, aes(x = value, group = Species)) +
    geom_histogram(col ="grey")+
    facet_wrap(~ key)

# 종별로 색상 표현
ggplot(iris2, aes(x = value, fill = Species)) +
    geom_histogram(col ="grey")+
    facet_wrap(~ key)


# 추가 문법
ggplot(iris2, aes(x = value)) +
    geom_histogram(col = "grey") +
    facet_grid(key ~ .)

ggplot(iris2, aes(x = value, fill = Species)) + 
    geom_histogram(col = "grey") +
    facet_grid(key ~ .)

# 밀도 그림
iris2 %>% 
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(~ key)

ggplot(iris2, aes(x = value, col = Species))+
    geom_density() +
    facet_grid(key ~., scales = "free")

iris2 %>% 
    ggplot(aes(x = value, y = ..density..)) +
    geom_histogram(color = "grey") +
    facet_wrap(~ key) +
    geom_density(color = "red")

# 박스 플랏
p1 <- iris %>% 
    ggplot(aes(factor(0), Petal.Length)) +
    geom_boxplot() +
    # geom_point()
    geom_jitter(alpha = 0.5, width = 0.1, aes(col = Species)) #jitter:점을 흩뿌림

ggplotly(p1)

# > mtcars %>% ggplot(aes(factor(0), mpg)) + geom_boxplot() + geom_jitter(alpha = 0.5, width = 0.1) -> temp
# > ggplotly(temp)    
# 중앙값 빼고 위아래 15개

# 종별 박스플랏
p2 <- iris %>% 
    select(Petal.Length, Species) %>% 
    ggplot(aes(Species, Petal.Length)) +
    geom_boxplot() +
    geom_jitter(aes(col = Species), alpha = 0.3, width = 0.1) +
    theme(legend.position =  "none") # 범례없음

ggplotly(p2)

# 비교 그래프 -----------------------------------------------------------------
# 범주형 데이터 다루기
# outlier => 정확도를 올리기 위해 

# 막대 그래프
data(mtcars)
glimpse(mtcars)

# 데이터 타입 변환
mtcars <- mtcars %>% 
    mutate_at(vars(cyl, vs, am), as.factor)

# 기어 및 실린더별 데이터 수
n_mtcars <- mtcars %>% 
    group_by(am, cyl) %>% 
    count()

n_mtcars

# 기어 및 실리더별 막대 그래프
ggplot(n_mtcars, aes(cyl, n, fill = am)) +
    # geom_bar(stat = "identity", position = "dodge") 
    geom_col(position = "dodge") #stack, fill
# dodge: 막대기 펼쳐져 있음
# stack: 쌓기

ggplot(n_mtcars, aes(cyl, n, fill = am)) +
    geom_col(position = "fill") #stack, fill (최대1)

ggplot(n_mtcars, aes(cyl, n, fill = am)) +
    geom_col(position = "stack") #stack, fill

# 범례명 변경
ggplot(n_mtcars, aes(cyl, n , fill =am)) +
    geom_col(position = "fill") +
    scale_fill_discrete(name = "기어", labels = c("자동", "수동"))
    # + theme(text = element_text(family = "AppleGothic")) # 맥

# 비교그래프(모델 비교)--------------------------------------------------------------
# 정규독립등분산: 각 비교군의 개수가 동일해야 함
# 비슷한 분포를 가지고 있기 때문에 비교군의 개수가 달라도 비교 가능

data("ChickWeight")
chicks <- ChickWeight
head(chicks)

# weight : 병아리 체중(mg)
# Time   : 성장일수(일)
# Chick  : 병아리 id
# Diet   : 모이

# 병아리 데이터 보기
# 18, 16, 15의 데이터가 다른 데이터보다 적음
chicks %>% 
    as_tibble() %>% 
    select(Chick) %>% 
    group_by(Chick) %>% 
    count() %>% 
    view()

# 데이터 수가 작은 병아리 이유는?
# 18, 16, 15 + 일반(13) 데이터 Time 별 Weight 그래프. 일찍 죽음 
chicks %>% 
    filter(Chick %in% c(18, 16, 15, 13)) %>% 
    ggplot(aes(Time, weight, group = Chick)) +
    geom_line(aes(col = Chick))

# 모이별 병아리 보기
chicks %>% 
    ggplot(aes(Time, weight, group = Chick, col= Diet)) + # col=색상
    geom_line()

# 모이별로 나누어 보기
# 1번 사료를 먹는 병아리가 제일 많다
# 비슷한 분포를 가지고 있기 때문에 비교군의 개수가 달라도 비교 가능
chicks %>% 
    ggplot(aes(Time, weight, group = Chick, col = Diet))+
    geom_line() +
    facet_wrap(~ Diet) +              #다이어트 기준으로 여러개 창 생성
    theme(legend.position = "none")

# 비교를 위해 평균선을 넣기기
chicks %>% 
    ggplot(aes(Time, weight)) +
    geom_line(aes(group = Chick, col = Diet)) +
    facet_wrap(~ Diet) +
    geom_smooth() +
    theme(legend.position = "none")
 
# 평균만 추출해서 보기
# 3번의 평균이 표준편차를 다 고려해도, 가장 높음(몸무게 가장 높다)
chicks %>% 
    ggplot(aes(Time, weight, col = Diet)) +
    geom_smooth(method = "loess") #loess: 국소회귀분석(선형회귀분석=lm)

# mtcars %>% ggplot(aes(wt, mpg)) + geom_point() +facet_grid(~cyl) #열기준(1개)
# mtcars %>% ggplot(aes(wt, mpg)) + geom_point() +facet_wrap(~cyl+am) #열기준(2개)
# mtcars %>% ggplot(aes(wt, mpg)) + geom_point() +facet_grid(cyl~am) #행/열기준

# 관계 그래프 ------------------------------------------------------------------------
# 전체 플랏
library(GGally)
ggpairs(iris)

# 두 변수 관계 보기
iris %>% 
    ggplot(aes(Petal.Width, Petal.Length))+
    geom_point(aes(col = Species))

# 모형적합: 데이터의 분포를 나타내는 직선
# 점: 실제치, 실선: 적합치이자 예측치
# 실제치와 예측치의 거리의 합을 비교해서 좋은 모형을 선택함(값이 작은게 좋은 모델)

# 모형적합1: 하나의 직선(모형)
iris %>% 
    ggplot(aes(Petal.Width, Petal.Length)) +
    geom_point(aes(col = Species)) +
    geom_smooth(method = "lm") 

# 모형적합2: 종 별 직선(모형)
iris %>% 
    ggplot(aes(Petal.Width, Petal.Length, col = Species)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = F) 

# 다수의 모형 적합
p3 <- iris %>% 
    ggplot(aes(Petal.Width, Petal.Length)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = lm, se = F, col = "black") +  #se: 표준오차 나타내기 여부
    geom_smooth(method = loess, se = F, col = "yellow") +
    geom_smooth(method = lm, formula = y~I(x^2), se = F, col = "purple")
    
ggplotly(p3)


# 모집단에서 표본으로 조사했기 때문에 표본이 모집단의 대표값을 가져야 함

# 구성비 그래프 ---------------------------------------------------------------------
data("diamonds")
glimpse(diamonds)

diamonds %>% 
    group_by(cut, color, clarity) %>% 
    summarise(count = n()) %>% 
    ggplot(aes(color, count, fill = cut)) +
    geom_col(position = "fill") +
    facet_grid(. ~ clarity)

#####################################################################################
# rm(list=ls())

library(tidyverse)
library(gridExtra)
data(diamonds)

glimpse(diamonds)
# <ord> ordered factor: 순서가 있는 범주형 데이터
levels(diamonds$cut)
levels(diamonds$color)

g1 <- diamonds %>% 
    ggplot(aes(carat, price, color = color)) +
    geom_point(alpha = 0.1)
    
g1

# 모형을 만들어서 뿌려줌. 1~2캐럿 사이는 일반화 가능
# 변수간의 관계를 잘 모르는 경우에 이런식으로 확인 가능
g1 + geom_smooth(method = "loess") 

# 가중치를 반영한 그래프 ----------------------------------------------------------
data(midwest)
glimpse(midwest)

# 변수정의(집계데이터)
# percwhite: 백인의 비율(집계:arrgegate)
# percvelowpoverty: 빈곤율(집계:arrgegate)
# poptotal: 동네 주민 수(raw data)

# 두 변수 선택
p <- ggplot(midwest, aes(percwhite, percbelowpoverty))

# 가중치 적용(백인비율 높을수록 빈곤율 낮아짐, 우하향)
a <- p + geom_point(alpha = 0.5) +
    geom_smooth() +
    scale_y_continuous(limits = c(0, 100))

b <- p + geom_point(aes(size = poptotal/1e6), alpha = 0.5) +  # size =전체인구/1,000,000
    geom_smooth(aes(weight = poptotal/1e6)) +  
    scale_y_continuous(limits = c(0, 100)) +
    theme(legend.position = "none")

# 두 그래프 비교: 집계 데이터를 환산한 오른쪽이 더 잘 표현해줌(점 크기)
grid.arrange(a, b, ncol = 2)


# 이번엔 흑인 인구 비율 데이터 적용: 우상향
p2 <- ggplot(midwest) +
    aes(percblack, percbelowpoverty) +
    scale_y_continuous(limits = c(0, 100))

a2 <- p2 + geom_point() + geom_smooth()

b2 <- p2 + geom_point(aes(size = poptotal/1e6)) +
    geom_smooth(aes(weight = poptotal/1e6)) +
    theme(legend.position = "none")

gridExtra::grid.arrange(a2, b2, ncol = 2)

# device (tag data) 보고 싶을 때 ------------------------------------------------------
# 공정 내 tag 데이터
set.seed(1234)
n <- 10000

c1 <- matrix(rnorm(n, mean = 0, sd = .05), ncol = 2)
c2 <- matrix(rnorm(n, 3, 2), ncol = 2)

mydata <- rbind(c1, c2) %>% 
    as_tibble() %>% 
    rename(x = V1, y = V2)

ggplot(mydata, aes(x, y)) + 
    geom_point(alpha = 0.05)

library(hexbin)
ggplot(mydata, aes(x, y)) +
    geom_hex()

# 시계열 그래프 작성 --------------------------------------------------------------
data(economics)
glimpse(economics)

# pce : 개별 소비지출
# pop : 총인구
# psavert : 개별 저축률
# uempmed : 실업기간의 중앙값(주)
# unemploy : 실업자수(천명)

econ <- economics
econ <- econ %>% gather(key, values, -date)
head(econ)

# 모든 변수 보기
library(scales)

econ %>% 
    ggplot(aes(date, values, group = key)) +
    geom_line(aes(color = key)) +
    scale_y_continuous(label = comma) +
    facet_grid(key ~ ., scale = "free_y") +
    theme(legend.position = "none") 

# 특정 변수 보기(내장 함수 사용)
econ %>% 
    filter(key %in% c("unemploy", "uempmed")) %>% 
    group_by(key) %>% 
    mutate(value2 = scale(values)) %>%     # scale: 정규화
    ggplot(aes(date, value2, col = key)) +
    geom_line()

# 특정 변수 보기 (사용자 정의 함수 사용)
# 정규화1(min-max) : x-min(x)/(max(x)-min(x))
range_f <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / diff(rng)
} 

# 정규화2(min-max)
range_f <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / max(rng) - min(rng)
} 

econ %>% 
    filter(date >= "2000-01-01", date <= "2010-12-31") %>%
    filter(key %in% c("unemploy", "uempmed")) %>%
    group_by(key) %>%
    mutate(value2 = range_f(values)) %>%
    ggplot(aes(date, value2, color = key)) + 
    geom_line()
    