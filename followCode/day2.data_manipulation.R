# 오늘: factor 데이터, 결합 & 조인, 반복문
ls()
rm(list = ls())

library(tidyverse)
library(tidymodels)

# 범주형(factor)데이터 ------------------------------------------------------
# 범주 순서 변경 시, 값이 바뀌지 않도록 주의

# 범주 순서 변경
sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes
sizes <- fct_relevel(sizes, c("small", "medium", "large"))
sizes
# >Levels: large medium small
# >Levels: small medium large

# 대/중/소를 factor로 만들고 레벨순서 변경
size <- factor(c("대", "대", "소", "중"))
size
size <-fct_relevel(size, c("대", "중", "소"))
size

# 요일을 연습
요일 <- factor(c("월", "화", "수", "목", "금", "토", "일"))
day <- 요일 %>% 
    fct_relevel(c("월", "화", "수", "목", "금", "토", "일"))
day
# > levels(요일)  [1] "금" "목" "수" "월" "일" "토" "화"
# > levels(day)   [1] "월" "화" "수" "목" "금" "토" "일"

# 테이블 결합 ----------------------------------------------------------------
# rbind, bind_rows
a <- data.frame(
    가 = c("James", "Tom", "Michael"),
    나 = c(50000, 60000, 70000),
    다 = c(35, 40, 45))

b <- data.frame(
    가 = c("Jane", "Juliet", "Emma"),
    나 = c(60000, 70000, 80000),
    다 = c(30, 35, 40))

c <- rbind(a, b); c
a %>% bind_rows(b)
bind_rows(a, b)

# cbind, bind_cols
cbind(a, b)
a %>% 
    bind_cols(b) %>% 
    set_names(nm = c("가", "나", "다", "가", "나", "다"))

# 테이블 조인 ----------------------------------------------------------------
kids <- c("철수", "순이", "동수", "영희")
address <- c("서울", "인천", "대전", "부산")
df1 <- tibble(kids, address)
df1

df2 = tibble(
    kids = c("순이", "영철", "길동"),
    ages = c(10, 7, 12))
df2

# 다양한 조인(그림 찾아보기)
inner_join(df1, df2)
left_join(df1, df2)
right_join(df1, df2)
full_join(df1, df2)
anti_join(df1, df2)
semi_join(df1, df2)

# 필드명이 다를 때
df3 <- tibble(
    pals = c("길동", "동수", "영희"),
    ages = c(12, 11, 9))

inner_join(df1, df3, by = c("kids" = "pals"))
full_join(df1, df3, by = c("kids" = "pals"))
left_join(df1, df3, by = c("kids" = "pals"))

# 특수 조인(age를 x, y로 나누지 않고 합치기)
library("rquery")
library("rqdatatable")

df1 %>% 
    left_join(df2) %>% 
    natural_join(df3, by = c("kids" = "pals"), jointype="LEFT")

# tidyverse 반복문 -----------------------------------------------------------
for (i in 1:3){
    print(i^2)
}

# 벡터 반복
c(1:3) %>% map(function(x) x^2)
c(1:3) %>% map(~ .x^2) # 리스트 반환(function이 길면 ~로, .x로 변환)
c(1:3) %>% map_dbl(~ .x^2) # 벡터 반환(dbl=double)

# 열 반복
iris %>% 
    select_if(is.numeric) %>% 
    map_df(mean)

# 데이터 프레임 반복
iris %>% 
    split(.$Species) %>%  #.은 iris임(baseR split만 사용가능, 종으로 나누기)
    map_df(head, 1) # 각행을 반복하고, df=dataframe으로 반환(map쓰면 list 반환)


# 모델 반복(base모델 여러개 돌려보기) ----------------------------------------
# petal.length&petal.width 선형관계
# 점위에 선형 > 모형, 적합시킨다, fitting
# 절편, 기울기 > 모수(모집단을 대표하는 대표값)

mod1 <- Petal.Length ~ Petal.Width              # y = a+bx == y ~ x
mod2 <- Petal.Length ~ Petal.Width + Species

models <- list(mod1, mod2)

models %>% 
  map(~ lm(.x, data = iris)) %>%                # lm(): linear regression(회귀)
  map(tidy) %>%                                 # tilde: 틸드
  set_names(nm = c("model_1", "model_2")) %>% 
  bind_rows(.id = "model.name")

# ~ 사용하지 않음
models %>% 
  map(lm, data = iris) %>%  
  map(tidy) %>% 
  set_names(nm = c("model_1", "model_2")) %>% 
  bind_rows(.id = "model.name")

# ~ 대신 function 사용
models %>% 
  map(function(x) lm(x, data = iris)) %>%  
  map(tidy) %>% 
  set_names(nm = c("model_1", "model_2")) %>% 
  bind_rows(.id = "model.name")






