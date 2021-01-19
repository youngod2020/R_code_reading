# 한국지능형사물인터넷협회, 오픈소스 통계 툴[R] 기반 빅데이터 
## DAY1: data manipulation
# 1.1

# 1개 설치
# install.packages("tidyverse")

# 2개 설치
# install.packages(c("rio", "DT"))

# 여러개 설치(3개 이상)
# pkgs <- c("tidyverse", "rio", "DT")
# install.packages(pkgs)

# 설치된 라이브러리 불러오기
library(tidyverse) 
# base R 문법 = tidyverse 문법

# 설치된 라이브러리 여러개 불러오기
pkgs <- c("rio", "DT")
sapply(pkgs, require, character.only = T) #pkgs에 require함수 적용 
# require: library와 유사, 설치 안 한 패키지 불러오면 false, 설치한 패키지 불러오면 true 반환

# 기본 명령어 ----------------------------------------------------------------------------------------
search()        # 로드된 패키지 확인 / 내 환경에 어떤 패키지가 있는지? 21개 (메모리에 뭐가 있는지?)
ls()            # 객체 확인 (메모리에 뭐가 있는지?)
list.files()    # 파일 확인 (파일에 뭐가 있는지?)
getwd()         # 경로 확인 (get workingdirectory)
# setwd() : 워킹디렉토리 설정하기 
# (우측: Files> Home/kyj/연습/R >More> Set As workingdirectory: 현재 경로로 지정됨)

data(iris)      # 샘플데이터 불러오기, 그냥 iris 해도 됨
iris            # 150개 데이터 다 나옴
head(iris, 3)   # 상위 3줄 / 기본 6줄
tail(iris, 3)   # 하위 3줄 / 기본 6줄
summary(iris)   # 데이터 요약
names(iris)     # 열이름, 행이름
str(iris)       # structure(데이터구조) 데이터프레임, 150행, 5컬럼, character와 factor의 차이는?   
glimpse(iris)   # str(structor)과 동일, 행렬(150, 5) > 라이브러리 tidyverse 불러와야 가능

view(iris)      # 엑셀시트처럼 보기 
datatable(iris) # 동적시트로 보기(데이터 많은 경우)

## 추가
# data(mtcars)  # 샘플 자동차 데이터 가져오기
# mtcars        # 자동차 데이터 
# r에서 작업하는 것은 ram에 올리는 것이기 때문에 새로 시작하는 경우 다시 불러오기 해야 함
# 함수가 적용되되지 않는다면,  {?함수명}으로 콘솔에 검색하여 불러와야하는 라이브러리 확인

# 데이터 쓰기 및 읽기 -------------------------------------------------------------------------------
# 쓰기
export(iris, "new.xlsx")    # 지정된 경로에 엑셀로 저장하기
export(iris, "new.csv")     # 지정된 경로에 csv로 저장

# 읽기
# rm(new)      # 객체에서 괄호안 이름의 객체 삭제
new <- import("new.xlsx")   # new.xlsx를 new1으로 불러오기
new2 <- import("new.csv")    # new.csv를 new2로 불러오기
# ls()

# 메모리 정리하기 -----------------------------------------------------------------------------------
rm(iris)                                    # 1개 지울 때
rm("new", "new2")                           # 2개 지울 때 
rm(list = ls(pattern = "new"))              # 객체에 new 단어가 들어간 패턴 지울때
rm(list = ls())                             # 모든 객체 지울 때
rm(list = paste0("newiris", seq(1, 3, 1)))  # 구간 지울 때, seq(1, 3 ,1) >> 1, 2, 3 (1~3까지 1 간격)
                                            # paste0+"단어(new)"+seq >> "new1" "new2" "new3"로 출력

# 객체(object) 정의 및 객체 만들기 ------------------------------------------------------------------
# R에는 vector, matrix, array, dataframe, list 객체 존재

# vector: 방향과 크기가 있는 단위
v1 <- c(1:10)                   # 1~10까지 벡터, seq(1, 10, 1)수열로도 표기 가능
v2 <- c(letters[1:10])          # a,b,...,j
v3 <- rep(c(T, F), times=5)     # Replicate(true false)를 5번 반복

class(v1) ; v1                  # integer(벡터형태), v1의 값
class(v2) ; v2                  # character, v2의 값
class(v3) ; v3                  # logical, v3의 값

# vector 단어
length(v1)                      # 길이(10)
range(v1)                       # 범위(1 10)

rep(c(T, F), 2) # True False 번갈아가며 2번 반복
rep(c(T, F), each = 2) # True False 각각 2번 반복

seq(1, 10) # 1 ~ 10, 1 간격
seq(1, 10, by = 2) # 1 ~ 10, 2 간격
seq(1, 10, length = 5) # 1 ~ 10, 동일 간격으로 값 5개 

all(v1 > 0) # 전체 v1 0보다 크다(logical 결과)
any(v1 < 0) # 전체 v1 0보다 작다(logical 결과)

paste("iris", v1) # 단어 뒤 여백 O
paste0("iris", v1) # 0은 단어 뒤 여백 X

# matrix
m1 <- matrix(1:12, ncol = 3) # r은 컬럼 기준, 데이터 세로로 들어감(julia, R,...)
# matrix(1:12, ncol= 3, byrow=T) # 파이썬과 같이 행 기준으로 데이터 입력
m2 <- matrix(letters[1:12], ncol=3)
m3 <- matrix(rep(c(T,F), 6), nrow =4)

class(m1) ; m1
class(m2) ; m2
class(m3) ; m3

# matrix 단어
apply(m1, 1, sum) # 더하기(행)
apply(m1, 2, sum) # 더하기(열)
apply(m1, 1, mean)
apply(m1, 2, median)
apply(m1, 2, function(x) x^2) # 함수 직접 만들어 쓰기(열)

# array
a1 <- array(1:12, c(2, 3, 2)) # 2행 3열 1배열, 2배열
a2 <- array(letters[1:12], c(2,3,2))

# data frame(==tibble, worksheet)
df <- data.frame(v1, v2, v3) # 컬럼명 v1, v2, v3
class(df) # data.frame
df

dim(df)
colnames(df)
nrow(df)
ncol(df)
head(df)
ggplot(df) # ggplot은 dataframe(tibble)만 받음

class(m1)

# list
my_list <- list(v1, m1, df) # vector, matrix, dataframe을 한번에 담음
class(my_list)
str(my_list, 1)
str(my_list[1])
str(my_list[[1]])

# subset object
my_list                     # list 전체 불러오기
my_list[1]                  # list 전체 중에 1번 불러오기
my_list[[1]]                # list 1번만 불러오기(-----값 선택 가능----)
my_list[[1]][1]             # list 1번 중에 첫번째 값
my_list[[1]][1:2]           # list 1번 중에 1~2번째 값

my_list[3]
my_list[[3]]                # list 3번만 불러오기
my_list[[3]][1]             # list 3번의 1열 가져오기
my_list[[3]][1:2]           # list 3번의 1,2열 가져오기
my_list[[3]][[1]]           # list 3번의 1열만 가져오기(----값 선택 가능----)
my_list[[3]][[1]][1]        # list 3번의 1열의 첫번째 값
my_list[[3]][[1]][1:2]      # list 3번의 1열의 1~2번째 값

# list 단어
my_list2 <- list(c(1:3), c(4:7), c(7:11))
my_list2

lapply(my_list2, mean)      # list apply > lapply
lapply(my_list2, first)
lapply(my_list2, last)
lapply(my_list2, function(x) append(x, 1, after = 0)) # 1을 리스트의 0 다음에 넣어라 == 첫번째로 넣어라
lapply(my_list2, function(x) append(x, 10, after = length(x))) # 10을 길이 뒤에 넣어라 == 마지막에 넣어라

# pipe operator (%>%) 정의 및 편리성 ---------------------------------------------------------------
# 1. 이전 결과를 다음 함수의 첫번째 인수로 넘김
# 2. 코드 가독성 증가
# 3. 유저가 생각하는 만큼 연결 가능

# head(iris, 3) == iris %>% head(3)
my_list2 %>% map(mean)
my_list2 %>% map(first)
my_list2 %>% map(last)

my_list2 %>% map(~ append(.x, 1, after=0))
my_list2 %>% map(~ append(.x, 10, after=length(.x)))

my_list2 %>% map(plot)

my_list2 %>% 
    map(as_tibble) %>% 
    map(~ ggplot(.x, aes(x = value)) + geom_bar())

# SQL 몰라도 DB에서 데이터 뽑을 수 있음
# 뽑은 데이터로 그림 그릴 수 있음

###############################################################################

# 1.2
# 1.tidyverse 패키지
  # a. 행추출
  # b. 열추출
  # c. 변수 생성
  # d. 정렬
  # e. 요약

# 2. 인터넷에 있는 테이블 데이터를 읽어온다.
  # a. tabular data only
  # b. text, image, voice data는 deep learning 할 때 하면 됩니다.


# 행 차원 명령어 -----------------------------------------------------
library(tidyverse)

# 행번호를 변수로 정의
iris <- iris %>% 
    as_tibble() %>% 
    rownames_to_column(var = "id")

# 특정행 추출
iris %>% 
    slice(c(1,3,5))

# 홀수행 추출
iris %>% 
    slice(seq(1, nrow(.), by = 2)) #seq 1 ~ 끝번호까지 2간격(홀수)로 추출

# 조건행 추출
iris %>% 
    filter(Species == "setosa")

iris %>% 
    filter(Petal.Length >= mean(Petal.Length))

# and(&) or(|), comma는 &
iris %>% 
    filter(Species == "versicolor", Petal.Length < mean(Petal.Length))

# 열차원 명령어 ------------------------------------------------------
# 특정열 추출
iris %>% 
    select(1, 3, 5)

# 특정열 제외 추출
iris %>% 
    select(-Species)

# 조건열 추출
iris %>% 
    select(starts_with("Petal")) # petal로 시작하는 열

iris %>% 
    select(ends_with("Length"))  # length로 끝나는 열

iris %>% 
    select(contains(".")) # 점을 포함하는 열 

# 변수 범위로 조건 지정
iris %>% 
    select(Sepal.Length:Petal.Length)

# iris %>% group_by(Species) %>% summarise_at(2:5, mean) 복수의 통계치 구하기 summarise_at


# 변수 생성
# 연산에 의한 변수 생성
iris %>% 
    mutate(Petals = Petal.Length * Petal.Width)

iris %>% 
    mutate(logical = Sepal.Length >= mean(Sepal.Length))

# 조건에 의한 변수 생성
# 조건 2개
iris %>% 
    mutate(Species2 = ifelse(Species == "setosa", 1, 0)) #setosa면 1, 아니면 0

# 조건 3개
iris %>% 
    mutate(Species3 = case_when(Species == "setosa" ~ 1,
                                Species == "versicolor" ~ 2,
                                TRUE ~ 3))

# 정렬 -------------------------------------------------------------------------------------
# 오름 차순
iris %>% 
    filter(Petal.Length >= median(Petal.Length)) %>% 
    arrange(Petal.Length)

iris %>% 
    filter(Petal.Length >= median(Petal.Length)) %>% 
    group_by(Species) %>% 
    arrange(desc(Petal.Length), .by_group = T) %>%   #.by_group = T, 그룹별로 내림차순, 없으면 섞임
    view()


# 그룹정의 및 요약 ---------------------------------------------------------------
# 그룹별 데이터 수
iris %>% 
  count(Species)

# 그룹별 count, 평균, 중앙값
iris %>% 
      group_by(Species) %>% 
      summarise(counts = n(),
                means = mean(Petal.Length), 
                meds = median(Petal.Length))

# 그룹별 특정 변수의 평균 (if, at, all) 
iris %>% 
    group_by(Species) %>% 
    summarise_at(.vars = vars(-id), .funs = list(mean))

# if문
iris %>% 
    group_by(Species) %>% 
    summarise_if(is.numeric, mean)

# 모든 변수에 함수 적용
iris %>% 
    select(-id) %>% 
    group_by(Species) %>% 
    summarise_all(list(mean = mean, sd = sd))


##############################################################################################
library(httr)
library(XML)

url <- "http://en.wikipedia.org/wiki/List_of_countries_by_military_expenditures"

url <- GET(url)
url    

mytable <- rawToChar(url$content)
tbl <- readHTMLTable(mytable, which=3)

glimpse(tbl)

tbl

mytbl <- tbl %>% 
  rename(rank = V1, country = V2, spending = V3) %>% 
  slice(-1) %>% 
  mutate(rank = as.numeric(rank), 
         spending = as.numeric(spending))

mytbl %>% 
  mutate(new_var = ifelse(rank == 1, 0, 1)) %>% 
  mutate(new_cum = cumsum(spending * new_var))

mytbl %>%
  mutate(new_var = ifelse(rank == 1, 0, 1)) %>% 
  mutate(new_cum = cumsum(spending * new_var)) %>% 
  mutate(smaller_us = new_cum < spending[1]) %>% 
  filter(smaller_us == TRUE) 

