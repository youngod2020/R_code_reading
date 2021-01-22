rm(list=ls())
library(patchwork)

## gapminder data를 활용한 회귀분석 연습

## load library
pkgs <- c("tidyverse",
          "broom",
          "gapminder",
          "trelliscopejs")
sapply(pkgs, require, character.only = T)

## load data
data("gapminder")
gap <- gapminder

# explore data
glimpse(gap)
plot(gap)

gap %>% head(3)

gap %>% 
    gather(key, value, -c("country", "continent", "year")) %>% 
    ggplot(aes(year, value, group = continent)) +
    geom_smooth(aes(col = continent), se = F) + 
    facet_grid(key ~ ., scales = "free_y")

gap %>% 
    ggplot(aes(year, lifeExp, group = continent)) +
    geom_smooth(aes(col = continent), se = F)
    
gap %>% 
    ggplot(aes(year, gdpPercap, group = continent))



################################################################################
# ML(회귀)

# 1. load package --------------------------------------------------------------
pkgs <- c("doParallel", "tidyverse", "patchwork", "tidymodels")
sapply(pkgs, require, character.only = T)

# 2. 병렬처리 세팅 --------------------------------------------------------------
detectCores() %>% registerDoParallel()

# 3. 데이터 불러오기 -------------------------------------------------------------
data("Boston", package = "MASS")
glimpse(Boston) # Y = medv 가정합니다.

# 4. 결측치 중복치 이상치 확인 ---------------------------------------------------

# 결측치와 중복치 확인
Boston %>% map_df(~ sum(is.na(.x)))
Boston %>% filter(duplicated(.))

# 이상치 확인
Boston %>% 
    gather(key, value) %>%
    ggplot(aes(y = value, group = key)) +
    geom_boxplot() +
    facet_wrap(~ key, scales = "free_y")

# 이상치 보정
Boston %>% 
    gather(key, value) %>% 
    filter(key %in% c("crim", "dis", "lstat", "zn")) %>% 
    ggplot(aes(y = value, group = key)) +
    geom_boxplot() +
    facet_wrap(~ key, scales = "free_y") +
    scale_y_log10() # 이상치 보정을 위해 로그 값 취함

Boston %>% 
    select(chas) %>% 
    ggplot(aes(x = chas)) +
    geom_bar()
    
Boston %>% 
    select(rad) %>% 
    ggplot(aes(x = rad)) +
    geom_bar()


# 5. 데이터 탐색 -------------------------------------------------------------------
# 선형성
Boston %>% 
    gather(key, value, -medv) %>% 
    ggplot(aes(value, medv, group = key)) +
    geom_point(alpha = 0.2) +
    facet_wrap(. ~key, scales = "free_x") +
    geom_smooth()


# 상관계수 확인
corr_table <- Boston %>% 
    select(-medv) %>% 
    map(~ cor.test(.x, Boston$medv)) %>% 
    map_df(tidy, .id = "variable")


corr_table %>% select(variable, estimate, p.value)

corr_table %>% 
    ggplot(aes(reorder(variable, estimate), estimate)) +
    geom_col(aes(fill = estimate > 0)) +
    coord_flip() +
    theme(legend.position = "none")


# 공선성 확인
# 1) 선형관계가 있는 것
# 2) 2개 이상 값이 여러개면 다중공선성존재 > 제외/ 이유: 간결한 모델을 만들기 위해
# https://datascienceschool.net/03%20machine%20learning/06.04%20%EB%8B%A4%EC%A4%91%EA%B3%B5%EC%84%A0%EC%84%B1%EA%B3%BC%20%EB%B3%80%EC%88%98%20%EC%84%A0%ED%83%9D.html

library(corrplot)
Boston %>% 
    cor() %>% 
    corrplot::corrplot(method = "shade",
                       type = "lower",
                       shade.col = NA,
                       order = "hclust",
                       addCoef.col = "grey")


# 6. 모델링 ------------------------------------------------------------------

# 데이터 분할
split_data <- initial_split(Boston, prop = 0.7, strata = medv) # 'medv' 기준 층화추출
# 층화추출: 구간별로 7:3을 나누겠음
train_data <- training(split_data)
test_data <- testing(split_data)

Boston %>% ggplot(aes(medv)) + geom_histogram(col = "grey")
train_data %>% ggplot(aes(medv)) + geom_histogram(col = "grey") # 원 데이터랑 비슷
test_data %>% ggplot(aes(medv)) + geom_histogram(col = "grey")

# 데이터 전처리 정의
boston_recipe <- recipe(medv ~ ., data = train_data) %>% 
    step_mutate(rad = factor(rad)) %>% # rad 수치 > factor(x변수는 독립, numeric)
    step_dummy(rad) %>% 
    step_log(crim, dis, lstat, zn, base = 10, signed = T) %>%  #로그취하겠음
    step_zv(all_predictors()) %>% #독립변수 분산이 0이면 제거: y에 영향 줄 수 없음
    step_corr(all_predictors(), threshold = 0.75) %>% #독립변수 공선성 0.75넘으면 제거
    step_normalize(all_predictors())

boston_recipe

# 모델 정의
lm_model <- linear_reg() %>% 
    set_engine(engine = "lm") #선형회귀모델을 쓸꺼야

lm_model

# 워크프롤우 정의(파이프라인) 
lm_wflow <- workflow() %>% 
    add_recipe(boston_recipe) %>% 
    add_model(lm_model)

lm_wflow    

# 모델 적합
lm_fit <- lm_wflow %>% fit(train_data)
lm_fit # lstat(음), rm(양) 집 값에 영향을 많이 줌

# 모델 성능
lm_metrics <- lm_fit %>% 
    predict(train_data) %>% 
    bind_cols(medv = train_data$medv) %>% 
    metrics(truth = medv, estimate = .pred)

lm_metrics

lm_no_resamp <- lm_fit %>% 
    predict(train_data) %>% 
    bind_cols(medv = train_data$medv) %>% 
    ggplot(aes(medv, .pred)) +
    geom_point(alpha = .15) +
    geom_abline(col = "red") + 
    coord_obs_pred() +
    # theme(text = element_text(family = "AppleGothic")) +
    ggtitle(label = "리샘플링 없는 LM 실제값과 예측값 비교")

lm_no_resamp

# 7. 교차검증 -----------------------------------------------------------------

# 교차 검증은 왜 하는 것인가요?
# 모델의 안정성, parameter(모수:데이터가 정함)tuning / hyperparameter: 유저가 정함

# 10겹 교차검증 과정 추가
set.seed(1234)
boston_folds <- vfold_cv(train_data, v = 10)
boston_folds

# 모델 재적합
set.seed(1300)

lm_resamp <- lm_wflow %>% 
    fit_resamples(resamples = boston_folds,
                  metrics = metric_set(rmse, rsq, mae),
                  control = control_resamples(save_pred = TRUE))

# 재적합 결과 보기
lm_resamp
lm_resamp$.metrics[[1]]
lm_resamp$.notes[[1]]      # 없음
lm_resamp$.predictions[[1]]

# 재적합 metrics 보기
lm_resamp %>% collect_metrics()
lm_metrics # 1개 metrics

# fold별 validation값 보기
lm_predictions <- collect_predictions(lm_resamp)

lm_predictions %>% 
    ggplot(aes(medv, .pred, group = id)) +
    geom_point() +
    facet_wrap(~ id)

# 단일 validataion 값과 resampling validataion 값 비교
lm_with_resamp <- lm_predictions %>% 
    ggplot(aes(x = medv, y = .pred)) + 
    geom_point(alpha = .15) +
    geom_abline(col = "red") + 
    coord_obs_pred() +
    # theme(text = element_text(family = "AppleGothic")) +
    ggtitle(label = "리샘플링된 LM 실제값과 예측값 비교") 

lm_no_resamp + lm_with_resamp

# 8. random Forest 모델 -----------------------------------------------------
# boosting 
library(randomForest)

# 모델 정의
rf_model <- rand_forest() %>% 
    set_engine(engine = "randomForest") %>% 
    set_mode(mode = "regression")

rf_model

# 워크플로우 정의
rf_wflow <- workflow() %>% 
    add_recipe(boston_recipe) %>% 
    add_model(rf_model)

rf_wflow

# 모델 적합
rf_fit <- fit(rf_wflow, train_data)
rf_fit

# 모델 성능
rf_metrics <- rf_fit %>% 
    predict(train_data) %>% 
    bind_cols(medv = train_data$medv) %>% 
    metrics(truth = medv, estimate = .pred) 

rf_metrics ; lm_metrics

rf_no_resamp <- rf_fit %>% 
    predict(train_data) %>% 
    bind_cols(medv = train_data$medv) %>% 
    ggplot(aes(medv, .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(col = "red") + 
    coord_obs_pred() +
    # theme(text = element_text(family = "AppleGothic")) +
    ggtitle(label = "리샘플링 없는 RF 실제값과 예측값 비교") 

# Parameter Tuning 추가
rf_spec <- rand_forest(
    mode  = "regression",
    trees = tune(),
    mtry  = tune(),
    min_n = tune()) %>% 
    set_engine("randomForest")

boston_predictors <- select(train_data, -medv)

set.seed(1234)
rf_grid <- grid_random(
    trees(),
    finalize(mtry(), boston_predictors),
    min_n(),
    size = 30)

rf2_wf <- workflow() %>%
    add_recipe(boston_recipe) %>% 
    add_model(rf_spec)

rf2_results <- tune_grid(
    object    = rf2_wf,
    resamples = boston_folds,
    grid      = rf_grid,
    metrics   = metric_set(rmse, mae, rsq),
    control   = control_grid(save_pred = TRUE) )

# best metric 추출
rf_params <- rf2_results %>% select_best("rmse")

# best param 적재 및 적합
rf2_model <- rf_spec %>% finalize_model(parameters = rf_params)

rf_final <- rf2_model %>%
    fit(medv ~ . , data = train_data)

# 비교
rf2_resamp <- rf_final %>% 
    predict(train_data) %>% 
    bind_cols(medv = train_data$medv) %>% 
    ggplot(aes(medv, .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(col = "red") + 
    coord_obs_pred() +
    # theme(text = element_text(family = "AppleGothic")) +
    ggtitle(label = "리샘플링된 RF 실제값과 예측값 비교") 

rf_no_resamp + rf2_resamp

# 테스트 데이터 예측 및 비교
models <- list(rf_fit, rf_final)

models %>% 
    map(~ predict(.x, test_data)) %>% 
    bind_cols() %>% 
    rename(preds_rf_fit = .pred...1, preds_rf_final = .pred...2) %>% 
    map(~ cor.test(.x, test_data$medv)) %>% 
    map_df(tidy, .id = "model_name") %>% 
    ggplot(aes(model_name, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
    theme(text = element_text(family = "AppleGothic")) +
    ggtitle(label = "RF 모델별 예측값과 실제값 상관 계수") 


# 예측
predict(rf_final, new_data = test_data)
