rm(list=ls())

# load packages
pkgs <- c("tidyverse", "tidymodels", "doParallel", "janitor", "patchwork")
sapply(pkgs, require, character.only = TRUE) 

# register parallel
detectCores() %>% registerDoParallel()

# import data
titanic <- read_csv(
    "https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")

glimpse(titanic)

# 변수 정의
# survived : 생존 유무 (0 = 사망, 1 = 생존)
# pclass : 1등칸, 2등칸, 3등칸
# name : 승객명
# sex : 성별
# age : 연령
# siblings_spouses_aboard : 형제자매, 배우자 탑승자 수
# parents_children_aboard : 부모, 자녀 탑승자 수
# fare : 요금

# R에서는 혼동매트릭에서 실제와 예측의 위치가 바뀜

# define y variable
titan <- titanic %>% 
    clean_names() %>% 
    mutate(survived = factor(survived, levels = rev(c(0, 1))))
# survived 0, 1 이렇게 있는데, R에서 factor로 바꾸면 1, 2이렇게 되니까 0, 1로 지정해줌

glimpse(titan)

# NA, duplicate, outlier
titan %>% map_df(~ sum(is.na(.x)))
titan %>% filter(duplicated(.))

titan %>% 
    select_if(is.numeric) %>% 
    gather(key, value) %>% 
    ggplot(aes(key, value)) + 
    geom_boxplot() +
    facet_wrap(~ key, scales = "free")

titan %>% 
    ggplot(aes(factor(0), fare)) +
    geom_boxplot() +
    scale_y_log10()

# 요금에 따른 생존자 수는 어떤 모습일까?
fare_hist <- titan %>% 
    ggplot(aes(fare, fill = survived)) +
    geom_histogram(col = "grey") +
    facet_wrap(. ~ survived) + 
    theme(legend.position = "nonw")

fare_box <- titan %>% 
    select(fare, survived) %>% 
    ggplot(aes(x = survived, y = fare, fill = survived)) + 
    geom_boxplot() + 
    scale_y_log10() +
    theme(legend.position = "nonw")

fare_hist + fare_box

# 연령에 따른 생존자 수는 어떤 모습일까?
age_hist <- titan %>% 
    ggplot(aes(age, fill = survived)) +
    geom_histogram(col = "grey") +
    facet_wrap(. ~ survived) +
    theme(legend.position = "nonw")

age_box <- titan %>%
    select(age, survived) %>% 
    ggplot(aes(x = survived, y = age, fill = survived)) +
    geom_boxplot() +
    theme(legend.position = "none")

age_hist + age_box

# 선실 등급과 성별에 따른 생존자수는 어떤 모습일까?
cat_plot <- ggplot(titan, aes(x = pclass, fill = survived)) +
    geom_bar(position = "dodge") +
    facet_grid(. ~ sex) +
    theme(legend.position = "nonw")

cat_plot2 <- ggplot(titan, aes(x = pclass, fill = survived)) + 
    geom_bar(position = "fill") +
    facet_grid(. ~ sex) +
    theme(legend.position = "nonw")

cat_plot + cat_plot2

# 변수 생성 
titan2 <- titan %>% 
    separate(name, into = c("title", "name"), sep = "\\. ")

titan2 %>% slice(511)

titan2 %>% 
    group_by(title) %>% 
    count(sort = T)

titan2 %>% 
    filter(title %in% c("Miss", "Mrs")) %>% 
    select(title, age) %>% 
    ggplot(aes(x = age, group = title)) +
    geom_histogram(aes(fill = title), col = "grey") +
    facet_grid(title ~ ., scales = "free") +
    geom_vline(xintercept = 28)

# split data
split_data <- initial_split(titan2 %>% select(-name), prop = 0.7, strata = "survived")
train_data <- training(split_data)
test_data  <- testing(split_data) 

# 종속변수의 비율 확인
list(titan2, train_data, test_data) %>% 
    map(~ count(.x, survived)) %>% 
    map(~ mutate(.x, prop = n / sum(n))) %>% 
    set_names(nm = c("titan2", "train_data", "test_data")) %>% 
    bind_rows(.id = "data_name")

library(glmnet)
# recipe
titanic_recipe <- recipe(survived ~ ., data = train_data) %>%
    step_other(title) %>%  # 시간관계상 섬세하게 조정 안함
    step_dummy(title, sex) %>% 
    step_corr(all_numeric(), -all_outcomes()) %>%
    step_YeoJohnson(fare) %>% 
    step_normalize(all_numeric(), -all_outcomes()) %>% 
    prep()

train_data <- bake(titanic_recipe, train_data)
test_data  <- bake(titanic_recipe, test_data)

# 10 fols CV
set.seed(1234)
titanic_folds <- vfold_cv(train_data, v = 5)

# 모델 정의 : model spec
glm_spec <- logistic_reg(
    mode    = "classification", 
    penalty = tune(),       # lambda
    mixture = tune() ) %>%  # alpha
    set_engine("glmnet")

rpart_spec <- decision_tree(
    mode            = "classification",
    cost_complexity = tune(),
    tree_depth      = tune(),
    min_n           = tune()) %>% 
    set_engine("rpart")

rf_spec <- rand_forest(
    mode  = "classification",
    trees = tune(),
    mtry  = tune(),
    min_n = tune()) %>% 
    set_engine("randomForest")

xgb_spec <- boost_tree(
    mode       = "classification", 
    trees      = tune(), 
    min_n      = tune(), 
    tree_depth = tune(), 
    learn_rate = tune() ) %>%
    set_engine("xgboost")

# 하이퍼 퍼래미터 조합 : random search
set.seed(1234)
(glm_grid <- grid_random(
    penalty(),
    mixture() %>% range_set(c(0, 1)), 
    size = 20))

set.seed(1234)
(rpart_grid <- grid_random(
    cost_complexity(),
    tree_depth() %>% range_set(c(1, 30)), 
    min_n() %>% range_set(c(1, 30)), 
    size = 30))

set.seed(1234)
(rf_grid <- grid_random(
    mtry() %>% range_set(c(1, 9)),
    trees() %>% range_set(c(500, 1000)), 
    min_n() %>% range_set(c(2, 30)),
    size = 30))

set.seed(1234)
(xgb_grid <- grid_random(
    trees() %>% range_set(c(500, 1000)),
    min_n() %>% range_set(c(2, 30)), 
    tree_depth() %>% range_set(c(2, 30)),
    learn_rate(),
    size = 40))

# 워크 플로우 (모델과 퍼래미터 결합)
glm_wf <- workflow() %>%
    add_model(glm_spec) %>% 
    add_formula(survived ~ .)

rpart_wf <- workflow() %>%
    add_model(rpart_spec) %>% 
    add_formula(survived ~ .)

rf_wf <- workflow() %>%
    add_model(rf_spec) %>% 
    add_formula(survived ~ .)

xgb_wf <- workflow() %>%
    add_model(xgb_spec) %>% 
    add_formula(survived ~ .)

# hyper-parameter tuning
glm_results <- tune_grid(
    object    = glm_wf,
    resamples = titanic_folds,
    grid      = glm_grid,
    metrics   = metric_set(accuracy, roc_auc),
    control   = control_grid(save_pred = TRUE) )

glm_results %>% collect_metrics() 
glm_results %>% show_best("accuracy")
glm_results %>% show_best("roc_auc")
glm_results %>% select_best("accuracy")
glm_results %>% select_best("roc_auc")

rpart_results <- tune_grid(
    object    = rpart_wf,
    resamples = titanic_folds,
    grid      = rpart_grid,
    metrics   = metric_set(accuracy, roc_auc),
    control   = control_grid(save_pred = TRUE) )

rf_results <- tune_grid(
    object    = rf_wf,
    resamples = titanic_folds,
    grid      = rf_grid,
    metrics   = metric_set(accuracy, roc_auc),
    control   = control_grid(save_pred = TRUE) )

xgb_results <- tune_grid(
    object    = xgb_wf,
    resamples = titanic_folds,
    grid      = xgb_grid,
    metrics   = metric_set(accuracy, roc_auc),
    control   = control_grid(save_pred = TRUE) )

# validation set 결과 비교
models_results <- list(glm_results, rpart_results, rf_results, xgb_results) 

models_valid <- models_results %>% 
    map(~ collect_predictions(.x)) %>% 
    set_names(nm = c("glm_results", "rpart_results", "rf_results", "xgb_results")) %>% 
    bind_rows(.id = "model_results")

models_valid %>% 
    group_by(model_results, id) %>% 
    roc_curve(survived, .pred_1) %>% 
    ggplot(aes(1-specificity, sensitivity)) +
    geom_line(aes(col = id)) +
    facet_wrap(~ model_results)

models_valid %>% 
    group_by(model_results, id) %>%                 
    roc_auc(survived, .pred_1) %>% 
    group_by(model_results) %>% 
    summarise(means_roc = mean(.estimate))

# 최적 퍼래미터 도출
glm_best    <- glm_results %>% select_best("accuracy")
rpart_best  <- rpart_results %>% select_best("accuracy")
rf_best     <- rf_results %>% select_best("accuracy")
xgb_best    <- xgb_results %>% select_best("accuracy")

glm_model   <- glm_spec %>% finalize_model(parameters = glm_best)
rpart_model <- rpart_spec %>% finalize_model(parameters = rpart_best)
rf_model    <- rf_spec %>% finalize_model(parameters = rf_best)
xgb_model   <- xgb_spec %>% finalize_model(parameters = xgb_best)

# 모델 성능 비교 
models <- list(glm_model, rpart_model, rf_model, xgb_model)

models_compared <- models %>% 
    map(~ fit(.x, survived ~ ., data = train_data)) %>% 
    map(~ predict(.x, train_data)) %>% 
    map(~ bind_cols(.x, train_data)) %>% 
    map(~ metrics(.x, truth = survived, estimate = .pred_class)) %>% 
    set_names(nm = c("glmnet", "rpart", "rf", "xgb")) %>% 
    bind_rows(.id = "model_name") %>% 
    filter(.metric == "accuracy")

models_compared

# roc_auc for train_data 
models %>% 
    map(~ fit(.x, survived ~ ., data = train_data)) %>% 
    map(~ predict(.x, train_data, type = "prob")) %>% 
    map(~ bind_cols(.x, train_data)) %>% 
    map(~ roc_auc(.x, survived, .pred_1)) %>% 
    set_names(nm = c("glmnet", "rpart", "rf", "xgb")) %>% 
    bind_rows(.id = "model_name") 

# roc_curve for train_data
models %>% 
    map(~ fit(.x, survived ~ ., data = train_data)) %>% 
    map(~ predict(.x, train_data, type = "prob")) %>% 
    map(~ bind_cols(.x, train_data)) %>% 
    map(~ roc_curve(.x, survived, .pred_1)) %>% 
    set_names(nm = c("glmnet", "rpart", "rf", "xgb")) %>% 
    bind_rows(.id = "model_name") %>% 
    ggplot(aes(1 - specificity, sensitivity)) +
    geom_line(aes(col = model_name))

# roc_auc for test_data 
models %>% 
    map(~ fit(.x, survived ~ ., data = train_data)) %>% 
    map(~ predict(.x, test_data, type = "prob")) %>% 
    map(~ bind_cols(.x, test_data)) %>% 
    map(~ roc_auc(.x, survived, .pred_1)) %>% 
    set_names(nm = c("glmnet", "rpart", "rf", "xgb")) %>% 
    bind_rows(.id = "model_name") 

# roc_curve for test_data
models %>% 
    map(~ fit(.x, survived ~ ., data = train_data)) %>% 
    map(~ predict(.x, test_data, type = "prob")) %>% 
    map(~ bind_cols(.x, test_data)) %>% 
    map(~ roc_curve(.x, survived, .pred_1)) %>% 
    set_names(nm = c("glmnet", "rpart", "rf", "xgb")) %>% 
    bind_rows(.id = "model_name") %>% 
    ggplot(aes(1 - specificity, sensitivity)) +
    geom_line(aes(col = model_name))

# 예측
final_model <- fit(xgb_model, survived ~ ., data = train_data)

predictions <- predict(final_model, test_data) %>% 
    bind_cols(survived = test_data$survived)

head(predictions)

