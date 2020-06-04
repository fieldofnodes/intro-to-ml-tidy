library(tidyverse)
library(AmesHousing)
library(tidymodels)
library(tune)
library(workflows)


ames <- make_ames() %>% 
  dplyr::select(-matches("Qu"))
set.seed(100)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
fit_data <- function(object, model, data, ...) {
  if (inherits(object, "formula")) {
    object <- add_model(add_formula(workflow(), object, blueprint = hardhat::default_formula_blueprint(indicators = FALSE, ...)))
  }
  fit(object, data, ...)
}


fit_split <- function(object, model, split, ...) {
  if (inherits(object, "formula")) {
    object <- add_model(add_formula(workflow(), object, blueprint = hardhat::default_formula_blueprint(indicators = FALSE)), model)
  }
  tune::last_fit(object, split, ...)
}




normalize_rec <-
  recipe(Sale_Price ~ ., data = ames) %>% 
  step_novel(all_nominal()) %>% 
  step_dummy(all_nominal()) %>% 
  step_zv(all_predictors()) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())


knn5_spec <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")


knn5_wf <- 
  workflow() %>% 
  add_recipe(normalize_rec) %>% 
  add_model(knn5_spec)

knn5_wf %>% 
  fit_resamples(resamples = cv_folds) %>% 
  collect_metrics()




knn10_spec <- 
  nearest_neighbor(neighbors = 10) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")


knn10_wf <- 
  workflow() %>% 
  add_recipe(normalize_rec) %>% 
  add_model(knn10_spec)

knn10_wf %>% 
  fit_resamples(resamples = cv_folds) %>% 
  collect_metrics()



knn20_spec <- 
  nearest_neighbor(neighbors = 20) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")


knn20_wf <- 
  workflow() %>% 
  add_recipe(normalize_rec) %>% 
  add_model(knn20_spec)

knn20_wf %>% 
  fit_resamples(resamples = cv_folds) %>% 
  collect_metrics()



expand_grid(neighbors = c(1:4), foo = 1:10)


k10_20 <- 
  expand_grid(neighbors = 10:20)

knn_tuner <- 
  nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_twf <- 
  workflow() %>% 
  add_recipe(normalize_rec) %>% 
  add_model(knn_tuner)

knn_results <- 
  knn_twf %>% 
  tune_grid(resamples = cv_folds, grid = k10_20)

knn_results %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse")


knn_results %>% 
  show_best(metric = "rmse", n = 5, maximize = FALSE)


knn_results %>% autoplot()

lm_spec <- linear_reg() %>% set_engine("lm")


pca_tuner <- recipe(Sale_Price ~ ., data = ames) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())


pca_twf <- workflow() %>% 
  add_recipe(pca_tuner) %>% 
  add_model(lm_spec)


nc10_40 <- expand_grid(num_comp = c(10,20,30,40))


pca_results <- pca_twf %>% 
  tune_grid(resamples = cv_folds, grid = nc10_40)

pca_results %>% show_best(metric = "rmse")


library(modeldata)
data(stackoverflow)
# split the data
set.seed(100) # Important!
so_split <- initial_split(stackoverflow, strata = Remote)
so_train <- training(so_split)
so_test  <- testing(so_split)
set.seed(100) # Important!
so_folds <- vfold_cv(so_train, v = 10, strata = Remote)


so_rec <- recipe(Remote ~ ., 
                 data = so_train) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_downsample(Remote)



rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


rf_wf <-
  workflow() %>% 
  add_recipe(so_rec) %>% 
  add_model(rf_spec)




rf_results <-
  rf_wf %>% 
  fit_resamples(resamples = so_folds,
                metrics = metric_set(roc_auc))




rf_results %>% 
  collect_metrics(summarize = TRUE)



rf_tuner <- 
  rand_forest(mtry = tune(),
              min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")
rf_twf <-
  rf_wf %>% 
  update_model(rf_tuner)
rf_results <-
  rf_twf %>% 
  tune_grid(resamples = so_folds)

rf_results %>% 
  show_best(metric = "roc_auc")


so_best <-
  rf_results %>% 
  select_best(metric = "roc_auc")
so_best


so_wfl_final <- 
  rf_twf %>%
  finalize_workflow(so_best)

so_test_results <-
  so_wfl_final %>% 
  fit_split(split = so_split)



so_test_results


so_best <-
  rf_results %>% 
  select_best(metric = "roc_auc")
so_wfl_final <- 
  rf_twf %>%
  finalize_workflow(so_best)
so_test_results <-
  so_wfl_final %>% 
  fit_split(split = so_split)
so_test_results %>% 
  collect_metrics()

so_test_results %>% 
  collect_predictions()


roc_values <- 
  so_test_results %>% 
  collect_predictions() %>% 
  roc_curve(truth = Remote, estimate = .pred_Remote)
autoplot(roc_values)



