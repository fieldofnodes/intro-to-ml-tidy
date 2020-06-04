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

rec <- 
  recipe(Sale_Price ~ ., 
         data = ames) %>% 
  step_center(all_numeric())


rec <- 
  recipe(Sale_Price ~ ., 
         data = ames) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())






rec %>% 
  prep(training = ames_train) %>%
  bake(new_data = ames_test) # or ames_train


rec %>% 
  prep(ames_train) %>%
  bake(ames_test) 


lm(Sale_Price ~ Roof_Style, data = ames)


rec %>% 
  step_dummy(all_nominal())


rec %>% 
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal())


rec %>% 
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())


rec %>%  
  step_pca(all_numeric(),
           num_comp = 5)




pca_rec <- 
  recipe(Sale_Price ~ ., data = ames) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)


pca_rec

pca_wf <-
  workflow() %>% 
  add_recipe(pca_rec) %>% 
  add_model(lm_spec)


pca_wf %>% 
  fit_split(split = ames_split) %>% 
  collect_metrics()


pca_wf %>%
  update_recipe(bc_rec)


bc_rec <- 
  recipe(Sale_Price ~ ., data = ames) %>%
  step_novel(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_BoxCox(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)
bc_wf <- 
  pca_wf %>% 
  update_recipe(bc_rec)


bc_wf %>% 
  fit_split(split = ames_split) %>% 
  collect_metrics()

library(modeldata)
data(stackoverflow)

glimpse(stackoverflow)

set.seed(100) # Important!
so_split <- initial_split(stackoverflow, strata = Remote)
so_train <- training(so_split)
so_test  <- testing(so_split)


tree_spec <- 
  decision_tree() %>%         
  set_engine("rpart") %>%      
  set_mode("classification")


so_rec <- recipe(Remote ~ ., data = so_train) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_lincomb(all_predictors())



so_wf <- workflow() %>% 
  add_model(tree_spec) %>% 
  add_recipe(so_rec)


set.seed(1980)
so_wf %>% 
  fit_split(split = so_split) %>% 
  collect_metrics()

set.seed(1980)
so_wf %>% 
  fit_split(split = so_split,
            metrics = metric_set(accuracy, roc_auc, sens, spec)) %>% 
  collect_metrics()

so_wf %>% 
  fit_split(split = so_split) %>% 
  collect_predictions() %>% 
  conf_mat(truth = Remote, estimate = .pred_class)



so_train %>% 
  count(Remote)

so_test %>% 
  count(Remote)


so_down <- recipe(Remote ~ ., data = so_train) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_lincomb(all_predictors()) %>% 
  step_downsample(all_outcomes())
so_downwf <- so_wf %>% 
  update_recipe(so_down)
set.seed(1980)
so_downwf %>%
  fit_split(split = so_split,
            metrics = metric_set(roc_auc, sens, spec)) %>% 
  collect_metrics()



so_down %>% 
  prep(training = so_train, 
       retain = TRUE) %>% 
  juice()


so_down %>% 
  prep(training = so_train, 
       retain = TRUE) %>% 
  juice() %>%
  count(Remote)
