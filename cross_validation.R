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


all_wf <- 
  workflow() %>% 
  add_formula(Sale_Price ~ .) %>% 
  add_model(lm_spec)
new_split <- initial_split(ames)
all_wf %>% 
  fit_split(split = new_split,
            metrics = metric_set(rmse)) %>% 
  collect_metrics() %>% 
  pull(.estimate)


rmses <- vector(length = 10, mode = "double")
for (i in 1:10) {
  new_split <- initial_split(ames)
  rmses[i] <-
    all_wf %>% 
    fit_split(split = new_split,
              metrics = metric_set(rmse)) %>% 
    collect_metrics() %>% 
    pull(.estimate)
}

mean(rmses)


set.seed(100)
cv_folds <- 
  vfold_cv(ames_train, v = 10, strata = Sale_Price, breaks = 4)
cv_folds

split1 <- cv_folds %>% 
  pluck("splits", 1)
all_wf %>% 
  fit_split(split = split1,
            metrics = metric_set(rmse)) %>% 
  collect_metrics()


fit_resamples(
  Sale_Price ~ Gr_Liv_Area, 
  model = lm_spec,          
  resamples = cv_folds
)


fit_resamples(
  all_wf,
  resamples = cv_folds
) %>% 
  collect_metrics(summarize = TRUE)


all_wf %>% 
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(rmse)) %>% 
  collect_metrics()


bb_wf <- 
  workflow() %>% 
  add_formula(Sale_Price ~ Bedroom_AbvGr + Full_Bath + Half_Bath) %>% 
  add_model(lm_spec)

sqft_wf <- 
  workflow() %>% 
  add_formula(Sale_Price ~ Gr_Liv_Area) %>% 
  add_model(lm_spec)

bb_wf %>% 
  fit_resamples(resamples = cv_folds) %>% 
  collect_metrics()

sqft_wf %>% 
  fit_resamples(resamples = cv_folds) %>% 
  collect_metrics()



# ames %>% 
#   vfold_cv(v = 10, strata = Sale_Price) %>% 
#   mutate(
#     train_set = map(splits, training),
#     trained_model = map(train_set, 
#                         ~fit(Sale_Price ~ Gr_Liv_Area,
#                              model = lm_spec, data = .x)),
#     test_set = map(splits,testing),
#     rmse = map2_dbl(trained_model, test_set,
#                     ~rmse_vec(predict(.x, new_data = .y)$.pred,
#                               .y$Sale_Price))
#     ) %>% 
#   summarise(mean = mean(rmse), sd = sd(rmse))
#     
#     


bb_wf %>% 
  fit_resamples(resamples = cv_folds, 
                metrics = metric_set(mae)) %>% 
  collect_metrics()


sqft_wf %>% 
  fit_resamples(resamples = cv_folds, 
                metrics = metric_set(mae)) %>% 
  collect_metrics()