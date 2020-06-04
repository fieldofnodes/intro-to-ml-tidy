library(tidyverse)
library(tidymodels)
library(tune)
library(here) #not mentioned in slides

fit_data <- function(formula, model, data, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula), model)
  fit(wf, data, ...)
}

fit_split <- function(formula, model, split, ...) {
  wf <- workflows::add_model(workflows::add_formula(workflows::workflow(), formula, blueprint = hardhat::default_formula_blueprint(indicators = FALSE)), model)
  tune::last_fit(wf, split, ...)
}

get_tree_fit <- function(results) {
  results %>% 
    pluck(".workflow", 1) %>% 
    workflows::pull_workflow_fit() 
}

stackoverflow <- read_rds(here::here("materials/data/stackoverflow.rds"))


stackoverflow


set.seed(100) # Important!
so_split <- initial_split(stackoverflow, strata = remote)
so_train <- training(so_split)
so_test  <- testing(so_split)

glimpse(stackoverflow)


so_train %>% 
  group_by(remote) %>% 
  summarise(n = n())


decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_spec <- 
  decision_tree() %>%         
  set_engine("rpart") %>%      
  set_mode("classification") 

set.seed(100) # Important!
tree_fit <- fit_split(remote ~ years_coded_job + salary, 
                      model = tree_spec, 
                      split = so_split) 


tree_fit

tree_fit %>% 
  unnest(.predictions)


tree_fit %>% 
  collect_predictions()

tree_fit %>%   
  collect_predictions() %>% 
  count(.pred_class, truth = remote)

conf_mat(data, truth = remote, estimate = .pred_class)

tree_fit %>%   
  collect_predictions() %>% 
  conf_mat(truth = remote, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

tree_fit %>% 
  collect_metrics()

fit_split(remote ~ years_coded_job + salary, 
          model = tree_spec, 
          split = so_split,
          metrics = metric_set(accuracy, sens, spec)) %>% 
  collect_metrics()


fit_split(remote ~ years_coded_job + salary, 
          model = tree_spec, 
          split = so_split,
          metrics = metric_set(accuracy, roc_auc)) %>% 
  collect_metrics()

fit_split(remote ~ years_coded_job + salary, 
          model = tree_spec, 
          split = so_split) %>% 
  collect_predictions() %>% 
  roc_curve(truth = remote, estimate = .pred_Remote)

tree_fit <-
  fit_split(remote ~ years_coded_job + salary, 
            model = tree_spec, 
            split = so_split) 
tree_fit %>% 
  collect_predictions() %>% 
  roc_curve(truth = remote, estimate = .pred_Remote)


tree_fit %>% 
  collect_predictions() %>% 
  roc_curve(truth = remote, 
            estimate = .pred_Remote) %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity)) +
  geom_line(
    color = "midnightblue",
    size = 1.5
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


tree_fit %>% 
  collect_predictions() %>%  
  roc_curve(truth = remote, 
            estimate = .pred_Remote) %>% 
  autoplot()


tree_fit %>%   
  collect_predictions() %>% 
  roc_curve(truth = remote, estimate = .pred_Remote) %>% 
  autoplot()