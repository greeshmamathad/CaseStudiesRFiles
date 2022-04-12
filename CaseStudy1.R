library(tidyverse)  
library(tidymodels) 
library(naniar)   
library(vip) 
library(rsconnect)
library(purrr)
library(usemodels) 
library(glmnet)  
library(stacks)
library(themis) 
library(modelr)

loans <- read_csv(file = 'loans_full_schema.csv')

## Mean interest rate for each state
loans %>% 
  group_by(state) %>% 
  mutate(mean_ir = mean(interest_rate)) %>% 
  ggplot(aes(x= state, y= mean_ir)) +
  geom_point()+
  geom_text(aes(label=state), vjust= 1.5, size= 2)+
  labs(y= "Mean Interest Rate")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


## application type vs the mean Interest rate

loans %>% 
  group_by(application_type) %>% 
  mutate(mean_ir = mean(interest_rate)) %>% 
  ggplot(aes(x= application_type, y= mean_ir)) +
  geom_point()+
  geom_text(aes(label=application_type), vjust= 1.5, size= 2)+
  labs(y= "Mean Interest Rate")+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


loans %>% 
  group_by(emp_length) %>%
  mutate(mean_ir = mean(interest_rate)) %>% 
  ggplot(aes(x=emp_length, y=mean_ir))+
  geom_point()+
  geom_line()+
  labs(x="Length in Job", y= "Mean Interest Rate")+
  scale_x_continuous(breaks = (0:10))


loans %>% 
  group_by(homeownership) %>% 
  ggplot(aes(x=homeownership, y=interest_rate))+
  geom_boxplot()+
  labs(y="Interest Rate")


sub_grade_lm <- lm(interest_rate ~ sub_grade, loans)
loans %>% 
  #group_by(grade) %>%
  ggplot(aes(x=sub_grade, y=interest_rate)) +
  geom_point()+
  geom_abline(slope = coef(sub_grade_lm)[[2]], intercept = coef(sub_grade_lm)[[1]])+
  labs(x="Sub Grade", y="Interest Rate")


loans %>% 
  ggplot(aes(y=interest_rate, x=account_never_delinq_percent ))+
  geom_point()+
  labs(y="Interest Rate", x="Percent of non-delinquency")


loans2 <- loans %>% 
  filter(application_type == "individual") %>% 
  select(-application_type, -annual_income_joint, -verification_income_joint, -debt_to_income_joint)  


loans2

##########################################

loans2 %>% 
  add_n_miss() %>% 
  count(n_miss_all)
####################################################

set.seed(327) #for reproducibility

# Randomly assigns 75% of the data to training.
loans_split <- initial_split(loans2, 
                             prop = .75)
loans_split
###################################################

#<training/testing/total>

loans_training <- training(loans_split)
loans_testing <- testing(loans_split)

#############################################################


loans_recipe <- recipe(interest_rate ~ ., 
                       data = loans_training) %>% 
  # Pre-processing:
  step_rm(emp_title, state, num_accounts_120d_past_due) %>% 
  step_mutate_at(all_numeric(), fn= ~replace(., is.na(.), 0)) %>%
  #step_mutate_at(all_nominal(), fn= ~replace(., is.na(.), "Null")) %>%
  # Make these evaluative variables, not included in modeling
  update_role(all_of(c("emp_title",
                       "state",
                       "issue_month")),
              new_role = "evaluative") %>% 
  # Create indicator variables for factors/character/nominal
  # explicitly remove outcome, even though outcome isn't nominal
  # this is important in cases when we have a nominal output (eg. logistic)
  step_dummy(all_nominal(), 
             -all_outcomes(), 
             -has_role(match = "evaluative")) %>% 
  step_normalize(all_predictors(), 
                 -all_nominal())


################################################################


loans_recipe %>% 
  prep(loans_training) %>%
  juice() 



set.seed(1211) # for reproducibility
loans_cv <- vfold_cv(loans_training, v = 5)


loans_lasso_mod <- 
  # Define a lasso model 
  linear_reg(mixture = 1) %>% 
  # Set the engine to "glmnet" 
  set_engine("glmnet") %>% 
  # The parameters we will tune.
  set_args(penalty = tune()) %>% 
  # Use "regression"
  set_mode("regression")


loans_lasso_wf <- 
  # Set up the workflow
  workflow() %>% 
  # Add the recipe
  add_recipe(loans_recipe) %>% 
  # Add the modeling
  add_model(loans_lasso_mod)

loans_lasso_wf

penalty_grid <- grid_regular(penalty(),
                             levels = 20)
penalty_grid 

loans_lasso_tune <- 
  loans_lasso_wf %>% 
  tune_grid(
    resamples = loans_cv,
    grid = penalty_grid
  )


loans_lasso_tune

# The rmse for each fold:
loans_lasso_tune %>% 
  select(id, .metrics) %>% 
  unnest(.metrics) %>% 
  filter(.metric == "rmse")



# rmse averaged over all folds:
loans_lasso_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") 




# Visualize rmse vs. penalty
loans_lasso_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",scales::math_format(10^.x))) +
  labs(x = "penalty", y = "rmse")


loans_lasso_tune %>% 
  show_best(metric = "rmse")



# Best tuning parameter by smallest rmse
best_param <- loans_lasso_tune %>% 
  select_best(metric = "rmse")
best_param

# Best tuning parameter by smallest rmse
one_se_param <- loans_lasso_tune %>% 
  select_by_one_std_err(metric = "rmse", desc(penalty))
one_se_param


loans_lasso_final_wf <- loans_lasso_wf %>% 
  finalize_workflow(one_se_param)
loans_lasso_final_wf


loans_lasso_final_mod <- loans_lasso_final_wf %>% 
  fit(data = loans_training)

loans_lasso_final_mod %>% 
  pull_workflow_fit() %>% 
  tidy() 




# Visualize variable importance
loans_lasso_final_mod %>% 
  pull_workflow_fit() %>% 
  vip()




# Fit model with best tuning parameter(s) to training data and apply to test data
loans_lasso_test <- loans_lasso_final_wf %>% 
  last_fit(loans_split)

# Metrics for model applied to test data
loans_lasso_test %>% 
  collect_metrics()



collect_predictions(loans_lasso_test) %>% 
  ggplot(aes(x = interest_rate, 
             y = .pred)) +
  geom_point(alpha = .5, 
             size = .5) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, 
              intercept = 0, 
              color = "darkred") +
  labs(x = "Actual Interest Rate", 
       y = "Predicted Interest Rate")





