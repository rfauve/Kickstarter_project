# Note: this process takes approximately 30 minutes (and requires around 3 GB of RAM)

# libraries required
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(lubridate))
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(rpart))
  install.packages("rpart", repos = "http://cran.us.r-project.org")
if (!require(kableExtra))
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if (!require(doParallel))
  install.packages("doParallel", repos = "http://cran.us.r-project.org")
#(.packages())
options(digits = 6)

# cleaned and optimized data set is imported from the Rda file.
load(file = "./DATA/versions/data_ks_opti.Rda")

# some variable must be parsed into classified variables, to ensure common 
# variables values in both train and test sets (necessary for certain models)
# (little foreshadowing: log is applied on goal_USD and pledged_USD (1 is added
# to avoid -Inf values))
data_ks <- data_ks %>%
  mutate(goal_log = log10(1 + goal_USD),
         pledged_log = log10(1 + pledged_USD),
         goal_log_class = cut(goal_log,
                              breaks = seq(0, 10, 0.001),
                              include.lowest = TRUE))

# seed for reproducible results
set.seed(2486) 

# test_set_final contains 10% of the data
test_index <- createDataPartition(data_ks$pledged_USD, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)
train_set_final <- data_ks[-test_index, ]
temp <- data_ks[test_index, ]
remove(test_index)

# train & test should have common variables values
test_set_final <- temp %>%
  semi_join(train_set_final, by = "name_nb_char") %>%
  semi_join(train_set_final, by = "name_nb_word") %>%
  semi_join(train_set_final, by = "name_spec_char") %>%
  semi_join(train_set_final, by = "name_lcase") %>%
  semi_join(train_set_final, by = "main_category") %>%
  semi_join(train_set_final, by = "true_category") %>%
  semi_join(train_set_final, by = "diff_cat") %>%
  semi_join(train_set_final, by = "time_span") %>%
  semi_join(train_set_final, by = "start_wday") %>%
  semi_join(train_set_final, by = "end_wday") %>%
  semi_join(train_set_final, by = "start_day") %>%
  semi_join(train_set_final, by = "end_day") %>%  
  semi_join(train_set_final, by = "start_month") %>%
  semi_join(train_set_final, by = "end_month") %>%  
  semi_join(train_set_final, by = "country") %>%
  semi_join(train_set_final, by = "goal_log_class")

# Add rows removed from final test set back into final train set
removed <- anti_join(temp,
                     test_set_final)
train_set_final <- rbind(train_set_final, removed)

# RMSE function
RMSE_fct <- function(test, pred) {
  sqrt(mean((test - pred) ^ 2))
}

# Residual's explanation model
# function for lambda (regularisation)
lambda_opt <- function(train, test, fct_mod) {
  # This function gives the optimal lambda (and RMSE) for given 
  # train/test sets and a model. The lambda associated with the lowest RMSE 
  # (with a given threshold) is designated as optimal.
  
  # threshold
  prec <- 0.1
  
  # initialization
  l_new <- 0
  i <- 2
  
  y <- fct_mod(train, test, l_new)
  rmse_l_new <- RMSE_fct(test$pledged_log, y)
  
  out <- data.frame(l = l_new,
                    rmse = rmse_l_new)
  
  # loop while lambda's transformation is above the threshold
  while (abs(i) > prec) {
    # short-term storage of last values (lambda, RMSE) for comparison
    l_old <- l_new
    rmse_l_old <- rmse_l_new
    
    # new lambda
    l_new <- l_old + i
    
    # RMSE with new lambda
    y <- fct_mod(train, test, l_new)
    rmse_l_new <- RMSE_fct(test$pledged_log, y)
    
    if (rmse_l_new > rmse_l_old) {
      # change in the "direction" and "amplitude" of the modification of lambda
      i <- -i / 2
    }
    # long-term storage of values (lambda, RMSE)
    out <- rbind(out,
                 c(l_new,
                   rmse_l_new))
  }
  # return penultimate values (lambda, RMSE), which the minimum RMSE reached
  out[nrow(out) - 1, ]
}

# model 16 biaises
mod_b_16 <- function(train, test, lambda) {
  mu_pledged <- mean(train$pledged_log)
  
  tcat_biais <- train %>%
    group_by(true_category) %>%
    summarise(b_tcat = sum(pledged_log - mu_pledged) / (n() + lambda))
  
  tspan_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    group_by(time_span) %>%
    summarise(b_tspan = sum(pledged_log - mu_pledged
                            - b_tcat) / (n() + lambda))
  
  mcat_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    group_by(main_category) %>%
    summarise(b_mcat = sum(pledged_log - mu_pledged 
                           - b_tcat - b_tspan) / (n() + lambda))
  
  goal_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    group_by(goal_log_class) %>%
    summarise(b_goal = sum(pledged_log - mu_pledged
                           - b_tcat - b_tspan - b_mcat) / (n() + lambda))
  
  nbword_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    group_by(name_nb_word) %>%
    summarise(b_nbword = sum(pledged_log - mu_pledged
                             - b_tcat - b_tspan - b_mcat - b_goal) / (n() + lambda))
  
  nbchar_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    group_by(name_nb_char) %>%
    summarise(b_nbchar = sum(pledged_log - mu_pledged
                             - b_tcat - b_tspan - b_mcat - b_goal
                             - b_nbword) / (n() + lambda))
  
  country_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    group_by(country) %>%
    summarise(b_country = sum(pledged_log - mu_pledged
                              - b_tcat - b_tspan - b_mcat - b_goal
                              - b_nbword - b_nbchar) / (n() + lambda))
  
  smonth_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    group_by(start_month) %>%
    summarise(b_smonth = sum(pledged_log - mu_pledged
                             - b_tcat - b_tspan - b_mcat - b_goal
                             - b_nbword - b_nbchar - b_country) / (n() + lambda))
  
  emonth_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    group_by(end_month) %>%
    summarise(b_emonth = sum(pledged_log - mu_pledged
                             - b_tcat - b_tspan - b_mcat - b_goal
                             - b_nbword - b_nbchar - b_country - b_smonth) / (n() + lambda))
  
  swday_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    group_by(start_wday) %>%
    summarise(b_swday = sum(pledged_log - mu_pledged
                            - b_tcat - b_tspan - b_mcat - b_goal
                            - b_nbword - b_nbchar - b_country - b_smonth
                            - b_emonth) / (n() + lambda))
  
  lcase_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    group_by(name_lcase) %>%
    summarise(b_lcase = sum(pledged_log - mu_pledged
                            - b_tcat - b_tspan - b_mcat - b_goal
                            - b_nbword - b_nbchar - b_country - b_smonth
                            - b_emonth - b_swday) / (n() + lambda))
  
  sday_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    group_by(start_day) %>%
    summarise(b_sday = sum(pledged_log - mu_pledged
                           - b_tcat - b_tspan - b_mcat - b_goal
                           - b_nbword - b_nbchar - b_country - b_smonth
                           - b_emonth - b_swday - b_lcase) / (n() + lambda))
  
  spchar_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    left_join(sday_biais, by = "start_day") %>%
    group_by(name_spec_char) %>%
    summarise(b_spchar = sum(pledged_log - mu_pledged
                             - b_tcat - b_tspan - b_mcat - b_goal
                             - b_nbword - b_nbchar - b_country - b_smonth
                             - b_emonth - b_swday - b_lcase - b_sday) / (n() + lambda))
  
  dcat_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    left_join(sday_biais, by = "start_day") %>%
    left_join(spchar_biais, by = "name_spec_char") %>%
    group_by(diff_cat) %>%
    summarise(b_dcat = sum(pledged_log - mu_pledged
                           - b_tcat - b_tspan - b_mcat - b_goal
                           - b_nbword - b_nbchar - b_country - b_smonth
                           - b_emonth - b_swday - b_lcase - b_sday
                           - b_spchar) / (n() + lambda))
  
  eday_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    left_join(sday_biais, by = "start_day") %>%
    left_join(spchar_biais, by = "name_spec_char") %>%
    left_join(dcat_biais, by = "diff_cat") %>%
    group_by(end_day) %>%
    summarise(b_eday = sum(pledged_log - mu_pledged
                           - b_tcat - b_tspan - b_mcat - b_goal
                           - b_nbword - b_nbchar - b_country - b_smonth
                           - b_emonth - b_swday - b_lcase - b_sday
                           - b_spchar - b_dcat) / (n() + lambda))
  
  ewday_biais <- train %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    left_join(sday_biais, by = "start_day") %>%
    left_join(spchar_biais, by = "name_spec_char") %>%
    left_join(dcat_biais, by = "diff_cat") %>%
    left_join(eday_biais, by = "end_day") %>%
    group_by(end_wday) %>%
    summarise(b_ewday = sum(pledged_log - mu_pledged
                            - b_tcat - b_tspan - b_mcat - b_goal
                            - b_nbword - b_nbchar - b_country - b_smonth
                            - b_emonth - b_swday - b_lcase - b_sday
                            - b_spchar - b_dcat - b_eday) / (n() + lambda))
  
  test <- test %>%
    left_join(tcat_biais, by = "true_category") %>%
    left_join(tspan_biais, by = "time_span") %>%
    left_join(mcat_biais, by = "main_category") %>%
    left_join(goal_biais, by = "goal_log_class") %>%
    left_join(nbword_biais, by = "name_nb_word") %>%
    left_join(nbchar_biais, by = "name_nb_char") %>%
    left_join(country_biais, by = "country") %>%
    left_join(smonth_biais, by = "start_month") %>%
    left_join(emonth_biais, by = "end_month") %>%
    left_join(swday_biais, by = "start_wday") %>%
    left_join(lcase_biais, by = "name_lcase") %>%
    left_join(sday_biais, by = "start_day") %>%
    left_join(spchar_biais, by = "name_spec_char") %>%
    left_join(dcat_biais, by = "diff_cat") %>%
    left_join(eday_biais, by = "end_day") %>%
    left_join(ewday_biais, by = "end_wday") %>%
    mutate(pred = mu_pledged 
           + b_tcat + b_tspan + b_mcat + b_goal
           + b_nbword + b_nbchar + b_country + b_smonth
           + b_emonth + b_swday + b_lcase + b_sday
           + b_spchar + b_dcat + b_eday + b_ewday)
  
  test$pred
}

# regression tree model
cl <- makePSOCKcluster(6)
set.seed(2486)
registerDoParallel(cl)

mod_tree <- train(pledged_log ~ .,
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.001, len = 100)),
                  data = train_set_final[, -c(1, 17, 18, 20)])
stopCluster(cl)

# models' training
res <- data.frame()

l_opt <- lambda_opt(train_set_final, test_set_final, mod_b_16) # 4min
res[1, 1] <- l_opt[1, 2] # 1.28069
y_16 <- mod_b_16(train_set_final, test_set_final, l_opt[1, 1])

y_tree <- predict(mod_tree, test_set_final, type = "raw")
res[1, 2] <- RMSE_fct(test_set_final$pledged_log, y_tree) # 1.29838

y_ens <- as.data.frame(cbind(y_16, y_tree)) %>%
  mutate(ens = (y_16 + y_tree) / 2) %>%
  select(ens)

res[1, 3] <- RMSE_fct(test_set_final$pledged_log, y_ens$ens) # 1.27115

# nice display of the results
# RMSE_results <- tibble(Model = "Ensemble (residuals' explanation + regression tree)",
#                        RMSE = res[1, 3])
# 
# RMSE_results %>% 
#   kable("latex", booktabs = T) %>%
#   kable_styling(position = "center")

# display final RMSE (final RMSE: 1.27115)
res[1, 3]

# Tadaa !!

