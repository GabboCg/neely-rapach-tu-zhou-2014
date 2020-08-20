library("tidyverse")
library("magrittr")
library("broom")
library("sandwich")
library("lmtest")

returns_econ_tech_data <- readxl::read_xls("returns_econ_tech_data.xls")

predictor_econ_tech <- returns_econ_tech_data %>% 
  mutate_all(list(~ as.numeric(.))) %>% 
  filter(date >= 192612) %>% 
  mutate(log_eqp = log(1 + crsp_spvw) - log(1 + lag(rfree, 1)),
         eqp = crsp_spvw - lag(rfree, 1),
         dp = log(d12) - log(index),
         dy = log(d12) - log(lag(index, 1)),
         ep = log(e12) - log(index),
         de = log(d12) - log(e12),
         rvol = sqrt(pi / 2) * sqrt(12) * (1 / 12) * zoo::rollsum(abs(eqp), 12, align = "right", fill = NA), 
         tbl = 100 * tbl,
         lty = 100 * lty,
         ltr = 100 * ltr,
         tms = lty - tbl,
         dfy = 100 * (baa - aaa),
         dfr = 100 * corpr - ltr,
         lag_infl = 100 * lag(infl, 1)) %>% 
  select(date, log_eqp, dp, dy, ep, de, rvol, bm, ntis, 
         tbl, lty, ltr, tms, dfy, dfr, lag_infl) %>%
  filter(date >= 195012)

prcomp_econ <- prcomp(scale(predictor_econ_tech[,3:n_col], center = TRUE, scale = TRUE)) 

predictor_econ_pc <- bind_cols(log_eqp = predictor_econ_tech$log_eqp, prcomp_econ$x)

# summary statistics - table 1 --------------------------------------------

# log equity premium
summary_stat_eqp <- predictor_econ_tech %>% 
  select(log_eqp) %>% 
  mutate(log_eqp = log_eqp * 100) %>% 
  map(function(x) bind_cols(mean = mean(x), 
                            std = sd(x), 
                            min = min(x), 
                            max = max(x),
                            ac = acf(x, plot = FALSE)[[1]][2],
                            sp = mean(x) / sd(x))) %>% 
  bind_rows(.id = "variable") 

# econ predictors
summary_stat_econ <- predictor_econ_tech %>% 
  select(-c(date, log_eqp)) %>% 
  map(function(x) bind_cols(mean = mean(x), 
                            std = sd(x), 
                            min = min(x), 
                            max = max(x),
                            ac = acf(x, plot = FALSE)[[1]][2])) %>% 
  bind_rows(.id = "variable") 

# log equity premium + econ predictors
summary_stat_eqp_econ <- bind_rows(summary_stat_eqp, summary_stat_econ)

# predictive regression estimation - table 2 ------------------------------

predictor_econ_tech %<>%
  mutate_at(vars(dp:lag_infl), list(~ lag(., 1))) %>% 
  na.omit()
  
n_col = ncol(predictor_econ_tech)
n_row = nrow(predictor_econ_tech)

in_sample_res <- matrix(0, ncol = 3, nrow = (n_col - 2))
colnames(in_sample_res) <- c("slope", "t-stat", "r2")
rownames(in_sample_res) <- colnames(predictor_econ_tech[,3:n_col])

for (i in 1:(n_col - 2)) {
  
  lm_econ_res <- lm(log_eqp * 100 ~ ., data = predictor_econ_tech[,c(2, i + 2)])
  nw_econ_res <- coeftest(lm_econ_res, vcov.= NeweyWest(lm_econ_res, lag = 0, adjust = FALSE, verbose = FALSE, prewhite = FALSE))
  
  in_sample_res[i,1] <- tidy(nw_econ_res)[2,2] %>% pull(1)
  in_sample_res[i,2] <- tidy(nw_econ_res)[2,4] %>% pull(1)
  in_sample_res[i,3] <- (glance(lm_econ_res)[,1] %>% pull(1)) * 100
  
}

round(in_sample_res, 2)

predictor_econ_pc %<>%
  mutate_at(vars(PC1:PC12), list(~ lag(., 1))) %>% 
  na.omit()

lm_pc_res <- lm(log_eqp * 100 ~ ., data = predictor_econ_pc[,c(1,2)])
nw_pc_res <- coeftest(lm_pc_res, vcov.= NeweyWest(lm_pc_res, lag = 0, adjust = FALSE, verbose = FALSE, prewhite = FALSE))
(glance(lm_pc_res)[,1] %>% pull(1)) * 100
