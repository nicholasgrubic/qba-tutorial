#################################################################################################################################
# Program: R Code for Probabilistic Bias Analysis (Sensitivity Analysis)
# Article: Probabilistic Quantitative Bias Analysis for Misclassification and Uncontrolled Confounding:
#          A Methodological Tutorial Using Real-World Data
# Authors: Grubic N, Johnston A, Grandi SM
# Version: November 23, 2025
#################################################################################################################################

#Package Information: https://cran.r-project.org/web/packages/episensr/index.html 
#Note: Due to the use of Monte Carlo simulation methods (50,000 replications), results may not exactly align with those reported 
#      in the article (although they will be within a few decimal places).

#install.packages("episensr")
library(episensr)
set.seed(123)

#################################################################################################################################
# EXPOSURE MISCLASSIFICATION (NON-DIFFERENTIAL)
#################################################################################################################################

### Females, 18-39 years ###
# SE: 85.3 (1)
# SP: 98.7 (0.2)
exp_misclass_f_18_39 <- probsens(matrix(c(93, 65, 187, 439), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.853, 0.01)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD) 
                                 spca = list("normal", parms = c(0, 1, 0.987, 0.002))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD) 

### Females, 40-59 years ###
# SE: 87.7 (1)
# SP: 98 (0.3)
exp_misclass_f_40_59 <- probsens(matrix(c(209, 196, 120, 269), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.877, 0.01)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.98, 0.003))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD)

### Females, 60+ years ###
# SE: 80.9 (1.2)
# SP: 98.8 (0.2)
exp_misclass_f_60 <- probsens(matrix(c(333, 521, 98, 273), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                     nrow = 2, byrow = TRUE),
                              type = "exposure",
                              reps = 50000,
                              seca = list("normal", parms = c(0, 1, 0.809, 0.012)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD)
                              spca = list("normal", parms = c(0, 1, 0.988, 0.002))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD)

### Males, 18-39 years ###
# SE: 84.3 (1.1)
# SP: 97.5 (0.3)
exp_misclass_m_18_39 <- probsens(matrix(c(100, 92, 95, 373), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.843, 0.011)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.975, 0.003))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD)

### Males, 40-59 years ###
# SE: 83.8 (1.2)
# SP: 96.4 (0.4)
exp_misclass_m_40_59 <- probsens(matrix(c(185, 190, 65, 212), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.838, 0.012)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.964, 0.004))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD)

### Males, 60+ years ###
# SE: 81.4 (1.4)
# SP: 98.1 (0.2)
exp_misclass_m_60 <- probsens(matrix(c(265, 511, 63, 211), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                     nrow = 2, byrow = TRUE),
                              type = "exposure",
                              reps = 50000,
                              seca = list("normal", parms = c(0, 1, 0.814, 0.014)), #SE of exposure classification (lower bound [0], upper bound [1], mean, SD)
                              spca = list("normal", parms = c(0, 1, 0.981, 0.002))) #SP of exposure classification (lower bound [0], upper bound [1], mean, SD)

#################################################################################################################################
# OUTCOME MISCLASSIFICATION (DIFFERENTIAL)
#################################################################################################################################

### Females, 18-39 years ###
# SE Obesity+: 48.1 (2.3)
# SE Obesity-: 32.4 (3.8)
# SP Obesity+: 91.7 (0.8)
# SP Obesity-: 95.9 (0.4)
out_misclass_f_18_39 <- probsens(matrix(c(40, 35, 258, 451), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.481, 0.023)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 seexp = list("normal", parms = c(0, 1, 0.324, 0.038)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.917, 0.008)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 spexp = list("normal", parms = c(0, 1, 0.959, 0.004)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Females, 40-59 years ###
# SE Obesity+: 69 (1.6)
# SE Obesity-: 50.1 (2.2)
# SP Obesity+: 88.9 (1.5)
# SP Obesity-: 95.2 (0.7)
out_misclass_f_40_59 <- probsens(matrix(c(163, 98, 219, 314), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.69, 0.016)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 seexp = list("normal", parms = c(0, 1, 0.501, 0.022)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.889, 0.015)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 spexp = list("normal", parms = c(0, 1, 0.952, 0.007)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Females, 60+ years ###
# SE Obesity+: 83.3 (1.1)
# SE Obesity-: 71 (1.2)
# SP Obesity+: 87.2 (2.8)
# SP Obesity-: 93.2 (1.3)
out_misclass_f_60 <- probsens(matrix(c(335, 320, 171, 399), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                     nrow = 2, byrow = TRUE),
                              type = "outcome",
                              reps = 50000,
                              seca = list("normal", parms = c(0, 1, 0.833, 0.011)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                              seexp = list("normal", parms = c(0, 1, 0.71, 0.012)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                              spca = list("normal", parms = c(0, 1, 0.872, 0.028)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                              spexp = list("normal", parms = c(0, 1, 0.932, 0.013)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                              corr_se = 0.8, #correlation between SE among those with and without outcome
                              corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 18-39 years ###
# SE Obesity+: 36.4 (2)
# SE Obesity-: 22.7 (1.6)
# SP Obesity+: 89.2 (1.2)
# SP Obesity-: 94.7 (0.5)
out_misclass_m_18_39 <- probsens(matrix(c(40, 30, 175, 415), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("normal", parms = c(0, 1, 0.364, 0.02)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 seexp = list("normal", parms = c(0, 1, 0.227, 0.016)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.892, 0.012)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 spexp = list("normal", parms = c(0, 1, 0.947, 0.005)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 40-59 years ###
# SE Obesity+: 62.7 (1.7)
# SE Obesity-: 48.1 (1.6)
# SP Obesity+: 90.4 (1.6)
# SP Obesity-: 92.7 (0.8)
out_misclass_m_40_59 <- probsens(matrix(c(150, 91, 130, 281), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("normal", parms = c(00, 1, 0.627, 0.017)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 seexp = list("normal", parms = c(0, 1, 0.481, 0.016)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 spca = list("normal", parms = c(0, 1, 0.904, 0.016)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                                 spexp = list("normal", parms = c(0, 1, 0.927, 0.008)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 60+ years ###
# SE Obesity+: 83 (1.3)
# SE Obesity-: 66.5 (1.2)
# SP Obesity+: 85.7 (2.6)
# SP Obesity-: 91.7 (1.1)
out_misclass_m_60 <- probsens(matrix(c(252, 333, 136, 329), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                     nrow = 2, byrow = TRUE),
                              type = "outcome",
                              reps = 50000,
                              seca = list("normal", parms = c(0, 1, 0.83, 0.013)), #SE of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                              seexp = list("normal", parms = c(0, 1, 0.665, 0.012)), #SE of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                              spca = list("normal", parms = c(0, 1, 0.857, 0.026)), #SP of outcome classification among those with exposure (lower bound [0], upper bound [1], mean, SD)
                              spexp = list("normal", parms = c(0, 1, 0.917, 0.011)), #SP of outcome classification among those without exposure (lower bound [0], upper bound [1], mean, SD)
                              corr_se = 0.8, #correlation between SE among those with and without outcome
                              corr_sp = 0.8) #correlation between SE among those with and without outcome


#################################################################################################################################
# UNCONTROLLED CONFOUNDING
#################################################################################################################################

### Females, 18-39 years ###
# P_exposed: 26.3 (1.2)
# P_non-exposed: 19.7 (0.9)
# logOR_poverty-HTN: 0.258 (0.093)
unc_c_f_18_39 <- probsens_conf(matrix(c(104, 54, 194, 432), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("normal", parms = c(0, 1, 0.263, 0.012)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                               prev_nexp = list("normal", parms = c(0, 1, 0.197, 0.009)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                               risk = list("log-normal", parms = c(0.258, 0.093)), #OR of confounder-outcome association (lower bound [0], upper bound [1], mean, SD)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Females, 40-59 years ###
# P_exposed: 13.9 (1)
# P_non-exposed: 10.2 (0.8)
# logOR_poverty-HTN: 0.18 (0.086)
unc_c_f_40_59 <- probsens_conf(matrix(c(242, 163, 140, 249), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("normal", parms = c(0, 1, 0.139, 0.01)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                               prev_nexp = list("normal", parms = c(0, 1, 0.102, 0.008)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                               risk = list("log-normal", parms = c(0.18, 0.086)), #OR of confounder-outcome association (meanlog, SDlog)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Females, 60+ years ###
# P_exposed: 12.9 (1)
# P_non-exposed: 10 (0.6)
# logOR_poverty-HTN: 0.287 (0.133)
unc_c_f_60 <- probsens_conf(matrix(c(397, 457, 109, 262), #contingency table cells (a,b,c,d)
                                   dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                   nrow = 2, byrow = TRUE),
                            reps = 50000,
                            prev_exp = list("normal", parms = c(0, 1, 0.129, 0.01)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                            prev_nexp = list("normal", parms = c(0, 1, 0.1, 0.006)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                            risk = list("log-normal", parms = c(0.287, 0.133)), #OR of confounder-outcome association (meanlog, SDlog)
                            corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 18-39 years ###
# P_exposed: 16.1 (1)
# P_non-exposed: 18.2 (1)
# logOR_poverty-HTN: -0.18 (0.08)
unc_c_m_18_39 <- probsens_conf(matrix(c(109, 83, 106, 362), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("normal", parms = c(0, 1, 0.161, 0.01)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                               prev_nexp = list("normal", parms = c(0, 1, 0.182, 0.01)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                               risk = list("log-normal", parms = c(-0.18, 0.08)), #OR of confounder-outcome association (meanlog, SDlog)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 40-59 years ###
# P_exposed: 9.6 (0.8)
# P_non-exposed: 11.8 (0.7)
# logOR_poverty-HTN: 0.038 (0.083)
unc_c_m_40_59 <- probsens_conf(matrix(c(205, 170, 75, 202), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("normal", parms = c(0, 1, 0.096, 0.008)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                               prev_nexp = list("normal", parms = c(0, 1, 0.118, 0.007)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                               risk = list("log-normal", parms = c(0.038, 0.083)), #OR of confounder-outcome association (meanlog, SDlog)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 60+ years ###
# P_exposed: 7 (0.8)
# P_non-exposed: 8.5 (0.6)
# logOR_poverty-HTN: 0.261 (0.112)
unc_c_m_60 <- probsens_conf(matrix(c(312, 464, 76, 198), #contingency table cells (a,b,c,d)
                                   dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                   nrow = 2, byrow = TRUE),
                            reps = 50000,
                            prev_exp = list("normal", parms = c(0, 1, 0.07, 0.008)), #Prevalence of confounder among exposed (lower bound [0], upper bound [1], mean, SD)
                            prev_nexp = list("normal", parms = c(0, 1, 0.085, 0.006)), #Prevalence of confounder among non-exposed (lower bound [0], upper bound [1], mean, SD)
                            risk = list("log-normal", parms = c(0.261, 0.112)), #OR of confounder-outcome association (meanlog, SDlog)
                            corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

#################################################################################################################################
# SUMMARIZE CONVENTIONAL AND BIAS-ADJUSTED ESTIMIATES
#################################################################################################################################

### EXPOSURE MISCLASSIFICATION (NON-DIFFERENTIAL) ###
exp_misclass_f_18_39_results <- data.frame(Sex = "Female", Age_group = "18-39 years",
                                           OR_conv = exp_misclass_f_18_39$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_f_18_39$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_f_18_39$obs_measures[2,3],
                                           OR_adj = exp_misclass_f_18_39$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_f_18_39$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_f_18_39$adj_measures[4,3])

exp_misclass_f_40_59_results <- data.frame(Sex = "Female", Age_group = "40-59 years",
                                           OR_conv = exp_misclass_f_40_59$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_f_40_59$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_f_40_59$obs_measures[2,3],
                                           OR_adj = exp_misclass_f_40_59$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_f_40_59$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_f_40_59$adj_measures[4,3])

exp_misclass_f_60_results <- data.frame(Sex = "Female", Age_group = "60+ years",
                                        OR_conv = exp_misclass_f_60$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_f_60$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_f_60$obs_measures[2,3],
                                        OR_adj = exp_misclass_f_60$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_f_60$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_f_60$adj_measures[4,3])

exp_misclass_m_18_39_results <- data.frame(Sex = "Male", Age_group = "18-39 years",
                                           OR_conv = exp_misclass_m_18_39$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_m_18_39$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_m_18_39$obs_measures[2,3],
                                           OR_adj = exp_misclass_m_18_39$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_m_18_39$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_m_18_39$adj_measures[4,3])

exp_misclass_m_40_59_results <- data.frame(Sex = "Male", Age_group = "40-59 years",
                                           OR_conv = exp_misclass_m_40_59$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_m_40_59$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_m_40_59$obs_measures[2,3],
                                           OR_adj = exp_misclass_m_40_59$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_m_40_59$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_m_40_59$adj_measures[4,3])

exp_misclass_m_60_results <- data.frame(Sex = "Male", Age_group = "60+ years",
                                        OR_conv = exp_misclass_m_60$obs_measures[2,1], OR_conv_lower_CI = exp_misclass_m_60$obs_measures[2,2], OR_conv_lower_CI = exp_misclass_m_60$obs_measures[2,3],
                                        OR_adj = exp_misclass_m_60$adj_measures[4,1], OR_adj_p2.5 = exp_misclass_m_60$adj_measures[4,2], OR_adj_p97.5 = exp_misclass_m_60$adj_measures[4,3])

exp_misclass_results <- rbind(exp_misclass_f_18_39_results, exp_misclass_f_40_59_results, exp_misclass_f_60_results,
                              exp_misclass_m_18_39_results, exp_misclass_m_40_59_results, exp_misclass_m_60_results)

rm(exp_misclass_f_18_39_results, exp_misclass_f_40_59_results, exp_misclass_f_60_results,
   exp_misclass_m_18_39_results, exp_misclass_m_40_59_results, exp_misclass_m_60_results)

### OUTCOME MISCLASSIFICATION (DIFFERENTIAL) ###
out_misclass_f_18_39_results <- data.frame(Sex = "Female", Age_group = "18-39 years",
                                           OR_conv = out_misclass_f_18_39$obs_measures[2,1], OR_conv_lower_CI = out_misclass_f_18_39$obs_measures[2,2], OR_conv_lower_CI = out_misclass_f_18_39$obs_measures[2,3],
                                           OR_adj = out_misclass_f_18_39$adj_measures[4,1], OR_adj_p2.5 = out_misclass_f_18_39$adj_measures[4,2], OR_adj_p97.5 = out_misclass_f_18_39$adj_measures[4,3])

out_misclass_f_40_59_results <- data.frame(Sex = "Female", Age_group = "40-59 years",
                                           OR_conv = out_misclass_f_40_59$obs_measures[2,1], OR_conv_lower_CI = out_misclass_f_40_59$obs_measures[2,2], OR_conv_lower_CI = out_misclass_f_40_59$obs_measures[2,3],
                                           OR_adj = out_misclass_f_40_59$adj_measures[4,1], OR_adj_p2.5 = out_misclass_f_40_59$adj_measures[4,2], OR_adj_p97.5 = out_misclass_f_40_59$adj_measures[4,3])

out_misclass_f_60_results <- data.frame(Sex = "Female", Age_group = "60+ years",
                                        OR_conv = out_misclass_f_60$obs_measures[2,1], OR_conv_lower_CI = out_misclass_f_60$obs_measures[2,2], OR_conv_lower_CI = out_misclass_f_60$obs_measures[2,3],
                                        OR_adj = out_misclass_f_60$adj_measures[4,1], OR_adj_p2.5 = out_misclass_f_60$adj_measures[4,2], OR_adj_p97.5 = out_misclass_f_60$adj_measures[4,3])

out_misclass_m_18_39_results <- data.frame(Sex = "Male", Age_group = "18-39 years",
                                           OR_conv = out_misclass_m_18_39$obs_measures[2,1], OR_conv_lower_CI = out_misclass_m_18_39$obs_measures[2,2], OR_conv_lower_CI = out_misclass_m_18_39$obs_measures[2,3],
                                           OR_adj = out_misclass_m_18_39$adj_measures[4,1], OR_adj_p2.5 = out_misclass_m_18_39$adj_measures[4,2], OR_adj_p97.5 = out_misclass_m_18_39$adj_measures[4,3])

out_misclass_m_40_59_results <- data.frame(Sex = "Male", Age_group = "40-59 years",
                                           OR_conv = out_misclass_m_40_59$obs_measures[2,1], OR_conv_lower_CI = out_misclass_m_40_59$obs_measures[2,2], OR_conv_lower_CI = out_misclass_m_40_59$obs_measures[2,3],
                                           OR_adj = out_misclass_m_40_59$adj_measures[4,1], OR_adj_p2.5 = out_misclass_m_40_59$adj_measures[4,2], OR_adj_p97.5 = out_misclass_m_40_59$adj_measures[4,3])

out_misclass_m_60_results <- data.frame(Sex = "Male", Age_group = "60+ years",
                                        OR_conv = out_misclass_m_60$obs_measures[2,1], OR_conv_lower_CI = out_misclass_m_60$obs_measures[2,2], OR_conv_lower_CI = out_misclass_m_60$obs_measures[2,3],
                                        OR_adj = out_misclass_m_60$adj_measures[4,1], OR_adj_p2.5 = out_misclass_m_60$adj_measures[4,2], OR_adj_p97.5 = out_misclass_m_60$adj_measures[4,3])

out_misclass_results <- rbind(out_misclass_f_18_39_results, out_misclass_f_40_59_results, out_misclass_f_60_results,
                              out_misclass_m_18_39_results, out_misclass_m_40_59_results, out_misclass_m_60_results)

rm(out_misclass_f_18_39_results, out_misclass_f_40_59_results, out_misclass_f_60_results,
   out_misclass_m_18_39_results, out_misclass_m_40_59_results, out_misclass_m_60_results)

### UNCONTROLLED CONFOUNDING ###
unc_c_f_18_39_results <- data.frame(Sex = "Female", Age_group = "18-39 years",
                                    OR_conv = unc_c_f_18_39$obs_measures[2,1], OR_conv_lower_CI = unc_c_f_18_39$obs_measures[2,2], OR_conv_lower_CI = unc_c_f_18_39$obs_measures[2,3],
                                    OR_adj = unc_c_f_18_39$adj_measures[4,1], OR_adj_p2.5 = unc_c_f_18_39$adj_measures[4,2], OR_adj_p97.5 = unc_c_f_18_39$adj_measures[4,3])

unc_c_f_40_59_results <- data.frame(Sex = "Female", Age_group = "40-59 years",
                                    OR_conv = unc_c_f_40_59$obs_measures[2,1], OR_conv_lower_CI = unc_c_f_40_59$obs_measures[2,2], OR_conv_lower_CI = unc_c_f_40_59$obs_measures[2,3],
                                    OR_adj = unc_c_f_40_59$adj_measures[4,1], OR_adj_p2.5 = unc_c_f_40_59$adj_measures[4,2], OR_adj_p97.5 = unc_c_f_40_59$adj_measures[4,3])

unc_c_f_60_results <- data.frame(Sex = "Female", Age_group = "60+ years",
                                 OR_conv = unc_c_f_60$obs_measures[2,1], OR_conv_lower_CI = unc_c_f_60$obs_measures[2,2], OR_conv_lower_CI = unc_c_f_60$obs_measures[2,3],
                                 OR_adj = unc_c_f_60$adj_measures[4,1], OR_adj_p2.5 = unc_c_f_60$adj_measures[4,2], OR_adj_p97.5 = unc_c_f_60$adj_measures[4,3])

unc_c_m_18_39_results <- data.frame(Sex = "Male", Age_group = "18-39 years",
                                    OR_conv = unc_c_m_18_39$obs_measures[2,1], OR_conv_lower_CI = unc_c_m_18_39$obs_measures[2,2], OR_conv_lower_CI = unc_c_m_18_39$obs_measures[2,3],
                                    OR_adj = unc_c_m_18_39$adj_measures[4,1], OR_adj_p2.5 = unc_c_m_18_39$adj_measures[4,2], OR_adj_p97.5 = unc_c_m_18_39$adj_measures[4,3])

unc_c_m_40_59_results <- data.frame(Sex = "Male", Age_group = "40-59 years",
                                    OR_conv = unc_c_m_40_59$obs_measures[2,1], OR_conv_lower_CI = unc_c_m_40_59$obs_measures[2,2], OR_conv_lower_CI = unc_c_m_40_59$obs_measures[2,3],
                                    OR_adj = unc_c_m_40_59$adj_measures[4,1], OR_adj_p2.5 = unc_c_m_40_59$adj_measures[4,2], OR_adj_p97.5 = unc_c_m_40_59$adj_measures[4,3])

unc_c_m_60_results <- data.frame(Sex = "Male", Age_group = "60+ years",
                                 OR_conv = unc_c_m_60$obs_measures[2,1], OR_conv_lower_CI = unc_c_m_60$obs_measures[2,2], OR_conv_lower_CI = unc_c_m_60$obs_measures[2,3],
                                 OR_adj = unc_c_m_60$adj_measures[4,1], OR_adj_p2.5 = unc_c_m_60$adj_measures[4,2], OR_adj_p97.5 = unc_c_m_60$adj_measures[4,3])

unc_c_results <- rbind(unc_c_f_18_39_results, unc_c_f_40_59_results, unc_c_f_60_results,
                       unc_c_m_18_39_results, unc_c_m_40_59_results, unc_c_m_60_results)

rm(unc_c_f_18_39_results, unc_c_f_40_59_results, unc_c_f_60_results,
   unc_c_m_18_39_results, unc_c_m_40_59_results, unc_c_m_60_results)