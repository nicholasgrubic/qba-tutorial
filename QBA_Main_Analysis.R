#################################################################################################################################
# Program: R Code for Probabilistic Bias Analysis
# Article: Probabilistic Quantitative Bias Analysis for Misclassification and Uncontrolled Confounding:
#          A Methodological Tutorial Using Real-World Data
# Authors: Grubic N, Johnston A, Grandi SM
# Version: September 2, 2025
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
# SE: 85.3 (83.3-87.3)
# SP: 98.7 (98.4-99.1)
exp_misclass_f_18_39 <- probsens(matrix(c(93, 65, 187, 439), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.833, 0.873, 0.853)), #SE of exposure classification (lower limit, upper limit, mode) 
                                 spca = list("triangular", parms = c(0.984, 0.991, 0.987))) #SP of exposure classification (lower limit, upper limit, mode)

### Females, 40-59 years ###
# SE: 87.7 (85.7-89.6)
# SP: 98 (97.5-98.6)
exp_misclass_f_40_59 <- probsens(matrix(c(209, 196, 120, 269), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.857, 0.896, 0.877)), #SE of exposure classification (lower limit, upper limit, mode) 
                                 spca = list("triangular", parms = c(0.975, 0.986, 0.98))) #SP of exposure classification (lower limit, upper limit, mode)

### Females, 60+ years ###
# SE: 80.9 (78.6-83.2)
# SP: 98.8 (98.5-99.2)
exp_misclass_f_60 <- probsens(matrix(c(333, 521, 98, 273), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                     nrow = 2, byrow = TRUE),
                              type = "exposure",
                              reps = 50000,
                              seca = list("triangular", parms = c(0.786, 0.832, 0.809)), #SE of exposure classification (lower limit, upper limit, mode) 
                              spca = list("triangular", parms = c(0.985, 0.992, 0.988))) #SP of exposure classification (lower limit, upper limit, mode)

### Males, 18-39 years ###
# SE: 84.3 (82.1-86.4)
# SP: 97.5 (97-98)
exp_misclass_m_18_39 <- probsens(matrix(c(100, 92, 95, 373), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.821, 0.864, 0.843)), #SE of exposure classification (lower limit, upper limit, mode) 
                                 spca = list("triangular", parms = c(0.97, 0.98, 0.975))) #SP of exposure classification (lower limit, upper limit, mode)

### Males, 40-59 years ###
# SE: 83.8 (81.5-86)
# SP: 96.4 (95.6-97.2)
exp_misclass_m_40_59 <- probsens(matrix(c(185, 190, 65, 212), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "exposure",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.815, 0.86, 0.838)), #SE of exposure classification (lower limit, upper limit, mode) 
                                 spca = list("triangular", parms = c(0.956, 0.972, 0.964))) #SP of exposure classification (lower limit, upper limit, mode)

### Males, 60+ years ###
# SE: 81.4 (78.6-84.1)
# SP: 98.1 (97.6-98.6)
exp_misclass_m_60 <- probsens(matrix(c(265, 511, 63, 211), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_sr+", "Obesity_sr-")),
                                     nrow = 2, byrow = TRUE),
                              type = "exposure",
                              reps = 50000,
                              seca = list("triangular", parms = c(0.786, 0.841, 0.814)), #SE of exposure classification (lower limit, upper limit, mode) 
                              spca = list("triangular", parms = c(0.976, 0.986, 0.981))) #SP of exposure classification (lower limit, upper limit, mode)

#################################################################################################################################
# OUTCOME MISCLASSIFICATION (DIFFERENTIAL)
#################################################################################################################################

### Females, 18-39 years ###
# SE Obesity+: 48.1 (43.6-52.7)
# SE Obesity-: 32.4 (25.1-39.8)
# SP Obesity+: 91.7 (90.2-93.2)
# SP Obesity-: 95.9 (95.1-96.8)
out_misclass_f_18_39 <- probsens(matrix(c(40, 35, 258, 451), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.436, 0.527, 0.481)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 seexp = list("triangular", parms = c(0.251, 0.398, 0.324)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 spca = list("triangular", parms = c(0.902, 0.932, 0.917)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 spexp = list("triangular", parms = c(0.951, 0.968, 0.959)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Females, 40-59 years ###
# SE Obesity+: 69 (65.8-72.2)
# SE Obesity-: 50.1 (45.7-54.5)
# SP Obesity+: 88.9 (86-91.8)
# SP Obesity-: 95.2 (93.8-96.6)
out_misclass_f_40_59 <- probsens(matrix(c(163, 98, 219, 314), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.658, 0.722, 0.69)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 seexp = list("triangular", parms = c(0.457, 0.545, 0.501)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 spca = list("triangular", parms = c(0.86, 0.918, 0.889)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 spexp = list("triangular", parms = c(0.938, 0.966, 0.952)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Females, 60+ years ###
# SE Obesity+: 83.3 (81.2-85.4)
# SE Obesity-: 71 (68.5-73.4)
# SP Obesity+: 87.2 (81.7-92.7)
# SP Obesity-: 93.2 (90.7-95.6)
out_misclass_f_60 <- probsens(matrix(c(335, 320, 171, 399), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                     nrow = 2, byrow = TRUE),
                              type = "outcome",
                              reps = 50000,
                              seca = list("triangular", parms = c(0.812, 0.854, 0.833)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                              seexp = list("triangular", parms = c(0.685, 0.734, 0.71)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                              spca = list("triangular", parms = c(0.817, 0.927, 0.872)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                              spexp = list("triangular", parms = c(0.907, 0.956, 0.932)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                              corr_se = 0.8, #correlation between SE among those with and without outcome
                              corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 18-39 years ###
# SE Obesity+: 36.4 (32.4-40.3)
# SE Obesity-: 22.7 (19.6-25.7)
# SP Obesity+: 89.2 (87-91.5)
# SP Obesity-: 94.7 (93.7-95.7)
out_misclass_m_18_39 <- probsens(matrix(c(40, 30, 175, 415), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.324, 0.403, 0.364)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 seexp = list("triangular", parms = c(0.196, 0.257, 0.227)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 spca = list("triangular", parms = c(0.87, 0.915, 0.892)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 spexp = list("triangular", parms = c(0.937, 0.957, 0.947)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 40-59 years ###
# SE Obesity+: 62.7 (59.3-66.1)
# SE Obesity-: 48.1 (44.9-51.3)
# SP Obesity+: 90.4 (87.2-93.5)
# SP Obesity-: 92.7 (91.2-94.2)
out_misclass_m_40_59 <- probsens(matrix(c(150, 91, 130, 281), #contingency table cells (a,b,c,d)
                                        dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                        nrow = 2, byrow = TRUE),
                                 type = "outcome",
                                 reps = 50000,
                                 seca = list("triangular", parms = c(0.593, 0.661, 0.627)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 seexp = list("triangular", parms = c(0.449, 0.513, 0.481)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 spca = list("triangular", parms = c(0.872, 0.935, 0.904)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                                 spexp = list("triangular", parms = c(0.912, 0.942, 0.927)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                                 corr_se = 0.8, #correlation between SE among those with and without outcome
                                 corr_sp = 0.8) #correlation between SE among those with and without outcome

### Males, 60+ years ###
# SE Obesity+: 83 (80.4-85.6)
# SE Obesity-: 66.5 (64.1-68.9)
# SP Obesity+: 85.7 (80.6-90.9)
# SP Obesity-: 91.7 (89.6-93.7)
out_misclass_m_60 <- probsens(matrix(c(252, 333, 136, 329), #contingency table cells (a,b,c,d)
                                     dimnames = list(c("HTN_sr+", "HTN_sr-"), c("Obesity_m+", "Obesity_m-")),
                                     nrow = 2, byrow = TRUE),
                              type = "outcome",
                              reps = 50000,
                              seca = list("triangular", parms = c(0.804, 0.856, 0.83)), #SE of outcome classification among those with exposure (lower limit, upper limit, mode)
                              seexp = list("triangular", parms = c(0.641, 0.689, 0.665)), #SE of outcome classification among those without exposure (lower limit, upper limit, mode)
                              spca = list("triangular", parms = c(0.806, 0.909, 0.857)), #SP of outcome classification among those with exposure (lower limit, upper limit, mode)
                              spexp = list("triangular", parms = c(0.896, 0.937, 0.917)), #SP of outcome classification among those without exposure (lower limit, upper limit, mode)
                              corr_se = 0.8, #correlation between SE among those with and without outcome
                              corr_sp = 0.8) #correlation between SE among those with and without outcome


#################################################################################################################################
# UNCONTROLLED CONFOUNDING
#################################################################################################################################

### Females, 18-39 years ###
# P_exposed: 26.3 (23.9-28.7)
# P_non-exposed: 19.7 (17.9-21.5)
# OR_poverty-HTN: 1.29 (1.08-1.55)
unc_c_f_18_39 <- probsens_conf(matrix(c(104, 54, 194, 432), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("triangular", parms = c(0.239, 0.287, 0.263)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                               prev_nexp = list("triangular", parms = c(0.179, 0.215, 0.197)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                               risk = list("triangular", parms = c(1.08, 1.55, 1.29)), #OR of confounder-outcome association (lower limit, upper limit, mode)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Females, 40-59 years ###
# P_exposed: 13.9 (12-15.9)
# P_non-exposed: 10.2 (8.7-11.7)
# OR_poverty-HTN: 1.20 (1.01-1.42)
unc_c_f_40_59 <- probsens_conf(matrix(c(242, 163, 140, 249), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("triangular", parms = c(0.12, 0.159, 0.139)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                               prev_nexp = list("triangular", parms = c(0.087, 0.117, 0.102)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                               risk = list("triangular", parms = c(1.01, 1.42, 1.20)), #OR of confounder-outcome association (lower limit, upper limit, mode)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Females, 60+ years ###
# P_exposed: 12.9 (11-14.8)
# P_non-exposed: 10 (8.8-11.2)
# OR_poverty-HTN: 1.33 (1.03-1.73)
unc_c_f_60 <- probsens_conf(matrix(c(397, 457, 109, 262), #contingency table cells (a,b,c,d)
                                   dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                   nrow = 2, byrow = TRUE),
                            reps = 50000,
                            prev_exp = list("triangular", parms = c(0.11, 0.148, 0.129)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                            prev_nexp = list("triangular", parms = c(0.088, 0.112, 0.10)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                            risk = list("triangular", parms = c(1.03, 1.73, 1.33)), #OR of confounder-outcome association (lower limit, upper limit, mode)
                            corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 18-39 years ###
# P_exposed: 16.1 (14.2-18)
# P_non-exposed: 18.2 (16.3-20.2)
# OR_poverty-HTN: 0.84 (0.71-0.98)
unc_c_m_18_39 <- probsens_conf(matrix(c(109, 83, 106, 362), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("triangular", parms = c(0.142, 0.18, 0.161)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                               prev_nexp = list("triangular", parms = c(0.163, 0.202, 0.182)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                               risk = list("triangular", parms = c(0.71, 0.98, 0.84)), #OR of confounder-outcome association (lower limit, upper limit, mode)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 40-59 years ###
# P_exposed: 9.6 (8-11.1)
# P_non-exposed: 11.8 (10.4-13.3)
# OR_poverty-HTN: 1.04 (0.88-1.22)
unc_c_m_40_59 <- probsens_conf(matrix(c(205, 170, 75, 202), #contingency table cells (a,b,c,d)
                                      dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                      nrow = 2, byrow = TRUE),
                               reps = 50000,
                               prev_exp = list("triangular", parms = c(0.08, 0.111, 0.096)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                               prev_nexp = list("triangular", parms = c(0.104, 0.133, 0.118)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                               risk = list("triangular", parms = c(0.88, 1.22, 1.04)), #OR of confounder-outcome association (lower limit, upper limit, mode)
                               corr_p = 0.8) #correlation between confounder prevalence among those without and without exposure

### Males, 60+ years ###
# P_exposed: 7 (5.4-8.7)
# P_non-exposed: 8.5 (7.3-9.7)
# OR_poverty-HTN: 1.3 (1.04-1.62)
unc_c_m_60 <- probsens_conf(matrix(c(312, 464, 76, 198), #contingency table cells (a,b,c,d)
                                   dimnames = list(c("HTN_m+", "HTN_m-"), c("Obesity_m+", "Obesity_m-")),
                                   nrow = 2, byrow = TRUE),
                            reps = 50000,
                            prev_exp = list("triangular", parms = c(0.054, 0.087, 0.07)), #Prevalence of confounder among exposed (lower limit, upper limit, mode)
                            prev_nexp = list("triangular", parms = c(0.073, 0.097, 0.085)), #Prevalence of confounder among non-exposed (lower limit, upper limit, mode)
                            risk = list("triangular", parms = c(1.04, 1.62, 1.3)), #OR of confounder-outcome association (lower limit, upper limit, mode)
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

