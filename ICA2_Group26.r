############################################################
# Load packages
############################################################
library(MASS)
library(mgcv)


############################################################
# Load data
############################################################
covid <- read.csv(
  "UKCovidWave1.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)


############################################################
# Preprocess
############################################################

## 1. Basic cleaning

# Convert categorical variables
covid$Region <- factor(covid$Region)
covid$RUCode <- factor(covid$RUCode)

# Recode missing outcomes
covid$Deaths_obs <- covid$Deaths
covid$Deaths_obs[covid$Deaths_obs == -1] <- NA

# Indicator for prediction rows
covid$is_pred_row <- is.na(covid$Deaths_obs)

## 2. Split data

train_dat <- subset(covid, !is_pred_row)
pred_dat  <- subset(covid,  is_pred_row)


## 3. Helper function

safe_prop <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA, num / den)
}

## 4. Create key derived variables

add_key_variables <- function(dat) {
  # Population size (useful for offset later)
  dat$log_pop <- log(dat$PopTot)
  
  # Age structure
  dat$age65p <- dat$`Age65-74` + dat$`Age75-84` + dat$`Age85-89` + dat$`Age90+`
  dat$age75p <- dat$`Age75-84` + dat$`Age85-89` + dat$`Age90+`
  dat$p_age65 <- safe_prop(dat$age65p, dat$PopTot)
  dat$p_age75 <- safe_prop(dat$age75p, dat$PopTot)
  
  # Care and communal exposure
  dat$care_res <- dat$LACare + dat$PrivCareNurs + dat$PrivCareNoNurs
  dat$p_comm <- safe_prop(dat$PopComm, dat$PopTot)
  dat$p_care <- safe_prop(dat$care_res, dat$PopTot)
  
  # Household vulnerability
  dat$p_hlth <- safe_prop(dat$HH_HealthPrb, dat$HH)
  dat$p_depr <- safe_prop(dat$HHDepriv3 + dat$HHDepriv4, dat$HH)
  dat$p_noch <- safe_prop(dat$HHNoCH, dat$HH)
  
  # Transport exposure
  dat$trans_tot <- dat$MetroUsers + dat$TrainUsers + dat$BusUsers
  dat$p_trans <- safe_prop(dat$trans_tot, dat$PopTot)
  
  # Socioeconomic position
  dat$grade_tot <- dat$GradeAB + dat$GradeC1 + dat$GradeC2 + dat$GradeDE
  dat$p_gradeDE <- safe_prop(dat$GradeDE, dat$grade_tot)
  
  # Education
  dat$qual_tot <- dat$NoQual + dat$Qual1 + dat$Qual2 + dat$QualApp +
    dat$Qual3 + dat$`Qual4+` + dat$QualOther
  dat$p_noqual <- safe_prop(dat$NoQual, dat$qual_tot)
  dat$p_deg <- safe_prop(dat$`Qual4+`, dat$qual_tot)
  
  # Ethnicity / migration
  dat$p_asian  <- safe_prop(dat$EthAsian, dat$PopTot)
  dat$p_black  <- safe_prop(dat$EthBlack, dat$PopTot)
  dat$p_noneu  <- safe_prop(dat$BornNonEU, dat$PopTot)
  
  # Crude death rate (EDA only)
  dat$death_rate_per_1000 <- safe_prop(dat$Deaths_obs, dat$PopTot) * 1000
  
  return(dat)
}

train_dat <- add_key_variables(train_dat)
pred_dat  <- add_key_variables(pred_dat)


############################################################
## EDA(plots will be saved automatically)
############################################################

death_corr <- cor(train_dat$Deaths_obs, train_dat$PopTot, use = "complete.obs")
death_rate_corr <- cor(train_dat$death_rate_per_1000, train_dat$PopTot, use = "complete.obs")

pdf(file = "All_Plots.pdf", width = 7.5, height = 11)

# Table: derived variables created from the original dataset

derived_var_table <- data.frame(
  New_variable = c(
    "log_pop",
    "p_age75",
    "p_comm",
    "p_care",
    "p_hlth",
    "p_depr",
    "p_noch",
    "p_trans",
    "p_gradeDE",
    "p_noqual",
    "p_deg",
    "p_asian",
    "p_black",
    "p_noneu",
    "death_rate_per_1000"
  ),
  Original_variables = c(
    "PopTot",
    "age_75_plus, PopTot",
    "PopComm, PopTot",
    "carehome_residents, PopTot",
    "HH_HealthPrb, HH",
    "HHDepriv3, HHDepriv4, HH",
    "HHNoCH, HH",
    "public_transport_total, PopTot",
    "GradeDE, grade_total",
    "NoQual, qual_total",
    "Qual4+, qual_total",
    "EthAsian, PopTot",
    "EthBlack, PopTot",
    "BornNonEU, PopTot",
    "Deaths_obs, PopTot"
  ),
  Definition = c(
    "log(PopTot)",
    "age_75_plus / PopTot",
    "PopComm / PopTot",
    "carehome_residents / PopTot",
    "HH_HealthPrb / HH",
    "(HHDepriv3 + HHDepriv4) / HH",
    "HHNoCH / HH",
    "public_transport_total / PopTot",
    "GradeDE / grade_total",
    "NoQual / qual_total",
    "Qual4+ / qual_total",
    "EthAsian / PopTot",
    "EthBlack / PopTot",
    "BornNonEU / PopTot",
    "(Deaths_obs / PopTot) * 1000"
  ),
  stringsAsFactors = FALSE
)

derived_var_table$New_variable <- gsub("_", "_ ", derived_var_table$New_variable)

plot.new()
par(mar = c(1, 1, 3, 1)) 

title(main = "Table 1: Derived Variables", cex.main = 1, font.main = 2)
table_text <- capture.output(print(derived_var_table, row.names = FALSE, right = FALSE))
final_str <- paste(table_text, collapse = "\n")
text(x = 0.5, y = 1, 
     labels = final_str, 
     adj = c(0.5, 1), 
     family = "mono",   
     cex = 0.75)        

# Key candidate predictors

var_list <- c(
  "p_age65",
  "p_age75",
  "p_comm",
  "p_care",
  "p_hlth",
  "p_depr",
  "p_noch",
  "p_trans",
  "p_gradeDE",
  "p_noqual",
  "p_deg",
  "p_asian",
  "p_black",
  "p_noneu",
  "PopDens"
)

cor_values <- sapply(var_list, function(v) {
  cor(train_dat[[v]], train_dat$death_rate_per_1000, use = "complete.obs")
})

cor_table <- data.frame(
  Variable = var_list,
  Correlation = cor_values
)


cor_table <- cor_table[order(-abs(cor_table$Correlation)), ]


par(mfrow = c(5, 3), mar = c(2.5, 2.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

# Histogram of observed death counts
hist(
  train_dat$Deaths_obs,
  breaks = 40,
  main = "Distribution of death counts",
  xlab = "Deaths",
  col = "lightgrey",
  border = "white"
)

# Boxplot of observed death counts
boxplot(
  train_dat$Deaths_obs,
  horizontal = TRUE,
  main = "Boxplot of death counts",
  xlab = "Deaths",
  col = "lightblue"
)


plot(
  train_dat$PopTot, train_dat$Deaths_obs,
  pch = 16, cex = 0.5,
  col = rgb(0, 0, 0, 0.25),
  xlab = "Total population",
  ylab = "Observed deaths",
  main = "Deaths vs population"
)

abline(lm(Deaths_obs ~ PopTot, data = train_dat), col = "red", lwd = 2)

plot(
  train_dat$PopTot, train_dat$death_rate_per_1000,
  pch = 16, cex = 0.5,
  col = rgb(0, 0, 0, 0.25),
  xlab = "Total population",
  ylab = "Deaths per 1,000",
  main = "Death rate vs population"
)

boxplot(
  death_rate_per_1000 ~ Region,
  data = train_dat,
  las = 2,
  cex.axis = 0.5,
  col = "lightgrey",
  main = "Death rates by region",
  ylab = "Deaths per 1,000"
)

boxplot(
  death_rate_per_1000 ~ RUCode,
  data = train_dat,
  las = 1,
  cex.axis = 0.9,
  col = "lightgreen",
  main = "Death rates by RUCode",
  ylab = "Deaths per 1,000"
)


# Correlation matrix among candidate predictors
cor_mat <- cor(train_dat[var_list], use = "complete.obs")

# Visual correlation summary

image(
  1:ncol(cor_mat), 1:ncol(cor_mat), cor_mat,
  axes = FALSE,
  xlab = "", ylab = "",
  col = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlation Heatmap"
)

axis(
  1,
  at = 1:ncol(cor_mat),
  labels = colnames(cor_mat),
  las = 2,        
  cex.axis = 0.6  
)

axis(
  2,
  at = 1:ncol(cor_mat),
  labels = colnames(cor_mat),
  las = 2,
  cex.axis = 0.6
)

box()


############################################################
## Model building
############################################################

# Initial Poisson GLM model with all candidate predictors selected from EDA
m_poisson <- glm(
  Deaths_obs ~ p_care + p_age75 + p_depr + p_hlth + PopDens +
    p_asian + p_comm + p_black + p_noqual + p_noch +
    p_trans + p_gradeDE +
    Region + RUCode +
    offset(log_pop),
  family = poisson(link = "log"),
  data = train_dat
)

disp_dev_1 <- deviance(m_poisson) / df.residual(m_poisson)
disp_pear_1 <- sum(residuals(m_poisson, type = "pearson")^2) / df.residual(m_poisson)
dev_exp_1 <- 1 - deviance(m_poisson) / m_poisson$null.deviance

# Quasi-Poisson model
m_qpoisson <- glm(
  Deaths_obs ~ p_care + p_age75 + p_depr + p_hlth + PopDens +
    p_asian + p_comm + p_black + p_noqual + p_noch +
    p_trans + p_gradeDE +
    Region + RUCode +
    offset(log_pop),
  family = quasipoisson(link = "log"),
  data = train_dat
)



# Negative binomial model
m_nb <- glm.nb(
  Deaths_obs ~ p_care + p_age75 + p_depr + p_hlth + PopDens +
    p_asian + p_comm + p_black + p_noqual + p_noch +
    p_trans + p_gradeDE +
    Region + RUCode +
    offset(log_pop),
  data = train_dat
)

# Assess the contribution of each term
single_selection <- drop1(m_nb, test = "Chisq")

# Hierarchical clustering for RUCode
# Select variables used to profile RUCode categories
hc_vars <- c(
  "p_care", "p_age75", "p_depr", "p_hlth", "PopDens",
  "p_asian", "p_comm", "p_black", "p_noqual",
  "p_noch", "p_trans", "p_gradeDE"
)
hc_vars <- hc_vars[hc_vars %in% names(train_dat)]

# Mean covariate values for each RUCode
RU_means <- aggregate(
  train_dat[, hc_vars],
  by = list(RUCode = train_dat$RUCode),
  FUN = mean,
  na.rm = TRUE
)

rownames(RU_means) <- RU_means$RUCode
RU_mat <- RU_means[, -1]

# Standardise before clustering
RU_mat_sc <- scale(RU_mat)

# Hierarchical clustering
RU_dist <- dist(RU_mat_sc)
RU_tree <- hclust(RU_dist, method = "complete")

# Candidate clusterings
RU_grp3 <- cutree(RU_tree, k = 3)
RU_grp4 <- cutree(RU_tree, k = 4)
RU_grp5 <- cutree(RU_tree, k = 5)

# Add grouped RUCode variables to dataset
RU_map3 <- data.frame(RUCode = names(RU_grp3), RUCluster3 = factor(RU_grp3))
RU_map4 <- data.frame(RUCode = names(RU_grp4), RUCluster4 = factor(RU_grp4))
RU_map5 <- data.frame(RUCode = names(RU_grp5), RUCluster5 = factor(RU_grp5))

train_dat <- merge(train_dat, RU_map3, by = "RUCode", all.x = TRUE, sort = FALSE)
train_dat <- merge(train_dat, RU_map4, by = "RUCode", all.x = TRUE, sort = FALSE)
train_dat <- merge(train_dat, RU_map5, by = "RUCode", all.x = TRUE, sort = FALSE)

# Plot dendrogram
plot(RU_tree, main = "Hierarchical clustering of RUCode")

# Negative binomial models with clustered RUCode
fit_nb_rucluster <- function(cluster_var) {
  form <- as.formula(paste(
    "Deaths_obs ~ p_care + p_age75 + p_depr + p_hlth + PopDens +",
    "p_asian + p_comm + p_black + p_noqual + p_noch +",
    "p_trans + p_gradeDE + Region +", cluster_var, "+ offset(log_pop)"
  ))
  glm.nb(form, data = train_dat)
}

# Three models with k = 3, 4 or 5
m_nb_hc3 <- fit_nb_rucluster("RUCluster3")
m_nb_hc4 <- fit_nb_rucluster("RUCluster4")
m_nb_hc5 <- fit_nb_rucluster("RUCluster5")

# Pearson residuals vs Fitted values
plot(fitted(m_nb_hc5), residuals(m_nb_hc5, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted(NB-GLM)")
abline(h = 0, col = "red", lty = 2)

# QQ plot
qqnorm(residuals(m_nb_hc5, type = "pearson"), main = "Normal Q-Q Plot(NB-GLM)")
qqline(residuals(m_nb_hc5, type = "pearson"), col = "red")

# Cook's distance
plot(cooks.distance(m_nb_hc5), type = "h",
     main = "Cook's Distance(NB-GLM)",
     ylab = "Cook's D")
abline(h = 4 / nrow(train_dat), col = "red", lty = 2)

# Fitted values vs Observed values
plot(train_dat$Deaths_obs, fitted(m_nb_hc5),
     xlab = "Observed deaths", ylab = "Fitted deaths",
     main = "Observed vs Fitted(NB-GLM)")
abline(0, 1, col = "red")


# GAM with negative binomial family
m_gam1 <- gam(
  Deaths_obs ~ 
    p_asian
  + s(p_age75, k = 10) 
  + s(p_care, k = 10) 
  + p_depr
  + p_hlth 
  + p_comm 
  + s(p_black, k = 10) 
  + s(p_noqual, k = 10) 
  + s(p_noch, k = 10) 
  + s(p_trans, k = 10) 
  + p_gradeDE
  + s(p_noneu, k = 10)
  + s(Region, bs = "re") + RUCluster5 +
    offset(log_pop),
  family = nb(),
  data = train_dat,
  method = "REML"
)

# Assess dispersion (overdispersion check) and model fit (deviance explained)
dev_gam1 <- deviance(m_gam1)
df_gam1  <- df.residual(m_gam1)
disp_dev_gam1  <- dev_gam1 / df_gam1
disp_pear_gam1 <- sum(residuals(m_gam1, type = "pearson")^2) / df_gam1
dev_exp_gam1   <- 1 - dev_gam1 / m_gam1$null.deviance
 
# GAM model with interactions
m_gam2 <- update(m_gam1, . ~ . + ti(p_age75, p_care, k = c(10, 10)) + ti(p_depr, p_hlth, k = c(10, 10)))
final_model <- m_gam2

# Pearson residuals vs Fitted values
plot(fitted(m_gam2), residuals(m_gam2, type = "pearson"),
     xlab = "Fitted values", ylab = "Pearson residuals",
     main = "Residuals vs Fitted(NB-GAM2)")
abline(h = 0, col = "red", lty = 2)

# QQ plot
qqnorm(residuals(m_gam2, type = "pearson"), main = "Normal Q-Q Plot(NB-GAM2)")
qqline(residuals(m_gam2, type = "pearson"), col = "red")

# Fitted values vs Observed values
plot(train_dat$Deaths_obs, fitted(m_gam2),
     xlab = "Observed deaths", ylab = "Fitted deaths",
     main = "Observed vs Fitted(NB-GAM2)")
abline(0, 1, col = "red")

dev.off()

############################################################
## Prediction
############################################################

# Map RUCode in prediction data to clustered RU groups
idx_pred <- match(as.character(pred_dat$RUCode),
                  as.character(RU_map5$RUCode))

pred_dat$RUCluster5 <- factor(
  RU_map5$RUCluster5[idx_pred],
  levels = levels(train_dat$RUCluster5)
)

# Predict on link scale and get standard error of eta = log(mu)
pred_link <- predict(  final_model,  newdata = pred_dat,  type = "link",  se.fit = TRUE)

eta_hat <- pred_link$fit
se_eta  <- pred_link$se.fit

# Mean prediction on response scale
pred_dat$pred_mean <- exp(eta_hat)

# Negative binomial theta
theta_hat <- final_model$family$getTheta(TRUE)

# Var(Y_i) under negative binomial
var_y <- pred_dat$pred_mean + (pred_dat$pred_mean^2) / theta_hat

# Var(mu_hat) using delta method
var_mu_hat <- (pred_dat$pred_mean^2) * (se_eta^2)

# SD of prediction error
pred_dat$pred_sd <- sqrt(var_y + var_mu_hat)

# Keep only required columns: ID, predicted mean, prediction error SD
pred_output <- pred_dat[, c("ID", "pred_mean", "pred_sd")]

# Write prediction file
write.table(  pred_output,  file = "ICA2_Group26_pred.dat",  sep = " ",  row.names = FALSE,  col.names = FALSE,  quote = FALSE)

