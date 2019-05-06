library(dplyr)

head(CPSch3)

avgs <- CPSch3 %>% 
  group_by(sex, year) %>%
  summarise(mean(ahe),
            sd(ahe),
            n())

print(avgs)

male <- avgs %>% filter(sex == "male")
female <- avgs %>% filter(sex == "female")

colnames(male) <- c("Sex", "Year", "Y_bar_m", "s_m", "n_m")

colnames(female) <- c("Sex", "Year", "Y_bar_f", "s_f", "n_f")

#estimate gender gaps
gap <- male$Y_bar_m - female$Y_bar_f

#compute standard errors 
gap_se <- sqrt(male$s_m^2 / male$n_m + female$s_f^2 / female$n_f)

#compute confidence interval 
gap_ci_l <- gap - 1.96 * gap_se

gap_ci_u <- gap + 1.96 * gap_se

result <- cbind(male[,-1], female[,-(1:2)], gap, gap_se, gap_ci_l,gap_ci_u)
---------------------------------------------------------------------------------------------------
  
  set.seed(123)

X <- runif(n=100,
           min = 18,
           max = 70)
Y <- X + rnorm(n=100, 50,15)

plot(X,Y,
     type = "p",
     main = "A Scatterplot of X and Y",
     xlab = "Age",
     ylab = "Earnings",
     col = "steelblue",
     pch = 19)

#estimate pop variance of X and Y
cov(X,Y)

#estimate sample corr between X and Y
cor(X,Y)
#or
cov(X,Y) / (sd(X) * sd(Y))

---------------------------------------------------------------------------------------------------
  library(AER)
library(MASS)

# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635) 

# Print out sample data
STR

#load data from AER
data("CASchools")

CASchools$STR <- CASchools$students/ CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math) / 2

# compute sample averages of STR and score
avg_STR <- mean(CASchools$STR) 
avg_score <- mean(CASchools$score)

# compute sample standard deviations of STR and score
sd_STR <- sd(CASchools$STR) 
sd_score <- sd(CASchools$score)

# set up a vector of percentiles and compute the quantiles 
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_STR <- quantile(CASchools$STR, quantiles)
quant_score <- quantile(CASchools$score, quantiles)

# gather everything in a data.frame 
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR",
     xlab = "STR(X)",
     ylab = "Test score (Y)")

cor(CASchools$STR, CASchools$score)

attach(CASchools) # allows to use the variables contained in CASchools directly

# compute beta_1_hat
beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)

# compute beta_0_hat
beta_0 <- mean(score) - beta_1 * mean(STR)

linear_model <- lm(score ~ STR,data = CASchools)

# plot the data
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))

# add the regression line
abline(linear_model) 

mod_summary <- summary(linear_model)

SSR <- sum(mod_summary$residuals^2)
TSS <- sum((score - mean(score))^2)

R2 <- 1- SSR/TSS

#compute SER manually 
n <- nrow(CASchools)
SER <- sqrt(SSR / (n-2))


Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

X <- c(5000, rep(NA, length(Date)-1))

for (i in 2:length(Date)){
  X[i] <- -50 + 0.98 * X[i-1] + rnorm(n=1, sd = 200)
}

plot(x = Date,
     y = X,
     type = "l",
     col = "steelblue",
     ylab = "Workers",
     xlab = "Time")
#Simulation 1

# simulate data
N <- 100000
X <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

# population regression
Y <- -2 + 3.5 * X + u
population <- data.frame(X, Y)

# set sample size
n <- 100

# compute the variance of beta_hat_0
H_i <- 1 - mean(X) / mean(X^2) * X
var_b0 <- var(H_i * u) / (n * mean(H_i^2)^2 )

# compute the variance of hat_beta_1
var_b1 <- var( ( X - mean(X) ) * u ) / (100 * var(X)^2)

# set repetitions and sample size
n <- 100
reps <- 10000

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# loop sampling and estimation of the coefficients
for (i in 1:reps){
  
  sample <- population[sample(1:N, n), ]
  fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
  
}

# compute variance estimates using outcomes
var(fit[, 1])

var(fit[,2])

# divide plotting area as 1-by-2 array
par(mfrow = c(1, 1))

# plot histograms of beta_0 estimates
hist(fit[, 1],
     cex.main = 1,
     main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[0] ~ Estimates), 
     xlab = bquote(hat(beta)[0]), 
     freq = F)

# add true distribution to plot
curve(dnorm(x, 
            -2, 
            sqrt(var_b0)), 
      add = T, 
      col = "darkred")

# plot histograms of beta_hat_1 
hist(fit[, 2],
     cex.main = 1,
     main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[1] ~ Estimates), 
     xlab = bquote(hat(beta)[1]), 
     freq = F)

# add true distribution to plot
curve(dnorm(x, 
            3.5, 
            sqrt(var_b1)), 
      add = T, 
      col = "darkred")
--------------------------------------------------------------------------------------------------
  
  # set seed for reproducibility
  set.seed(1)

# set repetitions and the vector of sample sizes
reps <- 1000
n <- c(100, 250, 1000, 3000)

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# divide the plot panel in a 2-by-2 array
par(mfrow = c(1, 1))

# loop sampling and plotting

# outer loop over n
for (j in 1:length(n)) {
  
  # inner loop: sampling and estimating of the coefficients
  for (i in 1:reps){
    
    sample <- population[sample(1:N, n[j]), ]
    fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
    
  }
  
  # draw density estimates
  plot(density(fit[ ,2]), xlim=c(2.5, 4.5), 
       col = j, 
       main = paste("n=", n[j]), 
       xlab = bquote(hat(beta)[1]))
  
}
--------------------------------------------------------------------------------------------------
  library(scales)
data("CASchools")

# add student-teacher ratio
CASchools$STR <- CASchools$students/CASchools$teachers

# add average test-score
CASchools$score <- (CASchools$read + CASchools$math)/2

# estimate the model
linear_model <- lm(score ~ STR, data = CASchools)          

summary(linear_model)$coefficients

#determine residual degrees of freedom
linear_model$df.residual


# set seed for reproducibility
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)

plot(Y, 
     pch = 19, 
     col = "steelblue")

cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5)

set.seed(1)

lower <- numeric(10000)
upper <- numeric(10000)

for(i in 1:10000){
  Y <- rnorm(100, mean = 5, sd = 5)
  lower[i] <- mean(Y) - 1.96 * 5 / 10
  upper[i] <- mean(Y) + 1.96 * 5 / 10
}

CIs <- cbind(lower, upper)

mean(CIs[,1] <= 5 & 5 <= CIs[,2])


# identify intervals not covering mu
# (4 intervals out of 100)
ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

# initialize the plot
plot(0, 
     xlim = c(3, 7), 
     ylim = c(1, 100), 
     ylab = "Sample", 
     xlab = expression(mu), 
     main = "Confidence Intervals")

# set up color vector
colors <- rep(gray(0.6), 100)
colors[ID] <- "red"

# draw reference line at mu=5
abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
for(j in 1:100) {
  
  lines(c(CIs[j, 1], CIs[j, 2]), 
        c(j, j), 
        col = colors[j], 
        lwd = 2)
  
}


confint(linear_model)


# compute 95% confidence interval for coefficients in 'linear_model' by hand
lm_summ <- summary(linear_model)

c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])

CASchools$D <- CASchools$STR < 20

plot(CASchools$D, CASchools$score,
     pch = 20,
     cex = 0.5,
     col = "Steelblue",
     xlab = expression(D[i]),
     ylab = "Test Score",
     main = "Dummy Regression")

#estimate the dummy regression model 

dummy_model <- lm(score ~ D, data = CASchools)

set.seed(123)

# load package and attach data
library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)

# get an overview
summary(CPSSWEducation)

labor_model <- lm(earnings ~ education)

plot(education,
     earnings,
     ylim = c(0,150))

abline(labor_model, 
       col = "steelblue",
       lwd = 2)
confint(labor_model)


t <- c()
t.rob <- c()

for (i in 1:10000) {
  X <- 1:1000
  Y <- rnorm(n = 1000,  mean = X, sd = 0.6*X)
  
  reg <- lm(Y~X)
  t[i] <- linearHypothesis(reg, "X = 1")$'Pr(>F)'[2]< 0.05
  t.rob[i] <- linearHypothesis(reg, "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
  
}

round(cbind(t = mean(t), t.rob = mean(t.rob)),3)
-------------------------------------------------------------------------------------------
  
  #BLUE estimator
  
  n <- 100
reps <- 1e5

epsilon <- 0.8
w <- c(rep((1 + epsilon) / n, n / 2), 
       rep((1 - epsilon) / n, n / 2))

ols <- rep(NA, reps)
weightedestimator <- rep(NA, reps)

for (i in 1:reps) {
  y <- rnorm(n)
  ols[i] <- mean(y)
  weightedestimator[i] <- crossprod(w,y)
}

#ols 

plot(density(ols),
     col = "purple",
     lwd = 3,
     main = "Density of OLS and Weighted Estimator",
     xlab = "Estimates")

#weighted 
lines(density(weightedestimator),
      col = "steelblue",
      lwd = 3)

abline(v = 0, lty = 2)

legend('topright', 
       c("OLS", "Weighted"),
       col = c("purple", "steelblue"),
       lwd = 10)

-------------------------------------------------------------------------------------------
  
  # Omitted Variables
  
  CASchools$STR <- CASchools$students / CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math) / 2

cor(CASchools$STR, CASchools$score)

cor(CASchools$STR, CASchools$english)

mod <- lm(score ~ STR, data = CASchools)
mult.mod <- lm(score ~ STR + english, data = CASchools)

# Multicolliniearity 

CASchools$FracEL <- CASchools$english / 100

mult.mod <- lm(score~ STR + english + FracEL, data = CASchools)

summary(mult.mod)

CASchools$NS <- ifelse(CASchools$STR < 12, 0,1)

mult.mod <- lm(score~ computer + english + NS, data = CASchools)

# Imperfect Multicollinearity 
# load packages
library(MASS)
library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  # for cov(X_1,X_2) = 0.25
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
  # for cov(X_1,X_2) = 0.85
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# obtain variance estimates
diag(var(coefs1))

# The distribution of the OLS estimators in Multiple Regression 
n <- 50 

coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

for (i in 1:10000) {
  
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

kde <- kde2d(coefs[, 1], coefs[, 2])

#plot density estimate

persp(kde, theta = 310,
      phi = 30,
      xlab = "beta_1",
      ylab = "beta_2",
      zlab = "Est.Density")
# estimate the correlation between estimators

cor(coefs[,1], coefs[,2])

-----------------------------------------------------------------------------------------
  m <- rbind(c(1,2), c(3,0))
graphics::layout(mat = m)

plot(score ~ english,
     data = CASchools,
     col = "steelblue", 
     pch = 20, 
     xlim = c(0,100),
     cex.main = 0.9,
     main = "Percentage of english lang learners")

plot(score ~ lunch, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20,
     cex.main = 0.9,
     main = "Percentage qualifying for reduced price lunch")

plot(score ~ calworks, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage qualifying for income assistance")
--------------------------------------------------------------------------------------------
  # load the stargazer library
  library(stargazer)
attach(CASchools)
# prepare the data
library(AER)                                                     
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math) / 2       
# estimate different model specifications
spec1 <- lm(score ~ size, data = CASchools)
spec2 <- lm(score ~ size + english, data = CASchools)
spec3 <- lm(score ~ size + english + lunch, data = CASchools)
spec4 <- lm(score ~ size + english + calworks, data = CASchools)
spec5 <- lm(score ~ size + english + lunch + calworks, data = CASchools)

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
               sqrt(diag(vcovHC(spec2, type = "HC1"))),
               sqrt(diag(vcovHC(spec3, type = "HC1"))),
               sqrt(diag(vcovHC(spec4, type = "HC1"))),
               sqrt(diag(vcovHC(spec5, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))

-------------------------------------------------------------------------------------------------
  # fit the quadratic Model
  quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)

# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(CASchools$income, CASchools$score,
     col  = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(linear_model, col = "black", lwd = 2)

# add quatratic function to the plot
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 2) 

# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)

# perform robust F-test 
linearHypothesis(cubic_model, 
                 hypothesis.matrix = R,
                 vcov. = vcovHC, type = "HC1")
---------------------------------------------------------------------------------------------------
  
  set.seed(3)
# simulate data set
X <- runif(100, -5, 5)
Y <- X^2 + rnorm(100)

# estimate the regression function 
ms_mod <- lm(Y ~ X)

# plot the data
plot(X, Y, 
     main = "Misspecification of Functional Form",
     pch = 20,
     col = "steelblue")

# plot the linear regression line
abline(ms_mod, 
       col = "darkred",
       lwd = 2)

# Mesearement error 

set.seed(1)

library(mvtnorm)

dat <- data.frame(
  rmvnorm(1000, c(50,100),
          sigma = cbind(c(10,5), c(5,10))))

colnames(dat) <- c("X", "Y")  

# estimate the model (without measurement error)

noerror_mod <- lm(Y ~ X, data = dat)

# estimate the model (with measurement error in X)
dat$X <- dat$X + rnorm(n = 1000, sd = sqrt(10))
error_mod <- lm(Y~X, data = dat)

# print estimated coefficients to console 
noerror_mod$coefficients

error_mod$coefficients

# plot sample data
plot(dat$X, dat$Y, 
     pch = 20, 
     col = "steelblue",
     xlab = "X",
     ylab = "Y")

# add population regression function
abline(coef = c(75, 0.5), 
       col = "darkgreen",
       lwd  = 1.5)

# add estimated regression functions
abline(noerror_mod, 
       col = "purple",
       lwd  = 1.5)

abline(error_mod, 
       col = "darkred",
       lwd  = 1.5)

# add legend
legend("topleft",
       bg = "transparent",
       cex = 0.8,
       lty = 1,
       col = c("darkgreen", "purple", "darkred"), 
       legend = c("Population", "No Errors", "Errors"))

----------------------------------------------------------------------------------
  
  # Cigarette price increased 
  library(AER)

data("CigarettesSW")
c1995 <- subset(CigarettesSW)

# estimate the model 
cignon_mod <- lm(log(packs) ~ log(price), data = c1995)

plot(log(c1995$price), log(c1995$packs),
     xlab = "ln(Price)",
     ylab = "ln(Consumption)", 
     main = "Demand for Cigarettes",
     pch = 20,
     col = "steelblue")

abline(cignon_mod, col = "darkred",
       lwd = 1.5)

# External & Internal Validity

data("MASchools")
summary(MASchools)
# Customized variables in MASchools
MASchools$score <- MASchools$score4 
MASchools$STR <- MASchools$stratio

# Reproduce Table 9.1 of the book
vars <- c("score", "STR", "english", "lunch", "income")

cbind(CA_mean = sapply(CASchools[, vars], mean),
      CA_sd   = sapply(CASchools[, vars], sd),
      MA_mean = sapply(MASchools[, vars], mean),
      MA_sd   = sapply(MASchools[, vars], sd))


------------------------------------------------------------------------------------------
  data("Fatalities")
# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")

# plot the observations and add the estimated regression line for 1982 data
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5)

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

# Application of Traffic Deaths

fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod

# obtain demeaned data
Fatalities_demeaned <- with(Fatalities,
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                                       beertax = beertax - ave(beertax, state)))

# estimate the regression
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))


library(plm)

# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities, 
                    index = c("state", "year"),
                    model = "within")

# print summary using robust standard errors 
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")


# estimate combined TIME & ENTITY fixed effects 
# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year-1, data = Fatalities)

# via plm()

fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"),
                      model = "within",
                      effect = "twoways")
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1" )

# discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage, 
                            breaks = 18:22,
                            include.lowest = TRUE,
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandatory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")) )

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988),]

# estimate all seven models
fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year,
                       index = c("state","year"),
                       model = "within",
                       effect = "twoways", 
                       data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles,
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)

library(stargazer)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))))

# generate the table
stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3, 
          fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7, 
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))

-------------------------------------------------------------------------------------------------
  # Binary Dependent variable 
  library(AER)
data(HMDA)  

#convert $deny to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1

#estimate a simple linear probability model
denymod1 <- lm(deny ~ pirat, data = HMDA)
# robust standard error 
coeftest(denymod1, vcov. = vcovHC, type = "HC1")

denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"),
                  data = HMDA)
coeftest(denyprobit, vcov. = vcovHC, type = "HC1")

HMDA$lvrat <- factor(
  ifelse(HMDA$lvrat < 0.8, "low", 
         ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")),
  levels = c("low", "medium", "high"))

# convert credit scores to numeric
HMDA$mhist <- as.numeric(HMDA$mhist)
HMDA$chist <- as.numeric(HMDA$chist)

# estimate all 6 models for the denial probability
lpm_HMDA <- lm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
               + insurance + selfemp, data = HMDA)

logit_HMDA <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                  + insurance + selfemp, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

probit_HMDA_1 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_2 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_3 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist 
                     + phist + insurance + selfemp + single + hschool + unemp + condomin 
                     + I(mhist==3) + I(mhist==4) + I(chist==3) + I(chist==4) + I(chist==5) 
                     + I(chist==6), 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_4 <- glm(deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)
---------------------------------------------------------------------------------------------------
  
  library(AER)
data("CigarettesSW")

# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

# compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax)/cpi)

cor(CigarettesSW$salestax, CigarettesSW$price)

# generate a subset for the year 1995

c1995 <- subset(CigarettesSW, year == "1995")

# perform the first stage regression 
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)

coeftest(cig_s1, vcov. = vcovHC, type = "HC1")

# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared

# store the predicted values 
lcigp_pred <- cig_s1$fitted.values

# run the stage 2 regression 
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov. = vcovHC)  

# perform TSLS using ivreg()
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)
coeftest(cig_ivreg, vcov. = vcovHC, type = "HC1")

# subset data for year 1985
c1985 <- subset(CigarettesSW, year == "1985")

# define differences in variables 
packsdiff <- log(c1995$packs) - log(c1985$packs)

pricediff <- log(c1995$price/ c1995$cpi) - log(c1985$price/ c1985$cpi)

incomediff <- log(c1995$income/c1995$population/c1995$cpi) - 
  log(c1985$income/c1985$population/c1985$cpi)

salestaxdiff <- (c1995$taxs - c1995$tax) / c1995$cpi - (c1985$taxs - c1985$tax)/c1985$cpi

cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi


# robust coefficient summary for 1.
coeftest(cig_ivreg_diff1, vcov = vcovHC, type = "HC1")

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))

# generate table
stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
          header = FALSE, 
          type = "html",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
          se = rob_se)
-------------------------------------------------------------------------------------------------
  library(AER)
library(rddtools)
library(scales)
library(tidyr)
library(dplyr)
data(STAR)
head(STAR, 2)
dim(STAR)
names(STAR)

# drop NA for the 1st obs and print
STAR[1, !is.na(STAR[1,])]

# compute differences Estimates for each grades
fmk <- lm(I(readk + mathk) ~ stark, data = STAR)
fm1 <- lm(I(read1 + math1) ~ star1, data = STAR)
fm2 <- lm(I(read2 + math2) ~ star2, data = STAR)
fm3 <- lm(I(read3 + math3) ~ star3, data = STAR)

# obtain coefficient matrix using robust standard errors
coeftest(fmk, vcov = vcovHC, type= "HC1")

# compute robust standard errors for each model and gather them in a list
rob_se_1 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
                 sqrt(diag(vcovHC(fm1, type = "HC1"))),
                 sqrt(diag(vcovHC(fm2, type = "HC1"))),
                 sqrt(diag(vcovHC(fm2, type = "HC1"))))

# generate subset with kindergarten data
STARK <- STAR %>% 
  transmute(gender,
            ethnicity,
            stark,
            readk,
            mathk,
            lunchk,
            experiencek,
            schoolidk) %>%
  mutate(black = ifelse(ethnicity == "afam", 1, 0),
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0), 
         boy = ifelse(gender == "male", 1, 0))

# estimate the models 
gradeK1 <- lm(I(mathk + readk) ~ stark + experiencek, 
              data = STARK)

gradeK2 <- lm(I(mathk + readk) ~ stark + experiencek + schoolidk, 
              data = STARK)

gradeK3 <- lm(I(mathk + readk) ~ stark + experiencek + boy + lunchk 
              + black + race + schoolidk, 
              data = STARK)

# obtain robust inference on the significance of coefficients
coeftest(gradeK1, vcov. = vcovHC, type = "HC1")

# compute robust standard errors for each model and gather them in a list
rob_se_2 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK1, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK2, type = "HC1"))),
                 sqrt(diag(vcovHC(gradeK3, type = "HC1"))))
stargazer(fmk, fm1, fm2, fm3,
          title = "Project STAR - Differences Estimates with 
          Additional Regressors for Kindergarten",
          header = FALSE, 
          type = "latex",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("(1)", "(2)", "(3)", "(4)"),
          dep.var.caption  = "Dependent Variable: Test Score in Kindergarten",
          dep.var.labels.include = FALSE,
          se = rob_se_2) 

# compute the sample standard deviations of test scores
SSD <- c("K" = sd(na.omit(STAR$readk + STAR$mathk)),
         "1" = sd(na.omit(STAR$read1 + STAR$math1)),
         "2" = sd(na.omit(STAR$read2 + STAR$math2)),
         "3" = sd(na.omit(STAR$read3 + STAR$math3)))

# translate the effects of small classes to standard deviations
Small <- c("K" = as.numeric(coef(fmk)[2]/SSD[1]),
           "1" = as.numeric(coef(fm1)[2]/SSD[2]),
           "2" = as.numeric(coef(fm2)[2]/SSD[3]),
           "3" = as.numeric(coef(fm3)[2]/SSD[4]))

# adjust the standard errors
SmallSE <- c("K" = as.numeric(rob_se_1[[1]][2]/SSD[1]),
             "1" = as.numeric(rob_se_1[[2]][2]/SSD[2]),
             "2" = as.numeric(rob_se_1[[3]][2]/SSD[3]),
             "3" = as.numeric(rob_se_1[[4]][2]/SSD[4]))

# translate the effects of regular classes with aide to standard deviations
RegAide<- c("K" = as.numeric(coef(fmk)[3]/SSD[1]),
            "1" = as.numeric(coef(fm1)[3]/SSD[2]),
            "2" = as.numeric(coef(fm2)[3]/SSD[3]),
            "3" = as.numeric(coef(fm3)[3]/SSD[4]))

# adjust the standard errors
RegAideSE <- c("K" = as.numeric(rob_se_1[[1]][3]/SSD[1]),
               "1" = as.numeric(rob_se_1[[2]][3]/SSD[2]),
               "2" = as.numeric(rob_se_1[[3]][3]/SSD[3]),
               "3" = as.numeric(rob_se_1[[4]][3]/SSD[4]))

# gather the results in a data.frame and round
df <- t(round(data.frame(
  Small, SmallSE, RegAide, RegAideSE, SSD),
  digits =  2))

# generate a simple table using stargazer
stargazer(df,
          title = "Estimated Class Size Effects 
          (in Units of Standard Deviations)",
          type = "html", 
          summary = FALSE,
          header = FALSE)

----------------------------------------------------------------------------------------
  # Regression discontinuity
  library(MASS)

mu <- c(0,0)
sigma<- matrix(c(1, 0.7, 0.7, 1), ncol = 2)

set.seed(1234)
d<- as.data.frame(mvrnorm(2000, mu, sigma))
colnames(d) <- c("W", "Y")

#introduce fuzziness 
d$treatProb <- ifelse(d$W < 0, 0, 0.8)

fuzz <- sapply(X = d$treatProb, FUN = function(x) rbinom(1,1, prob = x))

# treatment effect 
d$Y <- d$Y + fuzz * 2

# generate a colored plot of treatment and control group
plot(d$W, d$Y,
     col = c("steelblue", "darkred")[factor(fuzz)], 
     pch= 20, 
     cex = 0.5,
     xlim = c(-3, 3),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")

# add a dashed vertical line at cutoff
abline(v = 0, lty = 2)

# estimate the fuzzy RDD
data <- rdd_data(d$Y, d$W,
                 cutpoint = 0,
                 z = d$treatProb)

frdd_mod <- rdd_reg_lm(rdd_object = data,
                       slope = "same")

# plot estimated FRDD function
plot(frdd_mod, 
     cex = 0.5, 
     lwd = 0.4,
     xlim = c(-4, 4),
     ylim = c(-3.5, 5),
     xlab = "W",
     ylab = "Y")

# estimate SRDD
data <- rdd_data(d$Y, 
                 d$W, 
                 cutpoint = 0)

srdd_mod <- rdd_reg_lm(rdd_object = data, 
                       slope = "same")
srdd_mod
------------------------------------------------------------------------------------
  
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)

# format date column
USMacroSWQ$...1 <- as.yearqtr(USMacroSWQ$...1, format = "%Y:0%q")

# adjust column names
colnames(USMacroSWQ) <- c("Date", "GDP96", "JAPAN_IP", "PCECTPI", "GS10", "GS1", "TB3MS","UNRATE", "EXUSUK", "CPIAUCSL")

# GDP series as xts object
GDP <- xts(USMacroSWQ$GDP96, USMacroSWQ$Date)["1960::2013"]

# GDP growth series as xts object
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))

# plot 
# reproduce Figure 14.1 (a) of the book
plot(log(as.zoo(GDP)),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Quarterly Real GDP")

# reproduce Figure 14.1 (b) of the book
plot(as.zoo(GDPGrowth),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Real GDP Growth Rates")

# compute Logarithms, annual growth rates and 1st lag of growth rates
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s/lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s/lag(s))))
  )
}

# obtain a data.frame with level, logarithm, annual growth rate and its 1st lag of GDP
quants(GDP["2011-07::2013-01"])

# compute the sample autocovariance 
acf(na.omit(GDPGrowth), lag.max = 4, plot = F)

# define series as xts objects 
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]

DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]

JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP),USMacroSWQ$Date)["1960::2013"]

# attach NYSESW data
data("NYSESW")

NYSESW <- xts(Delt(NYSESW))

#divide plotting area into 2x2 matrix
par(mfrow = c(2,2))

# plot the series 
plot(as.zoo(USUnemp),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     xlab = "Date",
     main = "US Unemployment Rate",
     cex.main = 1)

plot(as.zoo(DollarPoundFX),
     col = "steelblue",
     lwd = 2,
     ylab = "Dollar per pound",
     xlab = "Date",
     main = "U.S. Dollar / B. Pound Exchange Rate",
     cex.main = 1)

plot(as.zoo(JPIndProd),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Japanese Industrial Production",
     cex.main = 1)

plot(as.zoo(NYSESW),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent per Day",
     xlab = "Date",
     main = "New York Stock Exchange Composite Index",
     cex.main = 1)

# compute sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), plot = F, lag.max = 10)

# plot sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), main = "Sample Autocorrelation for NYSESW Data")


# Autoregressions

# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

# estimate the model 
ar.ols(GDPGRSub,
       order.max = 1,
       demean = F,
       intercept = T)

# length of data set
N <-length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

# estimate the model
armod <- lm(GDPGR_level ~ GDPGR_lags)

# robust summary
coeftest(armod, vcov. = vcovHC, type = "HC1")

library(forecast)

# assign GDP growth rate in 2012:Q4
new <- data.frame("GDPGR_lags" = GDPGR_level[N-1])

# forecast GDP Growth rate in 2013:Q1
forecast(armod, newdata = new)

# compute the forecast error
forecast(armod, newdata = new)$mean - GDPGrowth["2013"][1]

# R^2
summary(armod)$r.squared

#SER
summary(armod)$sigma

# estimate the AR(2) model 
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level),2))

coeftest(GDPGR_AR2, vcov. = sandwich)

summary(GDPGR_AR2)$r.squared

# AR(2) forecast of GDP growth in 2013:Q1
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1], GDPGR_level[N-2]))

# compute AR(2) forecast error
GDPGrowth["2013"][1] - forecast

-----------------------------------------------------------------------------------
  
# Stock Returns
StockReturns <- ts(SReturns[,3:4],
                   start = c(1931, 1),
                   end = c(2002,12),
                   frequency = 12)

# Estimate AR models 

SR_AR1 <- dynlm(ExReturn ~ L(ExReturn),
                data = StockReturns, start = c(1960,1), end = c(2002,12))

SR_AR2 <- dynlm(ExReturn ~ L(ExReturn),
                data = StockReturns, start = c(1960,1), end = c(2002,12))


SR_AR4 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 1:4),
                data = StockReturns, start = c(1960,1), end = c(2002,12))

# compute robust standard errors
rob_se <- list(sqrt(diag(sandwich(SR_AR1))),
               sqrt(diag(sandwich(SR_AR2))),
               sqrt(diag(sandwich(SR_AR4))))

# generate table using 'stargazer()'
stargazer(SR_AR1, SR_AR2, SR_AR4,
          title = "Autoregressive Models of Monthly Excess Stock Returns",
          header = FALSE, 
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("AR(1)", "AR(2)", "AR(4)"),
          dep.var.caption  = "Dependent Variable: Excess Returns on the CSRP Value-Weighted Index",
          dep.var.labels.include = FALSE,
          covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", 
                               "$excess return_{t-3}$", "$excess return_{t-4}$", 
                               "Intercept"),
          se = rob_se,
          omit.stat = "rsq") 

# Additional Predictors and the ADL Model

# 3-months Treasury bills interest rate 
TB3MS <- xts(USMacroSWQ$TB3MS, USMacroSWQ$Date)["1960::2012"]

# 10 - years Treasury bonds interest rate
TB10YS <- xts(USMacroSWQ$GS10, USMacroSWQ$Date)["1960::2012"]

# term spread
TSpread <- TB10YS - TB3MS

# reproduce 

plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)),
     plot.type = "single",
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Interest Rates")

# define function that transform years to class "yearqtr"
YToYQTR <- function(years){
  return(
    sort(as.yearqtr(sapply(years,paste, c("Q1", "Q2", "Q3", "Q4"))))
  )
}

# recessions
recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001, 2007:2008))


# add color shading for recessions 
xblocks(time(as.zoo(TB3MS)),
        c(time(TB3MS) %in% recessions),
        col = alpha("steelblue", alpha = 0.3))

# add legend 
legend("topright",
       legend = c("TB3MS", "TB10YS"), 
       col = c("darkred", "steelblue"),
       lwd = c(2,2))

# reproduce Figure 14.2 (b) of the book
plot(as.zoo(TSpread), 
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions), 
        col = alpha("steelblue", alpha = 0.3))

# convert growth and spread series to ts objects
GDPGrowth_ts <- ts(GDPGrowth,
                   start = c(1960,1),
                   end = c(2013,4),
                   frequency = 4)
TSpread_ts <- ts(TSpread,
                 start = c(1960,1),
                 end = c(2012,4),
                 frequency = 4)

# join both ts objects
ADLdata <- ts.union(GDPGrowth_ts, TSpread_ts)

# estimate the ADL(2,1) model of GDP growth
GDPGR_ADL21 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + L(TSpread_ts), 
                     start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL21, vcov. = sandwich)
