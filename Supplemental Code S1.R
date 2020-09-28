##Beetle Analysis

library(dplyr)
dat1 <- dat %>% group_by(Species, Night, Hour, Behaviour) %>% summarise(sum.time = sum(Amount.of.time.s))
head(dat1)
tail(dat1)

#######################################################################################
# Question 1. What determines the outcome of aggressive contests (competitive interactions) for the resource?
#######################################################################################

# Hypothesis 1. Body size determines the outcome of competitive interactions.
# Hypothesis 2. Species determines the outcome of competitive interactions.
# Hypothesis 3. Priority determines the outcome of competitive interactions.

#load data
int.dat <- read.csv(file.choose())
names(int.dat)

int.dat$outcome <- cbind(int.dat$orb.wins, int.dat$tom.wins) # we combine wins and losses into one dependent variable - best way to analyze these data
int.dat$rel.mass <- log(int.dat$orb.mass/int.dat$tom.mass) # orb is larger when >0; tom is larger when <0

# remove trials with no interactions
int.dat$sum.wins <- int.dat$orb.wins + int.dat$tom.wins
int.dat2 <- subset(int.dat, !sum.wins == 0)

int.model <- glm(outcome ~ rel.mass + first.to.carcass, data = int.dat2, family = binomial)
# note that rel.mass tests H1, the intercept tests H2, and first.to.carcass tests H3
# the intercept tests H2 because it estimates a difference in the response variable when the difference in mass = 0
# the family is binomial because our response variable is a combination of two columns - orb.wins and tom.wins
summary(int.model) # residual deviance is much larger than the degrees of freedom - model is overdispersed - move to quasibinomial to address this

int.model.a <- glm(outcome ~ rel.mass * first.to.carcass, data = int.dat2, family = quasibinomial)
summary(int.model.a)

# compare models with different predictors using information-theoretic approach with MuMIn package; rank models by delta QAICc
library(MuMIn)
dfun <- function(object) {
  with(object, sum((weights*residuals^2)[weights>0])/df.residual)
} #function to extract the dispersion parameter, for use in dredge
int.model.a.bin <- glm(outcome ~ rel.mass * first.to.carcass, data = int.dat2, family = binomial)

# construct a function that allows for ML estimation - see see Bolker (2014) http://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
x.quasibinomial <- function(...) {
  res = quasibinomial(...)
  res$aic = binomial(...)$aic
  res
}

#model evaluation using Information Theoretic approach
int.model.a.bin2 <- update(int.model.a.bin, family = "x.quasibinomial", na.action = "na.fail")
m1 <- dredge(int.model.a.bin2, rank = "QAICc", chat = dfun(int.model.a.bin)) #chat (c-hat) uses dfun to extract the dispersion parameter from the global model, for use in calculating QAICc values; beta asks for standardized estimates
m1
m1sub <- subset(m1, delta<2) #subset of models where delta QAICc <2

int.model.b <- glm(outcome ~ rel.mass, data = int.dat2, family = quasibinomial)
summary(int.model.b)

# support for H1, large mass predicts winner (P = 0.0412)

#######################################################################################
#######################################################################################
