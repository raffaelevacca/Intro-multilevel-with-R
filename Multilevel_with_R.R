# Author: Raffaele Vacca <raffaele.vacca@unimi.it>
#
# License: Creative Commons Attribution-NonCommercial-ShareAlike CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###       MULTILEVEL ANALYSIS AND HIERARCHICAL LINEAR MODELS (HLMs)         ====
############################################################################## #


# Explore and prepare the data ----
# ============================================================================ =  
## ---- explore

# Packages
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(ggeffects)
library(magrittr)
library(broom)
library(broom.mixed)

# Data
load("./Data/multilevel_data.rda")

# View the data
stud_data
school_data

# How many schools?
stud_data %>%
  pull(school) %>%
  n_distinct

# How many students per school (school size)?
stud_data %>%
  count(school)

# Sort by school size
stud_data %>%
  count(school, sort = TRUE)

# Average school size
stud_data %>%
  count(school) %>%
  summarise(mean(n))

# Calculate mean student SES in each school
stud_data %<>%                      # Note marittr %<>% operator: pipe + assign
  group_by(school) %>%              # Group data frame by school for mutate 
  mutate(mean.ses= mean(ses)) %>%   # Create mean(ses) by school
  ungroup                           # Ungroup data frame


stud_data

# Create student's SES deviation from school mean
(stud_data %<>% 
    mutate(ses.dev = ses - mean.ses))

# Join with more school-level variables (sector)
df <- left_join(stud_data, school_data, by="school")

df

# Groups (school ID) should be a factor (i.e., categorical variable)
df %<>% 
  mutate(school = factor(school))

# Frequencies of school sector: 

# N schools
school_data %>%
  count(sector)
# N students
df %>%
  count(sector)

## ---- end-explore


# Separate analyses by school: scatter plots  ----
# ============================================================================ =  
## ---- scatterplots

# Sample 20 random IDs of catholic schools
set.seed(1129)
school.IDs <- school_data %>%
  filter(sector=="Catholic") %>%
  sample_n(20) %>% 
  pull(school)

school.IDs

# Filter data to just those school IDs
(df.cat <- df %>%
    filter(school %in% school.IDs))

# We can put everything into a single pipe to have shorter code
set.seed(1129)
df.cat <- school_data %>%
  filter(sector=="Catholic") %>%
  sample_n(20) %>% 
  pull(school) %>%
  {filter(df, school %in% .)} # Note the {} brackets so that "." is not used as first argument in filter()

# Same thing for Public schools
set.seed(1129)
df.pub <- school_data %>%
  filter(sector=="Public") %>%
  sample_n(20) %>% 
  pull(school) %>%
  {filter(df, school %in% .)}

# Plot SES vs math score in each of the 20 Catholic schools
# Data and variables
p <- ggplot(df.cat, aes(x=ses.dev, y=mathach)) + 
  # Scatterplot geom
  geom_point(shape=1) + 
  # Un-comment following line to add loess smothing line
  # geom_smooth(method="loess", color= "blue", se=FALSE) + 
  # linear regression line
  geom_smooth(method="lm", color= "red", se=FALSE) + 
  # Facet by school
  facet_wrap(~ school, nrow=4, ncol=5) + 
  # Black/white theme
  theme_bw() 

# See the plot
p

# Same as above for Public schools
ggplot(df.pub, aes(x=ses.dev, y=mathach)) + 
  # Scatterplot geom
  geom_point(shape=1) + 
  # Un-comment following line to add loess smothing line
  # geom_smooth(method="loess", color= "blue", se=FALSE) + 
  # linear regression line
  geom_smooth(method="lm", color= "red", se=FALSE) + 
  # Facet by school
  facet_wrap(~ school, nrow=4, ncol=5) + 
  # Black/white theme
  theme_bw() 

## ---- end-scatterplot


# Separate analyses by school: linear regressions  ----
# ============================================================================ =  

## ---- separate-reg

# Estimate a linear model for mathach as predicted by ses.dev, separately in
# each of the 160 schools.

# First, nest the student data frame by school
nested.df <- df %>% 
  group_by(school) %>%
  nest()

# This creates a school-level data frame (data row = school), with each school's
# row containing the student-level data frame for that school.
nested.df

# What do the numbers of rows and columns indicate in each school's data frame?
# (e.g. [47 x 7])

# The column nested.df$data is a list of data frames, one for each school
class(nested.df$data)
length(nested.df$data)

# For example, look at the data for school 1224:

## Still nested
nested.df %>%
  filter(school=="1224") %>%
  dplyr::select(school, data) 

## Unnested
nested.df %>%
  filter(school=="1224") %>%
  dplyr::select(school, data) %>%
  unnest(cols = c(data))

# Or get the first element of nested.df$data
nested.df$data[[1]]

# Equivalently, with tidyverse syntax
nested.df %>%
  pull(data) %>%
  extract2(1)

# Using the nested data frame, we can now fit a separate linear model in each
# school's data frame (each element of nested.df$data)
lmodels <- nested.df %>%
  # Get all school data frames
  pull(data) %>%
  # Run lm() on each via map
  purrr::map(~ lm(mathach ~ ses.dev, data= .x))

# Note the formula notation in purrr::map(), where .x indicates each element of
# nested.df$data.

# lmodels is now a list of fitted linear models, one for each school
head(lmodels)
class(lmodels)
length(lmodels)

# Instead of creating this list as a separate object, we can create it as a new
# column in nested.df, adding each model to its school's row in nested.df.
nested.df %<>%
  mutate(model = purrr::map(data, 
                            ~ lm(mathach ~ ses.dev, data= .x)))

# Result
nested.df

# The third column of nested.df ($model) now includes the fitted linear model 
# for each school (each row).

# E.g., model for school 1224
nested.df %>%
  filter(school=="1224") %>%
  pull(model) %>%
  extract2(1)

# Get intercept and slope for model of school 1224
nested.df %>%
  filter(school=="1224") %>%
  pull(model) %>%
  # This is needed to get the lm object out of the list object
  extract2(1) %>%
  coef

# Or, for tidy output
nested.df %>%
  filter(school=="1224") %>%
  pull(model) %>%
  extract2(1) %>%
  broom::tidy()

# Let's apply the same code within mutate to get tidy estimation results for all 
# models (all schools)
nested.df %<>%
  mutate(model.results = purrr::map(model, 
                            broom::tidy)
  )

nested.df

# Unnest to view the results
nested.df %>%
  unnest(model.results)

# Let's keep the part we're interested in
lm.coeff <- nested.df %>%
  unnest(model.results) %>%
  dplyr::select(school, term, estimate)

lm.coeff

# Reshape and rename
lm.coeff %<>%
  # Model intercept and slope into two columns
  pivot_wider(names_from = term, values_from = estimate) %>%
  # Rename columns
  dplyr::select(school, intercept = `(Intercept)`, slope = ses.dev)

lm.coeff

# Bring school sector and mean.ses into this data frame.

# Create data frame with school ID, school sector, mean.ses
lm.df <- df %>% 
  dplyr::select(school, sector, mean.ses) %>%
  distinct

lm.df

# Join with lm.coeff
(lm.df %<>%
    left_join(lm.coeff, by="school")
  )

# Now we can get a boxplot of school intercepts by school sector
ggplot(lm.df, aes(x= sector, y= intercept)) + geom_boxplot()

# And a boxplot of school SES slopes by school sector
ggplot(lm.df, aes(x= sector, y= slope)) + geom_boxplot()

# Or both sets of estimates in a scatterplot by school sector
ggplot(lm.df, aes(x= intercept, y= slope)) + 
  # Scatterplot
  geom_point() +
  # Linear regression line
  geom_smooth(method="lm") +
  # Facet by sector
  facet_wrap(~ sector) +
  theme_bw()

# Scatterplot of school intercepts by school mean.ses, by sector
ggplot(lm.df, aes(x= mean.ses, y= intercept)) + 
  geom_point() + 
  geom_smooth(method="loess") +
  facet_wrap(~ sector) +
  theme_bw()

# Scatterplot of school slopes by school mean.ses, by sector
ggplot(lm.df, aes(x= mean.ses, y= slope)) + 
  geom_point() + 
  geom_smooth(method="loess") +
  facet_wrap(~ sector) +
  theme_bw()

## ---- end-separate-reg

# Hierarchical Linear Model: variance components ----
# ============================================================================ =  

## ---- var-comp

# Let's start with a simple variance components model
mod1 <- lmer(mathach ~ 1 + (1 | school), 
             data=df)

# Model 1 is estimated with Restricted Maximum Likelihood (REML) by default.
# You can set REML=FALSE to use Maximum Likelihood (ML) instead.

# See results.

# lmerTest augmented summary function
summary(mod1)

# Results in tidy format
(mod1.res <- tidy(mod1))

# School-level variance estimate
(sigma2_u <- mod1.res %>%
    filter(effect == "ran_pars", group == "school") %>%
    # Standard deviation
    pull(estimate) %>%
    # ^2 = Variance
    .^2)

# Estimate for residual, individual-level variance
(sigma2_e <- mod1.res %>%
    filter(effect == "ran_pars", group == "Residual") %>%
    # Standard deviation
    pull(estimate) %>%
    # ^2 = Variance
    .^2)

# Variance Partition Coefficient
sigma2_u/(sigma2_u + sigma2_e)

# To test for significance of school effect, let's estimate a null single-level
# model
mod1_sl <- lm(mathach ~ 1, 
              data=df)

# Compare the two models with Likelihood Ratio Test (LRT).
anova(mod1, mod1_sl) 

# Note that by default, anova() refits the models with ML so the LRT is correct.

## ---- end-var-comp

# Hierarchical Linear Model: random intercept ----
# ============================================================================ =  

## ---- rand-int

# Random intercept model with fixed slope for individual student SES.
mod2 <- lmer(mathach ~ 1 + ses.dev + (1 | school), 
             data=df)

# See results.
summary(mod2)

# Another option to test significance of single coefficient estimates.
Anova(mod2)

# Kenward-Roger "F" tests with Satterthwaite degrees of freedom, which in this 
# case have very close results (but take much longer to calculate)
# Anova(mod2, test="F")

# Tidy results
(mod2.res <- tidy(mod2))

# See the estimates for the population-level fixed effects: intercept and SES
# slope
mod2.res %>%
  filter(effect == "fixed")

# See the estimates for the variance component parameters (aka random-effect
# parameters)
mod2.res %>%
  filter(effect == "ran_pars")

# School-level variance estimate
(sigma2_u <- mod2.res %>%
  filter(effect == "ran_pars", group == "school") %>%
  # Standard deviation
  pull(estimate) %>%
  # ^2 = Variance
  .^2)

# Residual, individual-level variance
(sigma2_e <- mod2.res %>%
    filter(effect == "ran_pars", group == "Residual") %>%
    # Standard deviation
    pull(estimate) %>%
    # ^2 = Variance
    .^2)

# Variance Partition Coefficient
sigma2_u/(sigma2_u + sigma2_e)

## ---- end-rand-int

# Hierarchical Linear Model: random slope ----
# ============================================================================ =  

## ---- rand-slo

# Random slope model with SES slope allowed to vary by school
mod3 <- lmer(mathach ~ 1 + ses.dev + (1 + ses.dev | school), 
             data=df)

# Results
summary(mod3)

# Tidy results
(mod3.res <- tidy(mod3))

# School-level variance of random intercept
mod3.res %>%
  filter(effect == "ran_pars", group == "school", term == "sd__(Intercept)") %>%
  # Standard deviation
  pull(estimate) %>%
  # ^2 = Variance
  .^2

# School-level variance of random SES slope
mod3.res %>%
  filter(effect == "ran_pars", group == "school", term == "sd__ses.dev") %>%
  # Standard deviation
  pull(estimate) %>%
  # ^2 = Variance
  .^2

# School-level correlation between random intercept and random SES slope
mod3.res %>%
  filter(effect == "ran_pars", group == "school", term == "cor__(Intercept).ses.dev") %>%
  # Standard deviation
  pull(estimate)


## ---- end-rand-slo


# Contextual variables and cross-level interactions ----
# ============================================================================ =  

## ---- contextual

# Estimate the model
mod4 <- lmer(mathach ~ 1 + mean.ses*ses.dev + sector*ses.dev 
             + (1 + ses.dev | school), 
             data=df)

# See results
summary(mod4)
tidy(mod4)

# Test for random slope for ses.dev: compare random intercept vs random slope
# models with same fixed effects.

# Estimate same model as mod4, but without random slope (only random intercept).
mod5 <- lmer(mathach ~ 1 + mean.ses*ses.dev + sector*ses.dev 
             + (1 | school), data=df)

# See results
summary(mod5)
tidy(mod5)

# Test for random slope: LRT comparing random slope model vs random intercept model
anova(mod5, mod4)

# Based on the value of the observed Chisq statistic and its p-value, we keep 
# the random intercept model with fixed slope for ses.dev: mod5

# Get mod5 predicted values by ses.dev, at different levels of school sector and 
# mean.ses.
pred.val <- ggpredict(mod5, terms = c("ses.dev", "mean.ses [-1:0.5 by=0.5]", "sector")) %>%
  as_tibble() %>%
  dplyr::rename(ses.dev = x, mathach = predicted, mean.ses = group, sector = facet)

pred.val

# Plot these predicted effects
ggplot(pred.val) + 
  # Lines of mathach by ses.dev, grouped/colored by sector (Public vs Catholic)
  geom_line(aes(y = mathach, x = ses.dev, group = sector, color = sector)) +
  # Different panels for different mean.ses values
  facet_grid(~ mean.ses) + 
  theme(legend.position="bottom")


## ---- end-contextual

# Examining school random effects and random intercepts ----
# ============================================================================ =  

## ---- rand-eff

# Estimated coefficients in selected model
mod5.res <- tidy(mod5)

# Get fixed parameters from model
mod5.res %>%
  filter(effect == "fixed")

# Fixed intercept
mod5.res %>%
  filter(effect == "fixed", term == "(Intercept)") %>%
  pull(estimate)

# Note that because Public schools are the reference category, for Catholic
# schools (dummy variable=1) we have to add the sectorCatholic parameter to get
# the actual fixed intercept.
mod5.res %>%
  filter(effect == "fixed", term == "sectorCatholic") %>%
  pull(estimate)

# Save the fixed intercepts.

# Fixed intercept for public schools.
(fixed_int_pub <- mod5.res %>%
    filter(effect == "fixed", term == "(Intercept)") %>%
    pull(estimate))

# sectorCatholic fixed effect.
(fixed_slo_cat <- mod5.res %>%
  filter(effect == "fixed", term == "sectorCatholic") %>%
  pull(estimate))

# Fixed intercept for Catholic schools: sum.
(fixed_int_cat <- fixed_int_pub + fixed_slo_cat)

# lme4::ranef can calculate estimates (conditional modes) for the intercept random effect
# for each school
lme4::ranef(mod5) %>% 
  str
ranef(mod5)$school %>%
  head

# Save as data frame
(school.effects <- ranef(mod5)$school %>%
  as_tibble(rownames = "school") %>%
    # These are called u_j in certain textbooks.
    rename(u_j = `(Intercept)`)
    )

# Note that school is character in this data frame: convert to factor.
school.effects %<>% 
  mutate(school = factor(school))

# Add school sector and mean.ses to the data frame of random effects.
# Remember we have these variables here:
lm.df
# Join
school.effects %<>%
  left_join(lm.df, by="school") %>%
  dplyr::select(school, u_j, sector, mean.ses)
  
school.effects

# Add a column with the fixed intercept estimate.
# Remember this is different for Catholic vs Public schools as per calculations 
# above: we use dplyr::case_when() to set the fixed intercept estimate
# for each school depending on sector.
school.effects %<>%
  mutate(fixed.int = case_when(
    sector == "Public" ~ fixed_int_pub,
    sector == "Catholic" ~ fixed_int_cat
  ))

# View
school.effects

# Now we can add the fixed intercept estimate to the school random effect estimate
# to get each school's estimated realization of the random intercept. 
school.effects %<>%
  mutate(ran.int = fixed.int + u_j)

# View
school.effects

# "Best" schools: highest value of random intercept (average math score at mean 
# values of predictors).
school.effects %>%
  arrange(desc(ran.int))

# Best among Catholic schools
school.effects %>% 
  filter(sector=="Catholic") %>%
  arrange(desc(ran.int))

# Best among Public schools
school.effects %>% 
  filter(sector=="Public") %>%
  arrange(desc(ran.int))

# Visualize random intercept distribution by sector
ggplot(school.effects, aes(x=sector, y= ran.int)) + geom_boxplot()

# Visualize random intercepts by mean.ses in Catholic vs Public schools
ggplot(school.effects, 
       aes(x=mean.ses, y=ran.int, color = sector)) + 
  # Scatterplot geom
  geom_point(shape=1) +
  # linear regression line
  geom_smooth(method="lm", se=FALSE) +
  # Black/white theme
  theme_bw() 

## ---- end-rand-eff
