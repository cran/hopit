## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
unlink('vignettes/vignette_cache', recursive = TRUE)

## ---- echo=FALSE, results='asis'-----------------------------------------
library(pander)
dd <- c(
'Categorical response           ',	'$y$',	'Dependent variable obtained during the survey',	'Self-rated health, self-rated happiness',
'Latent measure                 ',	'$h$',	'Modeled continuous latent measure of the investigated response variable',	'Latent health, latent happiness',
'Latent index                   ',	'$H$',	'Standardized latent measure',	'Health index, happiness index',
'Latent variables               ',  '  ---',	 	'Variables used to model the latent measure',	'Health variables, happiness variables',
'Latent terms                   ',	'$X$',	'Terms of the design matrix used to model the latent measure', '',
'Latent coefficients            ',	'$\\beta$',	'Coefficients corresponding to each latent term','',
'Standardized coefficient           ',	'$D$',	'Standardized value of a coefficient', 'Disability weights',
'Thresholds                     ',  '$\\alpha$',	'Thresholds used to group the latent measure ', 	'Cut-points',
'Threshold variables            ',  '  ---',		'Variables used to model the thresholds	','Socio-demographic, cultural, contextual variables',
'Threshold terms                ',	'$Y$',	'Terms of the design matrix used to model the latent measure','',
'Threshold coefficients         ',	'$\\gamma$, $\\lambda$',	'Coefficients corresponding to each threshold term','')

dd <- matrix(dd, 11, 4, byrow=TRUE)
colnames(dd) <- c('Term',	'Symbol',	'Definition',	'Exemplary case specific synonyms')
dd <- as.data.frame(dd)
pander(dd,style='multiline',split.tables=Inf,justify='lccc',split.cells=c(Inf,3,25,25))

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(devtools)
#  install_github("maciejdanko/hopit")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(hopit)

## ---- echo=FALSE,  results='hide', eval=TRUE, include=FALSE--------------
g <- capture.output(library(hopit))

## ---- echo=TRUE, cache=TRUE----------------------------------------------
# load *healthsurvey* dataset
data(healthsurvey)

# horizontal view of the dataset (omitting ID)
print(t(healthsurvey[1:6,-1]), quote=FALSE, na.print='NA', right=TRUE)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
# first determine the order of the dependent variable
levels(healthsurvey$health)

# the order is decreasing (from the best to the worst health state)
# so we set: decreasing.levels = TRUE
model1<- hopit(latent.formula = health ~ hypertension + high_cholesterol + 
                             heart_attack_or_stroke + poor_mobility + very_poor_grip + 
                             depression + respiratory_problems + 
                             IADL_problems + obese + diabetes + other_diseases, 
               thresh.formula = ~ sex + ageclass,
               decreasing.levels = TRUE,
               control=list(trace=FALSE),
               data = healthsurvey)

summary(model1)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
# extract parameters in the form of a list
cm1 <- coef(model1, aslist = TRUE)

# names of the returned coefficients
names(cm1)

# extracting the latent health coefficients
cm1$latent.params

## ---- echo=TRUE, cache=TRUE----------------------------------------------
model2<- hopit(latent.formula = health ~ hypertension + high_cholesterol + 
                      heart_attack_or_stroke + poor_mobility + 
                      very_poor_grip + depression + respiratory_problems + 
                      IADL_problems + obese + diabetes + other_diseases, 
               thresh.formula = ~ sex + ageclass + country,
               decreasing.levels = TRUE,
               control=list(trace=FALSE),
               data = healthsurvey)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
AIC(model2, model1)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
anova(model2, model1)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
model3<- hopit(latent.formula = health ~ hypertension + high_cholesterol + 
                      heart_attack_or_stroke + poor_mobility + 
                      very_poor_grip + depression + respiratory_problems + 
                      IADL_problems + obese + diabetes + other_diseases, 
               thresh.formula = ~ sex * ageclass + country,
               decreasing.levels = TRUE,
               control=list(trace=FALSE),
               data = healthsurvey)

print(anova(model3,model2), short=TRUE)


## ---- echo=TRUE, cache=TRUE----------------------------------------------
model4<- hopit(latent.formula = health ~ hypertension + high_cholesterol + 
                      heart_attack_or_stroke + poor_mobility + 
                      very_poor_grip + depression + respiratory_problems + 
                      IADL_problems + obese + diabetes + other_diseases +
                      sex : respiratory_problems, 
               thresh.formula = ~ sex * ageclass + country + sex : depression,
               decreasing.levels = TRUE,
               control=list(trace=FALSE),
               data = healthsurvey)

print(anova(model3,model4), short=TRUE)


## ---- echo=TRUE, cache=TRUE----------------------------------------------
design <- svydesign(ids = ~ country + psu, weights = healthsurvey$csw, 
                    data = healthsurvey)

model2s<- hopit(latent.formula = health ~ hypertension + high_cholesterol + 
                       heart_attack_or_stroke + poor_mobility + 
                       very_poor_grip + depression + respiratory_problems + 
                       IADL_problems + obese + diabetes + other_diseases, 
                thresh.formula = ~ sex + ageclass + country,
                decreasing.levels = TRUE,
                design = design,
                control=list(trace=FALSE),
                data = healthsurvey)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
cbind('No survey design'=coef(model2,aslist=TRUE)$latent.par,
      'Has survey design'=coef(model2s,aslist=TRUE)$latent.par)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
profile(model3)

## ---- echo=TRUE, cache=TRUE----------------------------------------------
model3$coef.ls$latent.params

## ---- echo=TRUE, fig.height = 5, fig.width = 5, fig.align = "center", cache=TRUE----
# A function that modifies the coefficient names.  
txtfun <- function(x) gsub('_',' ',substr(x,1,nchar(x)-3))

# Calculate and plot the disability weights
sc <- standardizeCoef(model3, plotf = TRUE, namesf = txtfun)
sc

## ---- echo=TRUE, fig.height = 4, fig.width = 5, fig.align = "center", cache=TRUE----
hi <- latentIndex(model3, plotf = TRUE, response = "data", 
                  ylab = 'Health index', col='deepskyblue3')

## ---- echo=TRUE, fig.height = 4, fig.width = 5, fig.align = "center", cache=TRUE, eval=FALSE----
#  hi <- latentIndex(model3, plotf = TRUE, response = "fitted",
#                    ylab = 'Health index', col='deepskyblue3')

## ---- echo=TRUE, fig.height = 4, fig.width = 5, fig.align = "center", cache=TRUE, eval=FALSE----
#  hi <- latentIndex(model3, plotf = TRUE, response = "Jurges",
#                    ylab = 'Health index', col='deepskyblue3')

## ---- echo=TRUE, fig.height = 3.8, fig.width = 4.6, fig.align = "center", cache=TRUE----
z <- getCutPoints(model=model3)

# Health index cut-points
z$cutpoints

# Adjusted health levels for individuals (using the Jürges method)
rev(table(z$adjusted.levels))

# Original health levels for individuals
table(model3$y_i)

# Adjusted health levels for individuals (using estimated model thresholds)
table(model3$Ey_i)

## ---- echo=TRUE, cache=TRUE, fig.height = 4, fig.width = 6, fig.align = "center", cache=TRUE----
# Health levels for age and gender, and pooled country of origin.
hl <- getLevels(model = model3, formula = ~ sex + ageclass, data = healthsurvey, 
                      sep=' ', plotf=TRUE)

## ---- echo=TRUE----------------------------------------------------------
round(100*(hl$original - hl$adjusted),2) # in (%)


## ---- echo=TRUE, fig.height = 5.6, fig.width = 4.7, fig.align = "center", cache=TRUE----
# the function to be bootstraped
diff_BadHealth <- function(model, data) {
  hl <- getLevels(model = model, formula = ~ sex + ageclass, data = data, 
                  sep = ' ', plotf = FALSE)
  hl$original[,1] + hl$original[,2] - hl$adjusted[,1]- hl$adjusted[,2]
}

# estimate the difference
est.org <- diff_BadHealth(model = model3, data = healthsurvey)

# perform the bootstrap
B <- boot_hopit(model = model3, data = healthsurvey, 
                func = diff_BadHealth, nboot = 100)

# calculate lower and upper bounds using the percentile method
est.CI <- percentile_CI(B)

# plot the difference and its (asymmetrical) confidence intervals
pmar <- par('mar'); par(mar = c(9.5,pmar[2:4]))
m <- max(abs(est.CI))
pos <- barplot(est.org, names.arg = names(est.org), las = 3, ylab = 'Original - Adjusted', 
               ylim=c(-m, m), density = 20, angle = c(45, -45), col = c('blue', 'orange'))
for (k in seq_along(pos)) lines(c(pos[k,1],pos[k,1]), est.CI[,k], lwd = 2, col = 2)
abline(h = 0); box(); par(mar = pmar)


