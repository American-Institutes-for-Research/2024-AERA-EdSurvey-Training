############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", 	
                            package="NAEPprimer"))	
	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
summary2(data = sdf, variable = "composite")	
############################################### Slide 8	
summary2(sdf, "composite", weightVar = NULL)	
############################################### Slide 9	
summary2(sdf, "b017451")	
############################################### Slide 10	
summary2(sdf, "b017451", dropOmittedLevels = TRUE)	
############################################### Slide 11	
############################################### Slide 12	
############################################### Slide 13	
es1 <- edsurveyTable(composite ~ dsex + b017451, data = sdf)	
library(knitr)	
library(kableExtra)	
kable(es1$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "30%")	
############################################### Slide 14	
es2 <- edsurveyTable(composite ~ dsex + b017451, data = sdf, pctAggregationLevel = 0)	
library(knitr)	
library(kableExtra)	
kable(es2$data, format="html") %>%	
  kable_styling(font_size = 16) %>%	
  scroll_box(width="100%", height = "60%")	
############################################### Slide 15	
############################################### Slide 16	
############################################### Slide 17	
edexercise <- edsurveyTable(composite ~ iep + b013801,	
                            weightVar = 'origwt', data = sdf)	
edexercise	
############################################### Slide 18	
############################################### Slide 19	
############################################### Slide 20	
############################################### Slide 21	
############################################### Slide 22	
############################################### Slide 23	
mathGap <- gap(variable = "composite", data = sdf,	
               groupA = dsex %in% "Male", 	
               groupB = dsex %in% "Female")	
	
############################################### Slide 24	
mathGap$results	
############################################### Slide 25	
############################################### Slide 26	
# for reference	
help(package = "EdSurvey")	
searchSDF("text", sdf)	
levelsSDF("myvar", sdf)	
############################################### Slide 27	
exerciseGap <- gap(variable = "composite", data = sdf,	
                   groupA = ell3 %in% "Yes", groupB = ell3 %in% "No",	
                   achievementLevel = c("Proficient"))	
exerciseGap$results	
############################################### Slide 28	
############################################### Slide 29	
############################################### Slide 30	
############################################### Slide 31	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
lm1 <- lm.sdf(composite ~ b017451,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 32	
lm2 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt', data = sdf)	
############################################### Slide 33	
summary(lm2)	
############################################### Slide 34	
summary(lm2, src = TRUE)	
############################################### Slide 35	
lm3 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt',	
              relevels = list(dsex = "Female"), data = sdf)	
############################################### Slide 36	
summary(lm3)	
############################################### Slide 37	
############################################### Slide 38	
lmexercise2 <- lm.sdf(composite ~ b017101 + b018201,	
                      weightVar = 'origwt', data = sdf)	
summary(lmexercise2)	
############################################### Slide 39	
############################################### Slide 40	
############################################### Slide 41	
logit1 <- logit.sdf(I(b013801 %in% ">100") ~ dsex,	
                    weightVar = 'origwt', data = sdf)	
############################################### Slide 42	
summary(logit1)	
############################################### Slide 43	
oddsRatio(logit1)	
############################################### Slide 44	
############################################### Slide 45	
logitexercise1 <- logit.sdf(I(lep %in% "Yes") ~ b018201,	
                          weightVar = 'origwt', data = sdf)	
summary(logitexercise1)	
############################################### Slide 46	
############################################### Slide 47	
############################################### Slide 48	
vignette("introduction", package="EdSurvey")	
	
# There are additional functions that we couldn't cover!	
gap() #gap analysis	
cor.sdf() # Bivariate correlations using "Pearson", "Spearman", "polychoric", or "polyserial" methods	
edsurveyTable2pdf() # creating production ready summary tables	
cbind(), rbind(), append(), merge() # useful functions in processing data	
help(package = "EdSurvey")	
############################################### Slide 49	
############################################### Slide 50	
