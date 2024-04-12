############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
############################################### Slide 13	
############################################### Slide 14	
############################################### Slide 15	
# this line is not executed	
x <- 12	
x	
j <- 12	
J	
############################################### Slide 16	
?mean	
############################################### Slide 17	
colors <- c("red", "green", "blue")	
colors	
numbers <- c(1, 2, 3)	
numbers	
############################################### Slide 18	
mean(x = numbers)	
mean(numbers)	
############################################### Slide 19	
############################################### Slide 20	
# to install the package	
install.packages("EdSurvey")	
# to load the package	
library(EdSurvey)	
############################################### Slide 21	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
############################################### Slide 25	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
############################################### Slide 29	
############################################### Slide 30	
############################################### Slide 31	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
############################################### Slide 32	
math19 <- readNAEP("//path_to_directory/Data/M50NT2AT.dat")	
############################################### Slide 33	
############################################### Slide 34	
############################################### Slide 35	
############################################### Slide 36	
print(sdf)	
############################################### Slide 37	
colnames(sdf)	
############################################### Slide 38	
searchSDF("education", data = sdf)	
searchSDF("b003501", data = sdf, levels = TRUE)	
searchSDF("", data = sdf)	
############################################### Slide 39	
levelsSDF(varnames = "b018201", data = sdf)	
############################################### Slide 40	
showCodebook(data = sdf) # or showCodebook(sdf)	
book <- showCodebook(sdf)	
View(showCodebook(sdf))	
############################################### Slide 41	
showPlausibleValues(sdf)	
showPlausibleValues(sdf, verbose = TRUE)	
############################################### Slide 42	
showWeights(sdf)	
showWeights(sdf, verbose = TRUE)	
############################################### Slide 43	
############################################### Slide 44	
############################################### Slide 45	
############################################### Slide 46	
############################################### Slide 47	
############################################### Slide 48	
# table(sdf$b017451)	
sdf$b017451_recode <- ifelse(sdf$b017451 %in% c("Omitted",	
                                                "Multiple"),	
                             "Omitted or multiple",	
                             as.character(sdf$b017451))	
# table(sdf$b017451_recode, sdf$b017451)	
############################################### Slide 49	
dat <- sdf[,c("b017451", "ell3")]	
dat <- sdf[,colnames(sdf)]	
############################################### Slide 50	
############################################### Slide 51	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
                 addAttributes = TRUE, dropOmittedLevels = FALSE)	
############################################### Slide 52	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, dropOmittedLevels = FALSE)	
############################################### Slide 53	
subsetSDF <- subset(gddat, dsex %in% c("Male"))	
dim(sdf)	
dim(subsetSDF)	
############################################### Slide 54	
library(dplyr)	
library(tidyEdSurvey)	
# with tidyEdSurvey loaded, you can just use dplyr	
subsetSDF <- sdf %>% filter(sdracem != "White")	
head(subsetSDF[,1:10], 6)	
############################################### Slide 55	
showWeights(subsetSDF)	
names(attributes(subsetSDF))	
############################################### Slide 56	
subsetSDF <- sdf %>% 	
  filter(sdracem != "White") %>%  	
  rebindAttributes(sdf) # adding rebindAttributes	
showWeights(subsetSDF)	
names(attributes(subsetSDF))	
############################################### Slide 57	
############################################### Slide 58	
sdf$b017451_edited <- ifelse(sdf$b017451 %in% c("Never or hardly ever", "Once every few weeks"),	
                             "Infrequently",	
                             sdf$b017451)	
#table(sdf$b017451_edited, sdf$b017451)	
############################################### Slide 59	
############################################### Slide 60	
# R code for loading ggplot2	
install.packages("ggplot2") # only necessary once, or if updating	
# R code for loading ggplot2	
library(ggplot2)	
library(tidyEdSurvey)	
sdf$sdracem <- as.factor(sdf$sdracem)	
sdf$dsex <- as.factor(sdf$dsex)	
sdf$dsex <- as.factor(sdf$dsex)	
############################################### Slide 61	
############################################### Slide 62	
library(tidyEdSurvey)	
# R code generating Figure 1	
bar1 <- ggplot(data=sdf, aes(x=b017451)) +	
  geom_bar() +	
  coord_flip() +	
  labs(title = "Figure 1") 	
bar1	
############################################### Slide 63	
# R code for generating Figure 2	
bar2 <- ggplot(data=sdf, aes(x=b017451, fill=dsex)) +	
  geom_bar(position='dodge') +	
  coord_flip() +	
  labs(title = "Figure 2")	
bar2	
############################################### Slide 64	
# R code for generating Figure 3	
hist1 <- ggplot(sdf, aes(x=mrpcm1)) +	
    geom_histogram(bins=30) + # geom type changed	
    labs(title = "Figure 3")	
hist1	
############################################### Slide 65	
hist2 <- ggplot(sdf, aes(x=mrpcm1, fill = dsex)) +	
     geom_histogram(position="identity", alpha = 0.6, bins=30) + # alpha adjust color transparency	
     labs(title = "Figure 4") + 	
     scale_fill_manual(values=c("#E69F00", "#56B4E9")) # scale fill can change default colors	
hist2	
############################################### Slide 66	
ggplot(sdf, aes(x=mrpcm1)) +	
     geom_histogram(position="identity", alpha = 0.6, bins=30) + # alpha adjust color transparency	
     geom_vline(aes(xintercept = mean(mrpcm1)),col='red',linewidth=1) + # add mean line	
     theme_minimal() + 	
     labs(y = "count") + 	
     facet_wrap(~ sdracem, ncol = 3 ) # great graph faceted by group 	
############################################### Slide 67	
# R code for generating Figure 4	
box1 <- ggplot(sdf, aes(x=sdracem, y=mrpcm1)) + 	
  geom_boxplot() +	
  stat_summary(fun=mean, geom="point", shape=23, size=4) + # transorm data in function	
  coord_flip() +	
  labs(title = "Figure 4") 	
box1	
############################################### Slide 68	
# R code for generating Figure 4	
box1 <- ggplot(sdf, aes(x=sdracem, y=mrpcm1), fill = dsex) + 	
  geom_boxplot(aes(fill = dsex)) +	
  stat_summary(fun=mean, geom="point", shape=23, size=4) + # transorm data in function	
  coord_flip() +	
  labs(title = "Figure 4") 	
box1	
