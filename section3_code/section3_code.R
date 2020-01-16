
column_names = c('label','index', 'nr_pix', 'height', 'width', 'tallness',
                 'rows_with_1','cols_with_1', 'rows_with_5_plus', 'cols_with_5_plus', 
                 'one_neighbour', 'three_or_more_neighbours', 'no_neighbours_below', 
                 'no_neighbours_above', 'no_neighbours_before', 'no_neighbours_after', 
                 'nr_regions', 'nr_eyes', 'r5_c5')
column_names

features <- read.csv("../section2_features/40156103_features.csv", header = FALSE, col.names = column_names)

features["Category"] <- NA

features$Category[1:56] <- "Digits"
features$Category[57:112] <- "Letters"
features$Category[113:168] <- "Math"
features$Category <- as.factor(features$Category)

#The features data is the overall data for each symbol in my report. Below I will split the data into three symbol groups

#digitdf is the feature data for the handwritten numbers between 1 and 7
digitdf <- features[1:56,]
#improve labels of code
digitdf$label[digitdf$label==11] <- '1'
digitdf$label[digitdf$label==12] <- '2'
digitdf$label[digitdf$label==13] <- '3'
digitdf$label[digitdf$label==14] <- '4'
digitdf$label[digitdf$label==15] <- '5'
digitdf$label[digitdf$label==16] <- '6'
digitdf$label[digitdf$label==17] <- '7'

#make categorical
digitdf$label <- as.factor(digitdf$label)



#letters df is the feature data for the handwritten letters between a and g
lettersdf <- features[57:112,]

#Improve labels of code
lettersdf$label[lettersdf$label==21] <- 'a'
lettersdf$label[lettersdf$label==22] <- 'b'
lettersdf$label[lettersdf$label==23] <- 'c'
lettersdf$label[lettersdf$label==24] <- 'd'
lettersdf$label[lettersdf$label==25] <- 'e'
lettersdf$label[lettersdf$label==26] <- 'f'
lettersdf$label[lettersdf$label==27] <- 'g'

#make categrical
lettersdf$label <- as.factor(lettersdf$label)

#mathdf is the feature data for the handwritten math symbols
mathdf <- features[113:168,]

mathdf$label[mathdf$label==31] <- '<'
mathdf$label[mathdf$label==32] <- '>'
mathdf$label[mathdf$label==33] <- '='
mathdf$label[mathdf$label==34] <- '<='
mathdf$label[mathdf$label==35] <- '>='
mathdf$label[mathdf$label==36] <- 'not='
mathdf$label[mathdf$label==37] <- 'approx'

mathdf$label <- as.factor(mathdf$label)

#1:Estimate the probability distribution for nr_pix for each of the three symbol groups: letter,
#digit, and math. Visualise the distributions. Briefly describe the shape of the distributions. 

#To perform a probability distribution for nr_pix, I must list all possible outcomes in my digitdf dataset for nr-pix
#digitoutcomes is the sample space

#Originally I used the code below to display the probability distribution of each of the three
#groups of symbols, however I later on discovered through my coding in R that a far neater way to display the respective
#Probabiliy distributions would be to plot a probability density curve on each of the symbols, with a faceted grid for each different symbol group.


# digitoutcomes <- sort(unique(digitdf$nr_pix))
# digitoutcomes
# 
# probs = c() # initialize probs as an empty vector
# for (outcome in digitoutcomes) {
#   probs <- append(probs, sum(digitdf$nr_pix ==outcome)/nrow(digitdf))
# }
# probs
# #probs is a list of the probabilites of each occurence listed in digitoutcomes for nr_pix of each digit symbol
# 
# #Create a new dataframe for the probability distribution of nr_pix of the digitdf dataset. We do this by 
# #creating a matrix from our previously calculated values of digitoutcomes and probs, then converting the matrix to a dataframe
# digit_prob_dist <-cbind(digitoutcomes, probs)
# colnames(digit_prob_dist) <- c('Outcome', 'Probability')
# digit_prob_dist <- as.data.frame(digit_prob_dist)
# 
# 
# #Maybe think about using below for using a table to show the probability dist of nr_pix 
# # as supplementary info alongside the histogram visualisations in the report
# rdigit_prob_dist <- data.frame((digit_prob_dist))
# rdigit_prob_dist <- round(rdigit_prob_dist, 3)
# write.table(rdigit_prob_dist, sep=",", "Probability Distribution table for Number of Pixels in Digits dataset.csv",row.names=FALSE)
# 
# #Plotting the data:
# library(ggplot2)
# dig_nr_pix__hist <- ggplot(digit_prob_dist, aes(x=Outcome, y=Probability)) +
#   geom_bar(stat = "identity", color="darkblue", fill="lightblue") +
#   geom_smooth(se = FALSE) + #plots a line over the bar charts
#   ggtitle("Histogram for nr_pix in digits dataset") +
#   theme_bw() 
#   
# 
# dig_nr_pix__hist
# #ggsave('digit_nr_pix__histogram.png')
# 
# 
# #Now to repeat te code to visualise the probability distribution of the letters dataset
# lettersoutcomes <- sort(unique(lettersdf$nr_pix))
# lettersoutcomes
# 
# probs1 = c() 
# for (outcome in lettersoutcomes) {
#   probs1 <- append(probs, sum(lettersdf$nr_pix ==outcome)/nrow(lettersdf))
# }
# probs1
# 
# letters_prob_dist <-cbind(lettersoutcomes, probs1)
# colnames(letters_prob_dist) <- c('Outcome', 'Probability')
# letters_prob_dist <- as.data.frame(letters_prob_dist)
# 
# rletters_prob_dist <- data.frame((rletters_prob_dist))
# rletters_prob_dist <- round(rletters_prob_dist, 3)
# write.table(rletters_prob_dist, sep=",", "Probability Distribution table for Number of Pixels in Letters dataset.csv",row.names=FALSE, col.names = FALSE)
# 
# l_probability_dist <- ggplot(letters_prob_dist, aes(x=Outcome , y=Probability)) +
#   geom_histogram(stat = "identity",color="orchid", fill="darkorchid") +
#   geom_smooth(se = FALSE) +
#     #geom_vline(aes(xintercept=mean(Outcome)),
#      #      color="orange", linetype="dashed", size=1)
#   ggtitle("Histogram for number of pixels in letters dataset") +
#   theme_bw()
# l_probability_dist
# #ggsave("Histogram for number of pixels in letters dataset.png")
# 
# #Distribution looks multimodal(?) and right skewed. Has a couple of outliers > 50.
# #Maybe you can go into detail as to why this and the other three nr_pix histograms do
# #not suggest a normal distribution. Outliers ar too common and far out?
# #Changed bin width to 2 because it gave a better visualisation of the data
# 
# 
# 
# mathoutcomes <- sort(unique(mathdf$nr_pix))
# mathoutcomes
# 
# probs2 = c() 
# 
# for (outcome in mathoutcomes) {
#   probs2 <- append(probs2, sum(mathdf$nr_pix ==outcome)/nrow(mathdf))
# }
# probs2
# 
# math_prob_dist <-cbind(mathoutcomes, probs2)
# colnames(math_prob_dist) <- c('Outcome', 'Probability')
# math_prob_dist <- as.data.frame(math_prob_dist)
# 
# rmath_prob_dist <- data.frame(t(math_prob_dist))
# rmath_prob_dist <- round(rmath_prob_dist, 3)
# #write.table(rmath_prob_dist, sep=",","Probability Distribution table for Number of Pixels in Math dataset.csv",col.names=FALSE)
# 
# m_probability_dist <- ggplot(math_prob_dist, aes(x=Outcome, y=Probability)) +
#   geom_histogram(stat = "identity", color="darkseagreen4", fill="darkseagreen1") +
#   geom_smooth(se = FALSE) +
#   ggtitle("Histogram for number of pixels in math dataset") +
#   theme_bw()
#   
# 
# m_probability_dist
# #distribution looks bimodal and slightly right skewed - although leaning more towards
# #uniform than other two symbol groups.
# #ggsave('math_nr_pix__histogram.png')

#Below is a plot showing the probability density of all three types of dataseets.
#with each of their respective means plotted with a dashed line
#http://rstudio-pubs-static.s3.amazonaws.com/374857_5a23bad9783a43c1b102aa80aa5c1a7c.html
library(plyr)
means <- ddply(features, "Category", summarise, nr_pix.mean = mean(nr_pix))

density <- ggplot(features, aes(x = nr_pix, fill = Category)) +
  geom_density(alpha = .5) + #alpha used for filling the density
  geom_vline(data = means, aes(xintercept = nr_pix.mean, colour = Category),
             linetype = "longdash", size=1) +
  #facet_wrap( ~ Category) + # For separate probability density graphs
  ggtitle("Probability Density of Number of Pixels in Dataset by Symbol Categoory") 


density
#ggsave('separate overall_nr_pix_probability_density.png')





#2: Suppose you randomly sample a digit from the set of digits. What is the probability that the number of pixels in the
#image is greater than 20?
print("Within my dataset it is certain that a digit with more than 20 black pixels
will be selected, as we can see by my histogram - the smallest value for nr_pix is 26. So the probability is 1")


# 3. Present summary statistics (e.g. mean and standard deviation) about all the features, for (a)
# the full set of 168 items, (b) the 56 digits, (b) the 56 letters, (c) the 56 math symbols. Briefly
# discuss the summary statistics, and whether they already suggest which features may be
# useful for discriminating digits and letters. For features you feel may be interesting, consider
# suitable visualisations (e.g. histogram of feature values for the three groups)

# (a) the full set of 168 items
#https://stackoverflow.com/questions/20997380/creating-a-summary-statistical-table-from-a-data-frame
library(fBasics)

#featuresSummary <- basicStats(features[, 3:19])[c("Mean", "Stdev","Variance", "Median", "Minimum", "Maximum","Skewness", "1. Quartile", "3. Quartile"),]

# I had the above for calculating all of these summary statistics for the features in my dataset, but the output table was too much,
# it was too verbose. So I found that I could do a boxplot for each category and by using the melt function I could visualise the 
# Minimum, Maximum, 1st Quartile, Median, 3rd Quartile, IQR, as well as potential outliers far better than if they were on a table

#Code below is adding summary stats not included in fbasic package but I think are useful
#IQR <- c(featuresSummary['3. Quartile',] - featuresSummary['1. Quartile',])
#featuresSummary <- rbind(featuresSummary, IQR)
#rownames(featuresSummary)[10]<-"IQR"
featuresSummary <- basicStats(features[, 3:19])[c("Mean", "Stdev","Variance", "Skewness"),]
featuresSummary <- round(featuresSummary, 2) # Round numbers so it looks cleaner
#write.csv(featuresSummary, "Summary table of all of the features.csv")

digitsSummary <- basicStats(features[1:56, 3:19])[c("Mean", "Stdev","Variance","Skewness"),]
digitsSummary <- round(digitsSummary, 2)
#write.csv(digitsSummary, "Summary table of the features for the digits dataset.csv")

lettersSummary <- basicStats(lettersdf[,3:19])[c("Mean", "Stdev","Variance", "Skewness"),]
lettersSummary <- round(lettersSummary, 2) 
#write.csv(lettersSummary, "Summary table of the features for the letters dataset.csv")

mathSummary <- basicStats(mathdf[, 3:19])[c("Mean", "Stdev","Variance","Skewness"),]
mathSummary <- round(mathSummary, 2)
#write.csv(mathSummary, "Summary table of the features for the math dataset.csv")

#With these summary tables, they show that the skewness for all of my data never exceeds -3 or 3.
#This, along with these written symbols of course being under 10% of written symbols in the population - showing they're independent of each other
#Means that I can assume the relative normality of my feature data along with it's independency of each other. 
# I can thus use hypothesis testing with confidence. No log transorm necessary for extremely skewed data




#Below is a histogram plot method I used to visualise the difference in feature  
#values for different groups of symbols on DIFFERENT AXIS/FACETS. To measure a variable, simply change the x value in aes()
diff <- ggplot(features, aes(x=factor(nr_regions, order = FALSE), fill = Category)) +
  geom_bar(position="identity") +
  xlab("Number of Regions in Symbol") +
  ylab("Count") +
  facet_grid(Category ~ .) +
  ggtitle("Histogram plots showing the difference in Number of Regions for each type of symbol in the dataset") +
  theme_bw()

diff

#ggsave("Histogram plots showing the difference in nr_regions for each type of symbol in the dataset.png")

#Below is the code I used to create my boxplot for each feature of the dataframe
#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
#install.packages("reshape") #Need this package installed for the melt() function
library(reshape)

features.m <- melt(features[, 3:20], id.var = "Category")
summarybox <- ggplot(data = features.m, aes(x=Category, y=value)) +
  geom_boxplot(aes(fill = Category)) +
  geom_point(aes(y=value, group=Category), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  ggtitle("Overall Boxplot for Feature Data")

summarybox
#ggsave("Overall Boxplot for feature data.png")

# #Plots strangely, maybe come back to it
# summaryhist <- ggplot(data = features.m, aes(x=variable, y = value)) +
#   geom_histogram(stat = "identity", position = "dodge", aes(fill = Category)) +
#   facet_wrap( ~ variable, scales="free") +
#   theme_classic() +
#   ggtitle("Overall Histogram for feature data")
# 
# summaryhist





# 4. Are there pairs of features which are highly associated with each other, and thus provide little
# extra information with respect to having only one of them in the data? Can you discard some
# features from your data set without losing much information? Justify your claims.


#Below is the code I used to create a point geom_point and geom_smooth graph demonstrating the assocation between two variables
aasoc <- ggplot(features, aes(x=no_neighbours_below, y=no_neighbours_above, color = Category)) +
  geom_point(stat = "identity", alpha = 0.6 ) +
  geom_smooth(se = FALSE) +
  ggtitle("Association between no neighbours above feature and no neighbours below feature") +
  theme_bw()

aasoc

#Below is the code I used to compare features based on their respective boxplots
association.m <- melt(features[,c("no_neighbours_before","no_neighbours_after","Category")], id.var = "Category")
assocbox <- ggplot(data = association.m, aes(y=value, x = Category)) +
  geom_boxplot(aes(fill = Category)) +
  facet_wrap( ~ variable) +
  ggtitle("Boxplots of the no_neighbours_below and no_neighbours_above features", 
          subtitle = "Detailing the difference between the features") 
assocbox 

#ggsave("Boxplots of the no_neighbours_before and no_neighbours_after features.png")





# 5. Are there features which are useful to discriminate between the 7 different letters? (Note that
# here we are looking for differences between 7 different groups, so consider a statistical
# method that tests for statistically significant differences between more than 2 groups).

# I thought that the last group visualisation of the boxplots was very useful in 
# discriminating between different, broad groups of symbols in the entire features dataset...
# I could use it again to summarise the differences between different groups of letters within the lettersdf dataset

testFeatures = column_names[3:19]
testFeatures[18] <- "label"
testFeatures

lettersdf.m <- melt(lettersdf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
lettersdf.m$label<- as.factor(lettersdf.m$label)
#Convert labels to categorical factors so we can use them in boxplot
lettersSummarybox <- ggplot(data = lettersdf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Overall Boxplot for Letters data\n", y = "Values") 

lettersSummarybox
#ggsave("Feature summary for letters dataset - boxplot.png")



#http://rstudio-pubs-static.s3.amazonaws.com/1204_67621a69f1dc465f81de9716ec063742.html
aov_letters <- lapply(lettersdf[,3:19], function(x) anova(lm(x ~ lettersdf$label)))
#Above returns a list of nested ANOVA dataframe results for each feature in letters dataframe
aov_letters

# The below code separates out the stats we want in each aov, namely the P value and the associated stat
#This anova test is merely a preliminary test. If a feature's P-value is below our alpha, this does not mean it is a significant discriminator
#for all groups of letters, but rather it is significant in telling apart at least one group from the others.
featuretemp <- c()
pvals <- c()
for (i in 1:17) {
  index <- names(aov_letters)[i]
  pval1 <- aov_letters[[index]][["Pr(>F)"]][1]
  
  if( pval1 < 0.05) { 
 
   featuretemp <- c(featuretemp, index)
  pvals <- c(pvals, pval1)
  
}
}
#Combine the newly created corresponding vectors into a dataframe where they can be read with more ease
lettersstatsignif <-cbind(featuretemp, pvals)
colnames(lettersstatsignif) <- c('Feature', 'P Value')
lettersstatsignif <- as.data.frame((lettersstatsignif))
lettersstatsignif
#write.table(lettersstatsignif, sep=",","Significant stats to discriminate between letters.csv",row.names=FALSE)

library(dplyr)

# 6. Are there features which are useful to discriminate between the 7 different digits? (Note that
# here we are looking for differences between 7 different groups, so consider a statistical
# method that tests for statistically significant differences between more than 2 groups).

digitdf.m <- melt(digitdf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
digitdf.m$label<- as.factor(digitdf.m$label) #Convert labels to categorical factors so we can use them in boxplot

digitsSummarybox <- ggplot(data = digitdf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Overall Boxplot for Digits data\n", y = "Values") 


digitsSummarybox
#ggsave("Overall Boxplot for digits data.png")

#I'll do a likewise ANOVA for the digits dataset
aov_digits <- lapply(digitdf[,3:19], function(x) anova(lm(x ~ digitdf$label)))
aov_digits


featuretemp <- c()
pvals <- c()
for (i in 1:17) {
  index <- names(aov_digits)[i]
  pval1 <- aov_digits[[index]][["Pr(>F)"]][1]
  
  if( pval1 < 0.05) { 
   
    featuretemp <- c(featuretemp, index)
    pvals <- c(pvals, pval1)
  }
}
#Combine the newly created corresponding vectors into a dataframe where they can be read with more ease
digitstatsignif <-cbind(featuretemp, pvals)
colnames(digitstatsignif) <- c('Feature', 'P Value')
digitstatsignif <- as.data.frame((digitstatsignif))
digitstatsignif
#write.table(digitstatsignif, sep=",","Significant stats to discriminate between digits.csv",row.names=FALSE)





# 7. Are there features which are useful to discriminate between the three groups? How confident
# are you about your findings?
#Recall earlier that I created a boxplot showing the possible differences between groups features:
summarybox

#I'll also create a probability density graph to illustrate possible differences

summarydensity <- ggplot(data = features.m, aes(x = value, fill = "Category")) +
  geom_density(alpha = .5 ) +
  facet_wrap( ~ variable, scales="free") +
  ggtitle("Overall probability density for each feature in dataset")

summarydensity

#Also the summary density shows the relatively normal distribution of most of my features
#ggsave("Probability Density for all features in math dataset.png")
features_aov <- lapply(features[,3:19], function(x) anova(lm(x ~ features$Category)))
features_aov

featuretemp1 <- c()
pvals1 <- c()
for (i in 1:17) {
  index <- names(features_aov)[i]
  pval <- features_aov[[index]][["Pr(>F)"]][1] 
  
  if(is.na(pval)) next
  if (pval < 0.05){  
  
  featuretemp1 <- c(featuretemp1, index) 
  pvals1 <- c(pvals1, pval)
  }
}
featuresstatsignif<-cbind(featuretemp1, pvals1)
colnames(featuresstatsignif) <- c('Features', 'P Value')
featuresstatsignif <- as.data.frame((featuresstatsignif))
featuresstatsignif
#write.table(featuresstatsignif, sep=",","Significant stats to discriminate between features.csv",row.names=FALSE)





# 8. Are there features which are useful to discriminate between the set of digits and the set of
# letters? (Consider statistical tests which test for differences between two groups).

lettersdigitsdf <- rbind(digitdf, lettersdf)

lettersdigitsdf.m <- melt(lettersdigitsdf[,3:20], id.var = "Category") # Exclude Category & Index - redundant here
lettersdigitsdf.m$Category<- as.factor(lettersdigitsdf.m$Category) #Convert labels to categorical factors so we can use them in boxplot

lettersdigitsSummarybox <- ggplot(data = lettersdigitsdf.m, aes(x=Category, y=value)) +
  geom_boxplot(aes(fill = Category)) +
  geom_point(aes(y=value, group=Category), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Comparing Letters and Digits with Boxplots\n", y = "Values") 

lettersdigitsSummarybox
#ggsave("Comparing Letters and Digits with Boxplots.png")

# aov_letters_digits <- lapply(lettersdigitsdf[,3:19], function(x) TukeyHSD(aov(x ~ lettersdigitsdf$Category), 
#                         conf.level = 0.95)[1][["lettersdigitsdf$Category"]])


#Originally I used above to compare the two groups but I later realised that the t.test 
#was more appropriate as it is only two groups and there are far less comparisons. Tukey HSD is more warranted when comparing
#all possible pairwise comparisons, whereas here I'm just comparing the difference in two groups


lettersdigitsdf$Category <- as.factor(lettersdigitsdf$Category)
# pairwise_letters_digits <-lapply(lettersdigitsdf[,3:19], function(x) pairwise.t.test(x = x,
#                                     g = lettersdigitsdf$Category, data=lettersdigitsdf, p.adj = "bonferroni"))

#Also, I had used the pairwise.t.test but I later learned that the normal t.test was perhaps more suitable
#as it didn't require equal variability among samples like the pairwise.t.test did. Also the normal t.test returned more information

t_test_letters_digits <-lapply(lettersdigitsdf[,3:19], function(x) t.test(x ~ lettersdigitsdf$Category, paired = TRUE))
t_test_letters_digits

typeoffeature <- c()
conf_intv <- c()
mean_diff <- c()
pvalue <- c()
for (i in 1:17) {
  index <- names(t_test_letters_digits)[i]
  pval5 <- t_test_letters_digits[[index]][[3]]
  # print(index)
  # print(pval5)
  num1 <- t_test_letters_digits[[index]][[4]][1]
  num1 <- round(num1, digits = 2)
  
  num2 <- t_test_letters_digits[[index]][[4]][2]
  num2 <- round(num2, digits = 2)
  
  tempconf <- c(as.character(num1), as.character(num2))
  #convert CI to character then add as vector to string to keep two numerical values in same column
  tempconf <- paste(tempconf, collapse = " : ")
  
  conf <- tempconf
  meandif <- t_test_letters_digits[[index]][[5]][[1]]
  if(is.na(pval5)) next # Control for possible NA values which can throw an error and break loop
  if(pval5 < 0.05) { # Want a 95% confidence interval. Throws up an error for some iterations because pval is NULL
    
    typeoffeature <- c(typeoffeature, index)
    conf_intv <- c(conf_intv, conf)
    mean_diff <- c(mean_diff, meandif)
    pvalue <- c(pvalue, pval5)
  }
  
}
lettersdigitssigstats<-cbind(typeoffeature, conf_intv, mean_diff, pvalue)
colnames(lettersdigitssigstats) <- c('Features', 'Confidence interval for Difference in Means', 'Mean Difference', 'P Value')
lettersdigitssigstats <- as.data.frame((lettersdigitssigstats))
lettersdigitssigstats
#write.table(lettersdigitssigstats, sep=",","Significant stats to discriminate between Letters and Digits.csv",row.names=FALSE)


?t.test()





# 9. Are there features which are useful to discriminate between the digit "1" and the digit "7"?
# Briefly interpret your findings.


oneandsevendf <- digitdf %>%
  dplyr::filter(label == 1 |
                  label == 7)

#Use pipe operation of dplyr to filter out desired results from table

oneandsevendf
oneandsevendf$label <- as.factor(oneandsevendf$label)


oneandsevendf.m <- melt(oneandsevendf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
oneandsevendf.m$label<- as.factor(oneandsevendf.m$label)


#Again creating a box plot to show the differences in the values
oneandsevenbox <- ggplot(data = oneandsevendf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Comparing Feature values of 1 and 7\n", y = "Values", x = "Labels") 

oneandsevenbox
#ggsave("Comparing Feature values of 1 and 7.png")

#I chose to do the standard t.test here rather than the pairwise.t.test. I don't think multiple comparison correction is too
#necessary and also the standard t.test function returns far more information that could be useful. 
#Especially when pinpointing a potential difference

oneSeventtest <- lapply(oneandsevendf[,3:19], function(x) t.test(x ~ oneandsevendf$label,
                                  var.equal = TRUE, paired = TRUE))


typeoffeature <- c()
conf_intv <- c()
mean_diff <- c()
pvalue <- c()
for (i in 1:17) {
  index <- names(oneSeventtest)[i]
  pval5 <- oneSeventtest[[index]][[3]]
  # print(index)
  # print(pval5)
  num1 <- oneSeventtest[[index]][[4]][1]
  num1 <- round(num1, digits = 2)

  num2 <- oneSeventtest[[index]][[4]][2]
  num2 <- round(num2, digits = 2)

  tempconf <- c(as.character(num1), as.character(num2))
  #convert CI to character then add as vector to string to keep two numerical values in same column
  tempconf <- paste(tempconf, collapse = " : ")

   conf <- tempconf
   meandif <- oneSeventtest[[index]][[5]][[1]]
  if(is.na(pval5)) next # Control for possible NA values which can throw an error and break loop
  if(pval5 < 0.05) { # Want a 95% confidence interval. Throws up an error for some iterations because pval is NULL

    typeoffeature <- c(typeoffeature, index)
    conf_intv <- c(conf_intv, conf)
    mean_diff <- c(mean_diff, meandif)
    pvalue <- c(pvalue, pval5)
  }
  
}
oneandsevencomp<-cbind(typeoffeature, conf_intv, mean_diff, pvalue)
colnames(oneandsevencomp) <- c('Feature', 'Confidence interval for Difference in Means',  "Mean Difference", 'P_Value')
oneandsevencomp <- as.data.frame((oneandsevencomp))
oneandsevencomp
#write.table(oneandsevencomp,sep = ",", "Table of P values comparing features of 1 and 7.csv",row.names = FALSE )






# 10. Are there features which are useful to discriminate between the letter "b" and the letter "d"?
# Briefly interpret your findings.

BDdf <- lettersdf %>%
  dplyr::filter(label == 'b' |
                  label == 'd')
BDdf

BDdf.m <- melt(BDdf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
BDdf.m$label<- as.factor(BDdf.m$label)


#Again creating a box plot to show the differences in the values
BDbox <- ggplot(data = BDdf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Comparing Feature values of b and d\n", y = "Values", x = "Labels") 

BDbox
#ggsave("Comparing Feature values of B and D.png")

bdttest <- lapply(BDdf[,3:19], function(x) t.test(x ~ BDdf$label,
                var.equal = TRUE, paired = TRUE))


typeoffeature <- c()
conf_intv <- c()
mean_diff <- c()
pvalue <- c()
for (i in 1:17) {
  index <- names(bdttest)[i]
  pval5 <- bdttest[[index]][[3]]
  num1 <- bdttest[[index]][[4]][1]
  num1 <- round(num1, digits = 2)
  num2 <- bdttest[[index]][[4]][2] 
  num2 <- round(num2, digits = 2)
  tempconf <- c(as.character(num1), as.character(num2))
  #convert CI to character then add as vector to string to keep two numerical values in same column
  tempconf <- paste(tempconf, collapse = " : ")
  
  conf <- tempconf
  meandif <- bdttest[[index]][[5]][[1]]
  if(is.na(pval5)) next
  if(pval5 < 0.05) { 
    
    typeoffeature <- c(typeoffeature, index)
    conf_intv <- c(conf_intv, conf)
    mean_diff <- c(mean_diff, meandif)
    pvalue <- c(pvalue, pval5)
  }
  
}
bdcompare<-cbind(typeoffeature, conf_intv, mean_diff, pvalue)
colnames(bdcompare) <- c('Feature', 'Confidence interval for Difference in Means',  "Mean Difference", 'P_Value')
bdcompare <- as.data.frame((bdcompare))
bdcompare
#write.table(bdcompare,sep = ",", "Table of P values comparing features of b and d.csv",row.names = FALSE )





# 11. Are there features which are useful to discriminate between the symbol "=" and the symbol
# "not=" Briefly interpret your findings.

equalsdf <- mathdf %>%
  dplyr::filter(label == '=' |
                  label == 'not=')

equalsdf

equalsdf$label <- as.factor(equalsdf$label)
                            
                            

equalsdf.m <- melt(equalsdf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
equalsdf.m$label<- as.factor(equalsdf.m$label)


#Again creating a box plot to show the differences in the values
equalsbox <- ggplot(data = equalsdf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Comparing Feature values of = and not=\n", y = "Values", x = "Labels") 

equalsbox

#ggsave("Comparing Feature values of = and not=.png")

#The normal t.test was not working. Could not diagnose the error so I used the pairwise t.test method to get p values

equalsttest <- lapply(equalsdf[,3:19], function(x) pairwise.t.test(x = x, 
                              g = equalsdf$label, data = equalsdf, paired = FALSE, na.action=na.omit))

# equalsttest <- lapply(equalsttest[,3:19], function(x) t.test(x ~ equalsdf$label,
#                                       var.equal = TRUE, paired = FALSE))

#Above throws up error: incorrect number of dimensions
equalsttest
?pairwise.t.test()
featuretemp <- c()
pvals <- c()
for (i in 1:17) {
  index <- names(equalsttest)[i]
  pval2 <- equalsttest[[index]][[3]][1]
  
  if(is.na(pval2)) next
  if(pval2 < 0.05) {

    featuretemp <- c(featuretemp, index)
    pvals <- c(pvals, pval2)
  }
  
}
featuretemp
equalsStats<-cbind(featuretemp, pvals)
colnames(equalsStats) <- c('Features', 'P Value')
equalsStats <- as.data.frame((equalsStats))
equalsStats
#write.table(equalsStats, sep = ",", "Table of P values comparing features of = and not=.csv",row.names = FALSE )




# 12. For each feature, find the pairs of digits (if any) that have a statistically significant difference
# for that feature (carefully consider multiple comparison correction).
#Remember the letters summary boxplot
digitsSummarybox
pairsofdigitstukey <- lapply(digitdf[,3:19], function(x) TukeyHSD(aov(x ~ digitdf$label), 
                                                            conf.level = 0.98)[1][["digitdf$label"]])
#Adjusted the confidence level in the function to 98% to account for the multiple comparisons
pairsofdigitstukey


comparisonv <- c()
pvalsv <- c()
typeoffeaturev <- c()
for (i in 1:17) {
  index <- names(pairsofdigitstukey)[i]
  nrpvals <- pairsofdigitstukey[[index]][,4]
  comparisons <- names(nrpvals)
  pvals4 <- unname(nrpvals)
  for(j in 1:21) { # nested for loop to deal with all possible comparisons
    num <- pvals4[j]
    if(is.na(num)) next
    if(num < 0.05) { 
      
      comparisonv <- c(comparisonv, comparisons[j])
      pvalsv <- c(pvalsv, pvals4[j])
      typeoffeaturev <- c(typeoffeaturev, index)
    }
  }
}
pairsofdigitsdf<-cbind(typeoffeaturev, comparisonv, pvalsv)
colnames(pairsofdigitsdf) <- c('Feature', "Comparison", 'P_Value')
pairsofdigitsdf <- as.data.frame((pairsofdigitsdf))


pairsofdigitsdf$Comparison <- paste(pairsofdigitsdf$Comparison, ".")
# AbovePrevents excel automtically converting comparison to date (dd-m)

#write.table(pairsofdigitsdf, sep = ",", "Table of all statistically significant pairs for each feature in digits subset.csv",row.names = FALSE )




# 13. For each feature, find the pairs of letters (if any) that have a statistically significant difference
# for that feature (carefully consider multiple comparison correction).


#lettersdf$label <- as.factor(lettersdf$label)

#Remember the letters summary boxplot
lettersSummarybox
pairsofletterstukey <- lapply(lettersdf[,3:19], function(x) TukeyHSD(aov(x ~ lettersdf$label), 
                                                conf.level = 0.98)[1][["lettersdf$label"]])
pairsofletterstukey
comparisonv <- c()
pvalsv <- c()
typeoffeaturev <- c()
for (i in 1:17) {
  index <- names(pairsofletterstukey)[i]
  nrpvals <- pairsofletterstukey[[index]][,4]
  comparisons <- names(nrpvals)
  pvals4 <- unname(nrpvals)
  for(j in 1:21) { # nested for loop to deal with all possible comparisons
    num <- pvals4[j]
    #print(num)
  if(is.na(num)) next
  if(num < 0.05) {

  comparisonv <- c(comparisonv, comparisons[j])
  pvalsv <- c(pvalsv, pvals4[j])
  typeoffeaturev <- c(typeoffeaturev, index)
}
  }
}
pairsoflettersdf<-cbind(typeoffeaturev, comparisonv, pvalsv)
colnames(pairsoflettersdf) <- c('Feature', "Comparison", 'P_Value')
pairsoflettersdf <- as.data.frame((pairsoflettersdf))

#write.table(pairsoflettersdf, sep = ",", "Table of all statistically significant pairs for each feature in digits subset.csv",row.names = FALSE )


#pairwise.t.test(lettersdf$nr_pix, lettersdf$label,  data=df, p.adj = "bonferroni")
#TukeyHSD(aov(lettersdf$nr_pix ~ lettersdf$label))
#Comparing the results of above the p value returned are quite similar most of the time




# 14. For each feature, find the pairs of math symbols (if any) that have a statistically significant
# difference for that feature (carefully consider multiple comparison correction).

mathdf.m <- melt(mathdf[,testFeatures], id.var = "label") # Exclude Category & Index - redundant here
mathdf.m$label<- as.factor(mathdf.m$label) #Convert labels to categorical factors so we can use them in boxplot

mathSummarybox <- ggplot(data = mathdf.m, aes(x=label, y=value)) +
  geom_boxplot(aes(fill = label)) +
  geom_point(aes(y=value, group=label), position = position_dodge(width=0.75)) +
  facet_wrap( ~ variable, scales="free") +
  theme_classic() +
  labs(title = "Overall Boxplot for Math data\n", y = "Values") 

mathSummarybox
#ggsave("Boxplot for Math data.png")

pairsofmathtukey <- lapply(mathdf[,3:19], function(x) TukeyHSD(aov(x ~ mathdf$label), 
                                            conf.level = 0.98)[1][["mathdf$label"]])
pairsofmathtukey
comparisonv <- c()
pvalsv <- c()
typeoffeaturev <- c()
for (i in 1:17) {
  index <- names(pairsofmathtukey)[i]
  nrpvals <- pairsofmathtukey[[index]][,4]
  comparisons <- names(nrpvals)
  pvals4 <- unname(nrpvals)
  for(j in 1:21) { # nested for loop to deal with all possible comparisons
    num <- pvals4[j]
    #print(num)
    if(is.na(num)) next
    if(num < 0.05) {
      
      comparisonv <- c(comparisonv, comparisons[j])
      pvalsv <- c(pvalsv, pvals4[j])
      typeoffeaturev <- c(typeoffeaturev, index)
    }
  }
}
pairsofmathdf<-cbind(typeoffeaturev, comparisonv, pvalsv)
colnames(pairsofmathdf) <- c('Feature', "Comparison", 'P_Value')
pairsofmathdf <- as.data.frame((pairsofmathdf))
pairsofmathdf
#write.table(pairsofmathdf, sep = ",", "Table of all statistically significant pairs for each feature in Math subset.csv",row.names = FALSE )





# 15. Using the best feature that you have discovered for discriminating between digits and letters,
# find a suitable threshold value for that feature to predict digit/letter in the best possible way
# (such that values to one side of that threshold are associated with digits and values to the
# other side of the threshold are associated with letters). How accurate is this simple thresholdbased
# classifier?


# 16. Using combinations of simple thresholds for certain features, how accurately can you
# discriminate between the three groups?
  

