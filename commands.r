###############################################################################
#                              NECESSARY STEPS
###############################################################################
## Import libraries
library(Hmisc)
library(ggplot2)

## Import the data
physData = read.csv("EpomophorusPhysiology_2015.csv");


## Data preprocessing
# First, change the ID column to be a rowname, since rowname does not participate in any of the mathematical operations
rownames(physData) = physData$ID;  # Sets the rownames of the data frame, physData, to be the ID column of physdata
physData$ID = NULL;                # Deletes the ID row of physData by setting it to NULL
physData$sex = as.factor( ifelse( physData$class == "M_NS", "male", "female") );
physData$disturbed = ifelse(physData$Habitat == "disturbed", 1, 0);
physData$sexC = ifelse(physData$sex == "male", 1, 0);


# Next, keep track of all of the variable names, except for Habitat and class
numericCols = colnames(physData);
numericCols = numericCols[ ! (numericCols %in% c("class", "Habitat", "sex") ) ];


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


###############################################################################
#                        CORRELATION/SUMMARY STATISTICS
###############################################################################
## Looking at linear correlation
rcorr( as.matrix( physData[, numericCols] ) )


## Summary statistics
summary( physData[, numericCols] )
apply( physData[, numericCols], 2, sd)


###############################################################################
#                  LINEAR REGRESSION (All individuals)
###############################################################################
# Example:  NA and BUN are linearly correlated in the general population
# First, train the linear model
physLm = lm( Na ~ BUN, physData )

# Plots the raw data in the formula
plot(Na ~ BUN, physData, main="Na vs BUN against all individuals" )
abline(physLm, col="black")

############ Working copy, without comments ############ 
physLm = lm( BUN ~ Cl, physData )
plot(BUN ~ Cl, physData, main="BUN vs Cl against all individuals" )
abline(physLm, col="black")



###############################################################################
#                    LINEAR REGRESSION (Male vs Female)
###############################################################################
# Do the different linear models
thisForm = "disturbed ~ Beecf";
everyone.lm    = lm( as.formula(thisForm), physData );
males.lm       = lm( as.formula(thisForm), physData[ physData$sex == "male", ] );
females.lm     = lm( as.formula(thisForm), physData[ physData$sex == "female", ]  );
disturbed.lm   = lm( as.formula(thisForm), physData[ physData$Habitat == "disturbed", ]  );
undisturbed.lm = lm( as.formula(thisForm), physData[ physData$Habitat == "undisturbed", ]  );

plot(PCO2 ~ pH, physData, main=thisForm, col=ifelse(physData$sex == "male", "blue", "red") )
abline(everyone.lm, col="black")
abline(males.lm,    col="blue")
abline(females.lm,  col="red")


plot(as.formula(thisForm), physData, main=thisForm, col=ifelse(physData$Habitat == "undisturbed", "blue", "red") )
abline(everyone.lm, col="black")
abline(undisturbed.lm,    col="blue")
abline(disturbed.lm,  col="red")

###############################################################################
#                         Multiple Regression
###############################################################################
lm1DF = data.frame(dependent=NULL, independent=NULL, rsquared=NULL, P=NULL, multiR=NULL);

thisForm_all  = as.formula("disturbed ~ BMI + Na + K + Cl + TCO2 + BUN + Glu + Hct + pH + PCO2 + HCO3 + Beecf + AnGap + Hb_star");

thisForm      = as.formula("disturbed ~ Beecf");
glm.out = glm( thisForm, family=binomial(logit), data=physData);


add1(glm.out, thisForm_all ,test="Chisq")


thisForm = as.formula("disturbed ~ Beecf + HCO3");
glm.out = glm( thisForm, family=binomial(logit), data=physData);
summary(glm.out)


# Stepwise search; STRONGLY suggests that best fit for logistic regression is 3 or 2 variables.  So we're good!
library(glmnet);
glmmod = glmnet( x=as.matrix(physData[, numericCols]), y=physData$disturbed, alpha=1, family='binomial', nlambda=1000)
cv.glmmod = cv.glmnet( x=as.matrix(physData[, numericCols]), y=physData$disturbed, alpha=1)
plot( cv.glmmod )


###############################################################################
#                    BOX AND WHISKER PLOTS (Male vs Female)
###############################################################################
ggplot(physData, aes_string(x="sex", y="Glu")) +
  ggtitle(sprintf("%s of male and female bats","Glucose")) +
  geom_boxplot(outlier.size=0, alpha=0.2) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(size=rel(2.5), face="bold")) +
  theme(axis.title = element_text(size=rel(2), face="bold")) +
  theme(axis.text  = element_text(size=rel(1.2), face="bold", colour="blue")) +
  theme(axis.ticks = element_line(size=rel(1.2), colour = "black"))


###############################################################################
#                    BOX AND WHISKER PLOTS (Disturbed and Undisturbed)
###############################################################################
# Only need to change this line.
thisVar = "Hb_star";
ggplot(physData, aes_string(x="Habitat", y=thisVar)) +
  ggtitle(sprintf("Hb* of bats from different habitats",thisVar)) +
  geom_boxplot(outlier.size=0, alpha=0.2) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.title = element_text(size=rel(2.5), face="bold")) +
  theme(axis.title = element_text(size=rel(2), face="bold")) +
  theme(axis.text  = element_text(size=rel(1.2), face="bold", colour="blue")) +
  theme(axis.ticks = element_line(size=rel(1.2), colour = "black"))


###############################################################################
#                   BAR CHART WITH STANDARD ERROR (Male vs Female)
###############################################################################
# Only need to change this line.
plotVars = numericCols;
for( thisVar in plotVars ) {

thisData = summarySE(physData, measurevar=thisVar, groupvars=c("sex","Habitat"))
thisData$quant = thisData[, thisVar];
theseBreaks = seq( min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se), length.out=6);
theseLabels = sprintf("%.2f",theseBreaks );

ggplot(thisData, aes(x=sex, y=quant, fill=Habitat)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes( ymin=quant-se, ymax=quant+se ),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Sex") +
  ylab(thisVar) +
  scale_fill_hue(name="Habitat type", # Legend label, use darker colors
                 breaks=c("disturbed", "undisturbed"),
                 labels=c("Disturbed", "Undisturbed")) +
  ggtitle(paste("Mean ", thisVar, " with standard error", sep="") ) +
  scale_y_continuous(breaks=theseBreaks, labels=theseLabels  ) +
  coord_cartesian(ylim=c(min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se) ) ) +
  theme_bw() +
  theme(plot.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.text    = element_text(size=rel(1), face="bold")) +
  theme(axis.ticks   = element_line(size=rel(1), colour = "black")) +
  theme(legend.title = element_text(size=rel(2))) +
  theme(legend.text  = element_text(size=rel(2)));

ggsave(filename=paste("C:\\Users\\dasanchez\\dev\\batStats\\batStats\\sex_disturbance\\", thisVar, "_mf_distundist.png",sep=""),height=10,width=10);

}



###############################################################################
#                   BAR CHART WITH STANDARD ERROR (Disturbed vs Undisturbed)
###############################################################################
# Only need to change this line.
thisVar = "BUN";
thisData = summarySE(physData, measurevar=thisVar, groupvars=c("sex","Habitat"))
thisData$quant = thisData[, thisVar];
theseBreaks = seq( min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se), length.out=6);
theseLabels = sprintf("%.2f",theseBreaks );

ggplot(thisData, aes(x=Habitat, y=quant, fill=sex)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes( ymin=quant-se, ymax=quant+se ),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Sex") +
  ylab(thisVar) +
  scale_fill_hue(name="Sex", # Legend label, use darker colors
                 breaks=c("male", "female"),
                 labels=c("Male", "Female")) +
  ggtitle(paste("Mean ", thisVar, " with standard error", sep="") ) +
  scale_y_continuous(breaks=theseBreaks, labels=theseLabels  ) +
  coord_cartesian(ylim=c(min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se) ) ) +
  theme_bw() +
  theme(plot.title   = element_text(size=rel(5), face="bold")) +
  theme(axis.title   = element_text(size=rel(3), face="bold")) +
  theme(axis.text    = element_text(size=rel(2), face="bold", colour="blue")) +
  theme(axis.ticks   = element_line(size=rel(2), colour = "black")) +
  theme(legend.title = element_text(size=rel(3))) +
  theme(legend.text  = element_text(size=rel(2)))


###############################################################################
#                   BAR CHART WITH STANDARD ERROR (ONLY male v.s. female)
###############################################################################
# Only need to change this line.
thisVar = "Cl";
thisData = summarySE(physData, measurevar=thisVar, groupvars=c("sex"))
thisData$quant = thisData[, thisVar];
theseBreaks = seq( min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se), length.out=6);
theseLabels = sprintf("%.2f",theseBreaks );

ggplot(thisData, aes(x=sex, y=quant, fill=sex)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes( ymin=quant-se, ymax=quant+se ),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Sex") +
  ylab(thisVar) +
  scale_fill_hue(name="Sex", # Legend label, use darker colors
                 breaks=c("male", "female"),
                 labels=c("Male", "Female")) +
  ggtitle(paste("Mean ", thisVar, " with standard error", sep="") ) +
  scale_y_continuous(breaks=theseBreaks, labels=theseLabels  ) +
  coord_cartesian(ylim=c(min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se) ) ) +
  theme_bw() +
  theme(plot.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.text    = element_text(size=rel(1), face="bold")) +
  theme(axis.ticks   = element_line(size=rel(1), colour = "black")) +
  theme(legend.title = element_text(size=rel(2))) +
  theme(legend.text  = element_text(size=rel(2)))


###############################################################################
#              BAR CHART WITH STANDARD ERROR (ONLY disturbed vs undisturbed)
###############################################################################
# Only need to change this line.
thisVar = "PCO2";
thisData = summarySE(physData, measurevar=thisVar, groupvars=c("Habitat"))
thisData$quant = thisData[, thisVar];
theseBreaks = seq( min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se), length.out=6);
theseLabels = sprintf("%.2f",theseBreaks );

ggplot(thisData, aes(x=Habitat, y=quant, fill=Habitat)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes( ymin=quant-se, ymax=quant+se ),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Habitat") +
  ylab(thisVar) +
  scale_fill_hue(name="Habitat type", # Legend label, use darker colors
                 breaks=c("disturbed", "undisturbed"),
                 labels=c("Disturbed", "Undisturbed")) +
  ggtitle(paste("Mean ", thisVar, " with standard error", sep="") ) +
  scale_y_continuous(breaks=theseBreaks, labels=theseLabels  ) +
  coord_cartesian(ylim=c(min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se) ) ) +
  theme_bw() +
  theme(plot.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.text    = element_text(size=rel(1), face="bold", colour="black")) +
  theme(axis.ticks   = element_line(size=rel(1), colour = "black")) +
  theme(legend.title = element_text(size=rel(2))) +
  theme(legend.text  = element_text(size=rel(2)))


###############################################################################
#              BAR CHART WITH STANDARD ERROR (ONLY males and disturbed vs undisturbed)
###############################################################################
# Only need to change this line.
thisVar = "PCO2";
thisData = summarySE(physData[physData$sex == "male",], measurevar=thisVar, groupvars=c("Habitat"))
thisData$quant = thisData[, thisVar];
theseBreaks = seq( min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se), length.out=6);
theseLabels = sprintf("%.2f",theseBreaks );

ggplot(thisData, aes(x=Habitat, y=quant, fill=Habitat)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes( ymin=quant-se, ymax=quant+se ),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Habitat") +
  ylab(thisVar) +
  scale_fill_hue(name="Habitat type", # Legend label, use darker colors
                 breaks=c("disturbed", "undisturbed"),
                 labels=c("Disturbed", "Undisturbed")) +
  ggtitle(paste("Mean ", thisVar, " with standard error", sep="") ) +
  scale_y_continuous(breaks=theseBreaks, labels=theseLabels  ) +
  coord_cartesian(ylim=c(min(thisData$quant - 1.1*thisData$se), max(thisData$quant + 1.1*thisData$se) ) ) +
  theme_bw() +
  theme(plot.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.title   = element_text(size=rel(2), face="bold")) +
  theme(axis.text    = element_text(size=rel(1), face="bold", colour="black")) +
  theme(axis.ticks   = element_line(size=rel(1), colour = "black")) +
  theme(legend.title = element_text(size=rel(2))) +
  theme(legend.text  = element_text(size=rel(2)))


