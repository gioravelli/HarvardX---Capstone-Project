
##########################################################################
#World Happiness
#An empirical study with machine learning applications"
#author: "Giovanni Ravelli, HarvardX - Data Science"
#date: "6/10/2020"
##########################################################################


###############################################
# Loading libraries and dataset
###############################################

# Installing needed libraries and packages

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tibble)) install.packages("tibble")
if(!require(dslabs)) install.packages("dslabs")
if(!require(ggplot2)) install.packages("ggplot2") 
if(!require(scales)) install.packages("scales")
if(!require(plyr)) install.packages("plyr")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr") 
if(!require(forcats)) install.packages("forcats") 
if(!require(lubridate)) install.packages("lubridate")
if(!require(caTools)) install.packages("caTools")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(reshape2)) install.packages("reshape2")
if(!require(data.table)) install.packages("data.table")
if(!require(corrgram)) install.packages("corrgram")
if(!require(corrplot)) install.packages("corrplot")
if(!require(formattable)) install.packages("formattable")
if(!require(gridExtra)) install.packages("gridExtra)")
if(!require(ggridges)) install.packages("ggridges")
if(!require(GGally)) install.packages("GGally")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(moments)) install.packages("moments")
if(!require(nortest)) install.packages("nortest")
if(!require(e1071)) install.packages("e1071")
if(!require(car)) install.packages("car")
if(!require(broom)) install.packages("broom")
if(!require(rlang)) install.packages("rlang")
if(!require(gam)) install.packages("gam")
if(!require(rpart)) install.packages("rpart")
if(!require(randomForest)) install.packages("randomForest")


# Sourcing the dataset
dataset <- read.csv("./input/2019.csv")
dataset_2016 <- read.csv("./input/2016.csv")


# High level view of the dataset in use
class(dataset)
dim(dataset)

str(dataset)
summary(dataset)

sum(dataset$GDP.per.capita==0)
dataset$Country[which(dataset$GDP.per.capita==0)]

rank_1 <- dataset %>%
  mutate(Country = reorder(Country, Score)) %>%
  slice(1:75) %>%
  ggplot(aes(Country, Score)) +
  geom_bar(stat="identity", fill="gray") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 5), axis.text.x = element_text(size = 5)) +
  xlab("") +
  ylab("") + 
  geom_jitter(width = 0.1, alpha = 0.2) 

rank_2 <- dataset %>%
  mutate(Country = reorder(Country, Score)) %>%
  slice(76:156) %>%
  ggplot(aes(Country, Score)) +
  geom_bar(stat="identity", fill="gray") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 5), axis.text.x = element_text(size = 5)) +
  xlab("") +
  ylab("") + 
  geom_jitter(width = 0.1, alpha = 0.2)
  
grid.arrange(rank_1, rank_2, ncol = 2) 

###############################################
# Data Wrangling
###############################################

dataset_2016 <- dataset_2016[,1:4]  # cleaning the 2016 dataset
dataset_2016 <- dataset_2016[,-3]  

dataset <- left_join(dataset,dataset_2016, by="Country", stringsAsFactors = FALSE)   # importing the 2016 "clean" dataset with Score and the Regions


# renaming columns

colnames(dataset)[which(colnames(dataset) %in% 
  c("Overall.rank", "GDP.per.capita","Social.support", "Healthy.life.expectancy","Freedom.to.make.life.choices", "Perceptions.of.corruption", "Happiness.Score"))] <- 
  c("Rank", "Economy", "Family", "Health", "Freedom", "Corruption", "Score_2016")


# shortening region names and checking for errors

levels(dataset$Region)[levels(dataset$Region)=="Australia and New Zealand"] <- "ANZ"
levels(dataset$Region)[levels(dataset$Region)=="Western Europe"] <- "West Europe"
levels(dataset$Region)[levels(dataset$Region)=="Central and Eastern Europe"] <- "East Europe"
levels(dataset$Region)[levels(dataset$Region)=="Middle East and Northern Africa"] <- "MENA"
levels(dataset$Region)[levels(dataset$Region)=="Latin America and Caribbean"] <- "LATAM"
levels(dataset$Region)[levels(dataset$Region)=="Southeastern Asia"] <- "SE Asia"
levels(dataset$Region)[levels(dataset$Region)=="Southern Asia"] <- "South Asia"
levels(dataset$Region)[levels(dataset$Region)=="Eastern Asia"] <- "East Asia"
levels(dataset$Region)[levels(dataset$Region)=="Sub-Saharan Africa"] <- "Sub-Sahara"


# checking for errors and cleaning/filling "N/A"

which(is.na(dataset$Region))
dataset$Country[which(is.na(dataset$Region))]

dataset$Region[which(is.na(dataset$Region))] = c("LATAM", "West Europe", "East Europe", "Sub-Sahara","Sub-Sahara","Sub-Sahara","Sub-Sahara","Sub-Sahara")
which(is.na(dataset$Region))
dataset <- dataset %>% drop_na()
sum(is.na(dataset))

dataset <- dataset[, c(1,2, 10, 11, 3, 4:9)]

kable(head(dataset,30), format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "scale_down")

glimpse(dataset)

save(file="dataset")


###############################################
# Visualization
###############################################

# Scatterplots

dataset %>% ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("GDP per capita") +
  ylab("Happiness Score") +
  ggtitle("") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "bottom", legend.title=element_blank())
  
west <- dataset %>% 
  filter(Region %in% c("North America", "West Europe", "ANZ", "East Europe"))%>% 
  ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("") +
  ylab("") +
  ggtitle("West") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position="none", plot.title = element_text(size = 10))

east <- dataset %>% 
  filter(Region %in% c("SE Asia", "East Asia", "South Asia"))%>% 
  ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("") +
  ylab("") +
  ggtitle("Asia") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "none") +
  theme(legend.position="none", plot.title = element_text(size = 10))

latin <- dataset %>% 
  filter(Region %in% c("LATAM"))%>% 
  ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("") +
  ylab("") +
  ggtitle("LATAM") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "none") +
  theme(legend.position="none", plot.title = element_text(size = 10))

ME_africa <- dataset %>% 
  filter(Region %in% c("MENA", "Sub-Sahara"))%>% 
  ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("") +
  ylab("") +
  ggtitle("Africa & ME") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "none") +
  theme(legend.position="none", plot.title = element_text(size = 10))

gridplot <- grid.arrange(west, east, latin, ME_africa, ncol = 2)


## qplots

qplot<- dataset %>% 
  qplot(Region, Score, data = ., xlab = "") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  xlab("")


## Box Plots
boxplot <- dataset %>% qplot(Region, Score, data = ., geom = "boxplot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")

grid.arrange(qplot, boxplot, ncol = 1)

## qPlots Grid

qplot1 <- dataset %>% 
  qplot(Region, Economy, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

qplot2 <- dataset %>% 
  qplot(Region, Family, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

qplot3 <- dataset %>% 
  qplot(Region, Health, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

qplot4 <- dataset %>% 
  qplot(Region, Freedom, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

qplot5 <- dataset %>% 
  qplot(Region, Generosity, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

qplot6 <- dataset %>% 
  qplot(Region, Corruption, data = .) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size = 8)) +
        xlab("")

gridQplot <- grid.arrange(qplot1, qplot2,qplot3,qplot4,qplot5,qplot6 ,ncol = 3)


# Histograms by Region
qplot(x = Score, data = dataset,
      color = I('black')) +
  xlab("Happiness Score") +
  ylab("Number of Countries") +
  facet_wrap(~Region, ncol = 5)


# Ridge Density Plot to understand distribution of variables by continent

dataset %>% 
  ggplot(aes(Score, Region)) +
  scale_x_continuous( ) +
  geom_density_ridges(jittered_points = TRUE) +
  xlab("Happiness Score") +
  ylab("")

p1 <- dataset %>%
  ggplot(aes(Economy, Region)) +
  scale_x_continuous( ) + 
  geom_density_ridges(jittered_points = FALSE) +
  xlab("Economy") +
  ylab("") + 
  theme(axis.text.y=element_blank(), axis.text = element_text(size = 7), axis.title.x = element_text(size = 8)) 


p2 <- dataset %>%
  ggplot(aes(Family, Region)) +
  scale_x_continuous( ) + 
  geom_density_ridges(jittered_points = FALSE) +
  xlab("Family") +
  ylab("") + 
  theme(axis.text.y=element_blank(), axis.text = element_text(size = 7), axis.title.x = element_text(size = 8))  

p3 <- dataset %>%
  ggplot(aes(Corruption, Region)) +
  scale_x_continuous( ) + 
  geom_density_ridges(jittered_points = FALSE) +
  xlab("Corruption") +
  ylab("") + 
  theme(axis.text.y=element_blank(), axis.text = element_text(size = 7), axis.title.x = element_text(size = 8)) 

p4 <- dataset %>%
  ggplot(aes(Freedom, Region)) +
  scale_x_continuous( ) + 
  geom_density_ridges(jittered_points = FALSE) +
  xlab("Freedom") +
  ylab("") + 
  theme(axis.text.y=element_blank(), axis.text = element_text(size = 7), axis.title.x = element_text(size = 8)) 

grid1 <- grid.arrange(p1,p2,p3,p4, ncol = 2)

# Comparing Happiness Scores across years: 2016 vs 2019

H2016 <- dataset %>% 
  ggplot(aes(Economy, Score_2016, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("GDP per capita") +
  ylab("") +
  ggtitle("Happiness Score, 2016") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "", text = element_text(size = 10))

H2019 <- dataset %>% 
  ggplot(aes(Economy, Score, label = "")) +
  geom_text(nudge_x = 0.5) +
  xlab("GDP per capita") +
  ylab("") +
  ggtitle("Happiness Score, 2019") + 
  geom_point(aes(col=Region), size = 2) +
  theme(legend.position = "", text = element_text(size = 10))

scatter <- grid.arrange(H2016, H2019, ncol = 2) 


# Histograms West vs Developing World

Hist2016_West <- dataset[(dataset$Region %in% c("ANZ", "East Europe", "North America", "West Europe")),] %>% 
  ggplot(aes(Score_2016)) +
  geom_histogram(binwidth = 0.2, color = "grey") + 
  xlab("") +
  ylab("Year 2016") 

Hist2016_DEV <- dataset[(dataset$Region %in% c("East Asia", "LATAM", "MENA", "SE Asia", "South Asia", "Sub-Sahara")),] %>% 
  ggplot(aes(Score_2016)) +
  geom_histogram(binwidth = 0.2, color = "grey") + 
  xlab("") +
  ylab("") 

Hist2019_West <- dataset[(dataset$Region %in% c("ANZ", "East Europe", "North America", "West Europe")),] %>% 
  ggplot(aes(Score)) +
  geom_histogram(binwidth = 0.2, color = "grey") + 
  xlab("West") +
  ylab("Year 2019") 

Hist2019_DEV <- dataset[(dataset$Region %in% c("East Asia", "LATAM", "MENA", "SE Asia", "South Asia", "Sub-Sahara")),] %>% 
  ggplot(aes(Score)) +
  geom_histogram(binwidth = 0.2, color = "grey") + 
  xlab("Developing World") +
  ylab("") 

hist <- grid.arrange(Hist2016_West, Hist2016_DEV, Hist2019_West, Hist2019_DEV, ncol = 2) 


# Boxplots 2016 vs 2019

Bplot2016 <- dataset %>% qplot(Region, Score_2016, data = ., geom = "boxplot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") +
  ylab("") + 
  ggtitle("Happiness Score, 2016") 

Bplot2019 <- dataset %>% qplot(Region, Score, data = ., geom = "boxplot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Happiness Score, 2019")

grid.arrange(Bplot2016, Bplot2019, ncol = 2) 



###############################################
# Exploratory data analysis 
###############################################

# Moving from Histograms versus Density plots

plot1 <- qplot(dataset$Score, bins=10, color = I("blue"), xlab = "")
plot2 <- qplot(dataset$Score, color = I("red"), xlab = "")
plot3 <- qplot(dataset$Score, geom = "density", color = I("red"), xlab = "2019 Happiness Score")

grid.arrange(plot1,plot2,plot3, ncol = 1)

# Correlation 

correlations <- cor(dataset[,5:11],method="pearson")
corrplot(correlations, number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "full", tl.cex=0.8,tl.col = "black")

ggcorr(dataset[,5:11], palette = "RdYlGn", name = "rho", 
       label = TRUE, label_color = "black")


# Sperman correlation

ranked_data <- mutate(dataset[,5:11], R_Score = rank(Score), R_Econ=rank(Economy),R_Fam=rank(Family), R_Health=rank(Health), 
                                      R_Free=rank(Freedom), R_Gen=rank(Generosity), R_Corrupt=rank(Corruption))
ranked_data <- ranked_data[,-(1:7)]

ggcorr(ranked_data, palette = "RdYlGn", name = "rho", 
      label = TRUE, label_color = "black")


# Using the GGally package

ggpairs(dataset[,5:11], columns = 1:ncol(dataset[,5:11]), title = "",  
        axisLabels = "show", columnLabels = colnames(dataset[, 5:11]),
        upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE), continuous = "density", combo = "box_no_facet"),
        lower = list(continuous =  wrap(ggally_nostic_resid, displayGrid = FALSE))
        )

ggpairs(data=dataset,
        columns=5:11, 
        upper = list(continuous = wrap(ggally_barDiag, displayGrid = FALSE)),
        lower = list(continuous = "density")
        )


ggpairs(dataset[,5:11], aes(colour = as.factor(dataset$Region), alpha = 0.4), 
        upper = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)),
        lower = list(continuous = "points", combo = "dot_no_facet")
        )


# Principal Component Analysis

pca <- prcomp(dataset[,5:11], center=TRUE, scale.=TRUE)
plot(pca, type="l", main='')
grid(nx = 10, ny = 14)
title(main = "Principal components weights", sub = NULL, xlab = "Components", cex.lab=1, cex.main = 1)
box() 

pca


###############################################
# Linear Regression Model
###############################################


# qq-plot to assess normality of distribution

qqnorm(dataset$Score, cex.lab=0.8, cex.main = 0.9); qqline(dataset$Score) 

dataset %>% 
  mutate(z_score = round((dataset$Score - mean(dataset$Score))/sd(dataset$Score))) %>%
  filter(z_score %in% -3:3) %>%
  ggplot() +
  stat_qq(aes(sample=dataset$Score)) +
  facet_wrap(~z_score) + 
  theme(axis.title.x=element_blank()) +
  xlab("") + 
  ylab("")


# Understanding skewness and kurtosis of the data distributions

hist(dataset$Score, col="gray", xlab = "", main = "Happiness Score") 

par(mfrow=c(2,3))
hist(dataset$Economy, col="gray", xlab = "", main = "GDP per capita") 
hist(dataset$Family, col="gray", xlab = "", main = "Social Support") 
hist(dataset$Health, col="gray", xlab = "", main = "Life Expectation") 
hist(dataset$Freedom, col="gray", xlab = "", main = "Freedom") 
hist(dataset$Generosity, col="gray", xlab = "", main = "Generosity") 
hist(dataset$Corruption, col="gray", xlab = "", main = "Trust in Government") 
par(mfrow=c(1,1))


#  Shapiro-Wilk test, skewness and kurtosis

sh_0 <- shapiro.test(dataset$Score)[2]
sk_0 <- skewness(dataset$Score)
kr_0 <- kurtosis(dataset$Score)

sh_1 <- shapiro.test(dataset$Economy)[2]
sk_1 <- skewness(dataset$Economy)
kr_1 <- kurtosis(dataset$Economy)

sh_2 <- shapiro.test(dataset$Family)[2]
sk_2 <- skewness(dataset$Family)
kr_2 <- kurtosis(dataset$Family)

sh_3 <- shapiro.test(dataset$Health)[2]
sk_3 <- skewness(dataset$Health)
kr_3 <- kurtosis(dataset$Health)

sh_4 <- shapiro.test(dataset$Freedom)[2]
sk_4 <- skewness(dataset$Freedom)
kr_4 <- kurtosis(dataset$Freedom)

sh_5 <- shapiro.test(dataset$Generosity)[2]
sk_5 <- skewness(dataset$Generosity)
kr_5 <- kurtosis(dataset$Generosity)

sh_6 <- shapiro.test(dataset$Corruption)[2]
sk_6 <- skewness(dataset$Corruption)
kr_6 <- kurtosis(dataset$Corruption)

norm_test <- matrix(c(sh_0,sh_1, sh_2, sh_3, sh_4, sh_5, sh_6,
                      sk_0,sk_1, sk_2, sk_3, sk_4, sk_5, sk_6,
                      kr_0,kr_1, kr_2, kr_3, kr_4, kr_5, kr_6), 
                  nrow = 7, ncol = 3)
colnames(norm_test) <- c("p-value", "skweness", "kurtosis")
rownames(norm_test) <- c("Score", "Economy", "Family", "Health", "Freedom", "Generosity", "Corruption")

kable(norm_test, format = "latex", booktabs = TRUE, caption = "Normality Tests") %>% 
  kable_styling(latex_options = "scale_down")


# Linear Regression
fit_Economy <- lm(Score ~ Economy, data = dataset) 
summary(fit_Economy)

fit_Health <- lm(Score ~ Health, data = dataset) 
summary(fit_Health)

fit_Corruption <- lm(Score ~ Corruption, data = dataset) 
summary(fit_Corruption)

ggplot1 <- dataset %>% ggplot(aes(Economy, Score)) + geom_point() + geom_smooth(method = "lm") 
ggplot2 <-  dataset %>% ggplot(aes(Health, Score)) + geom_point() + geom_smooth(method = "lm")
ggplot3 <-  dataset %>% ggplot(aes(Corruption, Score)) + geom_point() + geom_smooth(method = "lm")

grid.arrange(ggplot1, ggplot2, ggplot3, ncol=3)


# Fitting the model

fit <- lm(Score ~ ., data = dataset[,5:11]) 
predictions <- predict(fit, se.fit = TRUE)
summary(fit)

par(mfrow=c(1,2))
qqplot(dataset$Score, fit$fitted.values, points.col=10, main = "Q-Q Plot: Estimates vs Scores", 
       cex.lab=0.8, cex.main = 0.8, 
       ylab = "Fitted Values", xlab = "True Values") 
qqnorm(fit$residuals, cex.lab=0.8, cex.main = 0.8); qqline(fit$residuals)  
par(mfrow=c(1,1))

tidy(fit, conf.int = TRUE)
glance(fit)
augment(fit)

td <- tidy(fit, conf.int = TRUE)
ggplot(td, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_line() + 
  theme(legend.position = "")


###############################################
# Machine Learning
###############################################

# Function that computes the residual means squared error 
  
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}
  
  
# Data partitioning into train-set and test-set
  
set.seed(1979, sample.kind="Rounding")
  
index <- createDataPartition(y = dataset$Score, times = 1, p = 0.2, list = FALSE)
train_set <- dataset[-index, 5:11]
test_set <- dataset[index, 5:11]

table <- matrix(c(ncol(train_set),nrow(train_set), ncol(test_set), nrow(test_set)), 
                nrow = 2, ncol = 2)

colnames(table) <- c("Train Set", "Test Set")
rownames(table) <- c("N. Features", "N. Observations")
table     
  

# ML MODEL 1: Generalized Linerar Model (GLM)

glm_fit <- train(Score ~ ., method = "glm", data = train_set)
glm_predictions <- predict(glm_fit, test_set, type = "raw")

glm_fit
summary(glm_predictions)

glm_rmse <- RMSE(test_set$Score, glm_predictions) 
glm_rmse


# ML MODEL 2: k-nearest neighbors (KNN)

knn_fit <- train(Score ~ ., method = "knn", data = train_set)
knn_predictions <- predict(knn_fit, test_set, type = "raw")

knn_fit
summary(knn_predictions)

knn_rmse <- RMSE(test_set$Score,knn_predictions) 
knn_rmse


# ML MODEL 3: Local weighted regression (loess)

grid <- expand.grid(span = seq(0, 10, len = 20), degree = 1)
train_loess <- train(Score ~ ., method = "gamLoess", 
                     tuneGrid=grid, 
                     data = train_set )

ggplot(train_loess, highlight = TRUE)

train_loess$bestTune

loess_fit <- train(Score ~ ., method = "gamLoess", 
                   span = train_loess$bestTune, 
                   degree = 1, 
                   data = train_set)
loess_fit

loess_predictions <- predict(loess_fit, test_set)
summary(loess_predictions)

loess_rmse <- RMSE(test_set$Score,loess_predictions) 
loess_rmse


# ML MODEL 4: Regression Tree

tree_fit <- rpart(Score ~ ., data = train_set)
plot(tree_fit, margin = 0.1)
text(tree_fit, cex = 0.75)

tree_fit$cptable

tree1 <- train_set %>%
  mutate(y_hat = predict(tree_fit)) %>%
  ggplot() +
  geom_point(aes(Economy, Score)) +
  geom_step(aes(Economy, y_hat), col="red") +
  theme(text = element_text(size = 10))

tree2 <- train_set %>%
  mutate(y_hat = predict(tree_fit)) %>%
  ggplot() +
  geom_point(aes(Family, Score)) +
  geom_step(aes(Family, y_hat), col="red") +
  theme(text = element_text(size = 10))

tree3 <- train_set %>%
  mutate(y_hat = predict(tree_fit)) %>%
  ggplot() +
  geom_point(aes(Health, Score)) +
  geom_step(aes(Health, y_hat), col="red") +
  theme(text = element_text(size = 10))

tree4 <- train_set %>%
  mutate(y_hat = predict(tree_fit)) %>%
  ggplot() +
  geom_point(aes(Freedom, Score)) +
  geom_step(aes(Freedom, y_hat), col="red") +
  theme(text = element_text(size = 10))

grid.arrange(tree1, tree2, tree3, tree4, ncol=1)

tree_fit <- rpart(Score ~ ., data = train_set)
tree_predictions <- predict(tree_fit, test_set)
summary(tree_predictions)

tree_rmse <- RMSE(test_set$Score,tree_predictions) 
tree_rmse


# ML MODEL 5: Random Forest

rf_fit <- randomForest(Score ~ ., data = train_set)
rf_fit

plot(rf_fit, cex.lab=1, cex.main = 1, main = "Random Forest Fit")

train_set %>%
  mutate(y_hat = predict(rf_fit, newdata = train_set)) %>%
  ggplot() +
  geom_point(aes(Economy, Score)) +
  geom_line(aes(Economy, y_hat), col="red") +
  theme(text = element_text(size = 10))

rf_predictions <- predict(rf_fit, test_set)
summary(rf_predictions)

rf_rmse <- RMSE(test_set$Score,rf_predictions) 
rf_rmse

varImp(rf_fit)

## ML Model 6: Clustering - Heatmap

x <- sweep(dataset[,5:11], 2, colMeans(dataset[,5:11]))
x<- as.matrix(x)

h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))

image(x[h_1$order, h_2$order],
      col = RColorBrewer::brewer.pal(11, "Spectral"))

heatmap(x, col = RColorBrewer::brewer.pal(10, "Spectral"), cexCol=1) 

small_x <- x[1:10,]
1-cor(t(small_x))
hc <- hclust(as.dist(1-cor(t(small_x))))
plot(hc)


###############################################
# Results
###############################################


# Summary table of RMSE results

RMSE_results <- matrix(c(glm_rmse, knn_rmse, loess_rmse, tree_rmse, rf_rmse, "N/A",
                         "linear (parametric)", "locally constant (non-parametric)", "locally linear  (non-parametric)", 
                         "non-linear", "non-linear", "non-linear",
                         "yes", "yes", "yes", "yes", "yes", "yes",
                         "no", "yes", "yes", "yes", "yes", "no",
                         "yes", "yes", "yes", "yes", "yes", "clustering"), 
                    nrow = 6, ncol = 5)

colnames(RMSE_results) <- c("RMSE", "Linearity", "Discriminative", "Cross-Validated", "Supervised")
rownames(RMSE_results) <- c("Generalized Linear Model", "K-nearest Neighbors", "Local Weighted Regression", 
                         "Regression Tree", "Random Forest", "Heatmap")

kable(RMSE_results, format = "latex", booktabs = TRUE, 
            caption = "Machine Learning Models - Predictive Accuracy (RMSE)") %>% 
  kable_styling(latex_options = "scale_down")


# GGplot of true scores versus predicted scores

glm_data <- as.data.frame(cbind(Predictions = glm_predictions, Actual = test_set$Score))
knn_data <- as.data.frame(cbind(Predictions = knn_predictions, Actual = test_set$Score))
loess_data <- as.data.frame(cbind(Predictions = loess_predictions, Actual = test_set$Score))
tree_data <- as.data.frame(cbind(Predictions = tree_predictions, Actual = test_set$Score))
rf_data <- as.data.frame(cbind(Predictions = rf_predictions, Actual = test_set$Score))

gg_glm <- ggplot(glm_data, aes(Actual, Predictions)) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Generalized Linear Model", x = "True Score",
       y = "Predicted Score") +
  theme(plot.title = element_text(face = "bold", size = (10)), 
        axis.title = element_text(size = (10)))

gg_knn <- ggplot(knn_data, aes(Actual, Predictions)) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "k-nearest Neighbors", x = "True Score",
       y = "Predicted Score") +
  theme(plot.title = element_text(face = "bold", size = (10)), 
        axis.title = element_text(size = (10)))

gg_loess <- ggplot(loess_data, aes(Actual, Predictions)) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Local Weighted Regression", x = "True Score",
       y = "Predicted Score") +
  theme(plot.title = element_text(face = "bold", size = (10)), 
        axis.title = element_text(size = (10)))

gg_tree <- ggplot(tree_data, aes(Actual, Predictions)) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Regression Tree", x = "True Score",
       y = "Predicted Score") +
  theme(plot.title = element_text(face = "bold", size = (10)), 
        axis.title = element_text(size = (10)))

gg_rf <- ggplot(rf_data, aes(Actual, Predictions)) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Rain Forest", x = "True Score",
       y = "Predicted Score") +
  theme(plot.title = element_text(face = "bold", size = (10)), 
        axis.title = element_text(size = (10)))

grid.arrange(gg_glm, gg_knn, gg_loess, gg_tree, gg_rf, ncol = 2, nrow = 3)
