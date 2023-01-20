library(dplyr)
library(purrr)
library(stats)
library(ggplot2)
## Importing libraries
library(glue)
library(dplyr)
library(readr)
library(ggthemes)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(ggpubr)


amountOfData <- 20
dataFromExperiment <- c(0,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,0,0,1,0)
estimatorP <- 0.55

#bootstrap parametryczny 
estimatorP_FromNewData_ALL <- c()

for (i in 1:1000) {
  drawnData <- rbernoulli(amountOfData, estimatorP)
  estimatorP_FromNewData <- (sum(drawnData))/amountOfData
  estimatorP_FromNewData_ALL[i] <- estimatorP_FromNewData
}

estimatorP_FromNewData_ALL_Sorted <- sort(estimatorP_FromNewData_ALL)
quartile_025 <- quantile(estimatorP_FromNewData_ALL_Sorted, probs = 0.025, type = 7)
quartile_025
quartile_97_25 <- quantile(estimatorP_FromNewData_ALL_Sorted, probs = 0.9725, type = 7)
quartile_97_25
estimatorP_FromNewData_ALL_summary <- summary(estimatorP_FromNewData_ALL)
estimatorP_ALL_mean <- estimatorP_FromNewData_ALL_summary["Mean"]

estimatorP_FromNewData_DF <- as.data.frame(estimatorP_FromNewData_ALL_Sorted)

sd_val <- sd(estimatorP_FromNewData_ALL_Sorted)
mean_val <- mean(estimatorP_FromNewData_ALL_Sorted)

ggplot(estimatorP_FromNewData_DF, aes(x = estimatorP_FromNewData_ALL_Sorted)) +
  scale_color_viridis(discrete = TRUE) +
  geom_histogram(aes(y=..density..), breaks = seq(0, 1, by = 0.048) , alpha=0.7, fill="#DCE319FF", color="#95D840FF") +
  ggtitle(label="Prawdopodobieństwo wylosowania orła dla 1000 prób", subtitle=paste("Rozkład normalny N(", signif(mean_val, 4), ", ", signif(sd_val*sd_val, 4), ")")) +
  xlab("Prawdopodobieństwo sukcesu (wylosowanie orła)") +
  ylab("Częstość") +
  stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color="#aa1836", size=1) +
  theme_ipsum() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=18),
      plot.subtitle = element_text(size=12, face="bold"),
      axis.title.x = element_text(size=12, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
    )


# hist(estimatorP_FromNewData_ALL, prob = T)


#boostrap nieparametryczny  
dataNonPar <- sample(dataFromExperiment,amountOfData, replace = TRUE)
