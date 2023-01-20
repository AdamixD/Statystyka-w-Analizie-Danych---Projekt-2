library(glue)
library(dplyr)
library(readr)
library(ggthemes)
library(tidyr)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(ggpubr)

x1 = seq(0,1,0.01)
y1 = dbeta(x1,20,15)

plot(x1,y1, type = "l", col = "blue")


alfa = 15
beta = 10
successesInTest = 11 

y2 = rbeta(20,alfa,beta)
y3 = rbeta(20,alfa+successesInTest,beta+successesInTest)

y2_df <- as.data.frame(y2)
y3_df <- as.data.frame(y3)

ggplot(y2_df, aes(y = y2)) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Rozkład a priori") +
  xlab("p") +
  ylab("f(p)") +
  grid() +
  stat_function(fun = dnorm, args = list(mean = mean(y2), sd = sd(y2)), color="#482677", size=1) +
  theme_ipsum() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size=12, face="bold"),
    plot.title = element_text(size=18),
    plot.subtitle = element_text(size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
  )

ggplot(y2_df, aes(y = y2)) +
  # scale_color_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE, labels = c("a priori", "a posteriori")) +
  stat_function(fun = dnorm, args = list(mean = mean(y2), sd = sd(y2)), color="#482677", size=1)  +
  stat_function(fun = dnorm, args = list(mean = mean(y3), sd = sd(y3)), color="#FDE725", size=1) +
  ggtitle("Rozkład a posteriori") +
  xlab("p") +
  ylab("f(p)") +
  grid() +
  theme_ipsum() +
  theme(
    legend.text = element_text(size=12, face="bold"),
    plot.title = element_text(size=18),
    plot.subtitle = element_text(size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
  )
