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


##### Inflation

### Importing data
inflation_data <- read.csv("data/EU_Inflation_HICP_data_mod.csv", header = TRUE, sep = ",")


### Converting data
inflation_data$Period <- as.Date(inflation_data$Period, format = "%d/%m/%Y")


### Converting data to numeric
suppressWarnings(inflation_data[, 2:29] <- sapply(inflation_data[, 2:29], as.numeric))


### Creating subsets
eurozone_countries_inflation <- select(inflation_data, matches("Period|Austria|Belgium|Cyprus|Germany|Estonia|Spain|Finland|France|Greece|Ireland|Italy|Lithuania|Luxembourg|Latvia|Malta|Netherlands|Portugal|Slovenia|Slovakia"))
other_countries_inflation <- select(inflation_data, matches("Period|Bulgaria|Czech|Denmark|Croatia|Hungary|Poland|Romania|Sweden"))
all_countries_inflation <- select(inflation_data, !matches("EU"))
poland_inflation <- select(inflation_data, matches("Period|Poland"))


### Creating subsets with average rate
eurozone_countries_inflation_avg <- data.frame(eurozone_countries_inflation[1], "Kraje w strefie Euro"=(rowMeans(eurozone_countries_inflation[2:20], na.rm=TRUE)))
other_countries_inflation_avg <- data.frame(other_countries_inflation[1], "Kraje poza strefa Euro"=(rowMeans(other_countries_inflation[2:9], na.rm=TRUE)))
all_countries_inflation_avg <- data.frame(all_countries_inflation[1], "UE"=(rowMeans(all_countries_inflation[2:28], na.rm=TRUE)))
poland_inflation <- data.frame(poland_inflation[1], "Polska"=(rowMeans(poland_inflation[2], na.rm=TRUE)))

inflation_euro_other_avg <- data.frame(
  inflation_data[1],
  eurozone_countries_inflation_avg[2],
  other_countries_inflation_avg[2]
)

inflation_avg <- data.frame(
  inflation_data[1],
  eurozone_countries_inflation_avg[2],
  other_countries_inflation_avg[2],
  all_countries_inflation_avg[2],
  poland_inflation[2]
)


### Converting data to tidy form
eurozone_countries_inflation_tidy <- eurozone_countries_inflation |> pivot_longer(-Period, names_to = "country", values_to = "inflation")
other_countries_inflation_tidy <- other_countries_inflation |> pivot_longer(-Period, names_to = "country", values_to = "inflation")
all_countries_inflation_tidy <- all_countries_inflation |> pivot_longer(-Period, names_to = "country", values_to = "inflation")
poland_inflation_tidy <- poland_inflation |> pivot_longer(-Period, names_to = "country", values_to = "inflation")
inflation_euro_other_avg_tidy <- inflation_euro_other_avg |> pivot_longer(-Period, names_to = "origin", values_to = "inflation")
inflation_avg_tidy <- inflation_avg |> pivot_longer(-Period, names_to = "origin", values_to = "inflation")


### Creating merged tidy data
eurozone_countries_inflation_tidy_origin <- eurozone_countries_inflation_tidy
other_countries_inflation_tidy_origin <- other_countries_inflation_tidy
all_countries_inflation_tidy_origin <- all_countries_inflation_tidy
poland_inflation_tidy_origin <- poland_inflation_tidy

eurozone_countries_inflation_tidy_origin['origin'] <- "Kraje w strefie Euro"
other_countries_inflation_tidy_origin['origin'] <- "Kraje poza strefą Euro"
all_countries_inflation_tidy_origin['origin'] <- "UE"
poland_inflation_tidy_origin['origin'] <- "Polska"

inflation_euro_other_tidy_origin <- rbind(eurozone_countries_inflation_tidy_origin,
                               other_countries_inflation_tidy_origin)

inflation_tidy_origin <- rbind(eurozone_countries_inflation_tidy_origin,
                          other_countries_inflation_tidy_origin,
                          all_countries_inflation_tidy_origin,
                          poland_inflation_tidy_origin)


### Creating ranges of intervals
start <- as.Date("01/01/2012", "%d/%m/%Y")
end <- as.Date("01/01/2016", "%d/%m/%Y")


### Split data to intervals

## Average
inflation_avg_data <- inflation_avg_tidy %>% filter(Period >= start & Period <= end)
inflation_euro_other_avg_data <- inflation_euro_other_avg_tidy %>% filter(Period >= start & Period <= end)

## Eurozone
eurozone_inflation_data <- eurozone_countries_inflation_tidy %>% filter(Period >= start & Period <= end)

## Countries outside the eurozone
other_inflation_data <- other_countries_inflation_tidy %>% filter(Period >= start & Period <= end)

## Origin
inflation_data_origin <- inflation_tidy_origin %>% filter(Period >= start & Period <= end)
inflation_euro_other_data_origin <- inflation_euro_other_tidy_origin %>% filter(Period >= start & Period <= end)


### Inflation differences from month to month

inflation_diff_eurozone_other <- inflation_euro_other_avg[1:(nrow(inflation_euro_other_avg) - 1),] - inflation_euro_other_avg[2:nrow(inflation_euro_other_avg),]
inflation_diff_eurozone_other$Period <- inflation_euro_other_avg[1:(nrow(inflation_euro_other_avg) - 1), 1]

inflation_diff_eurozone_other_tidy <- inflation_diff_eurozone_other |> pivot_longer(-Period, names_to = "zone", values_to = "difference")
inflation_diff_eurozone_other_data <- inflation_diff_eurozone_other_tidy %>% filter(Period >= start & Period <= end)


