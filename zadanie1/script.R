###### Visualization - Line

# Line (avg)
plot_rate_lines_avg(inflation_avg_data, "Poziom inflacji od 2012 do 2016 roku", "inflation")
plot_rate_lines_avg(inflation_euro_other_avg_data, "Poziom inflacji od 2012 do 2016 roku", "inflation")

# Line (country) - Eurozone
plot_rate_lines_country(eurozone_inflation_data, "Poziom inflacji w strefie Euro od 2012 do 2016 roku", "inflation")

# Line (country) - Countries outside the Eurozone
plot_rate_lines_country(other_inflation_data, "Poziom inflacji poza strefą Euro od 2012 do 2016 roku", "inflation")

# Line (country) - Multiple zones
plot_rate_lines_country_multiple(eurozone_inflation_data, "Poziom inflacji w strefie Euro od 2012 do 2016 roku",
                                 other_inflation_data, "Poziom inflacji poza strefą Euro od 2012 do 2016 roku",
                                 "inflation")


###### Visualization - Box

# Box - Average inflation in period
plot_rate_boxes(inflation_avg_data, "Poziom inflacji od 2012 do 2016 roku (uśredniona wartość w okresie)", "inflation")
plot_rate_boxes(inflation_euro_other_avg_data, "Poziom inflacji od 2012 do 2016 roku (uśredniona wartość w okresie)", "inflation")

# Box - Inflation in period
plot_rate_boxes(inflation_data_origin, "Poziom inflacji od 2012 do 2016 roku", "inflation")
plot_rate_boxes(inflation_euro_other_data_origin, "Poziom inflacji od 2012 do 2016 roku", "inflation")


###### Summary
summary_eurozone_inflation_data <- inflation_data_origin |> filter(origin == 'Kraje w strefie Euro') |> summary()
summary_other_inflation_data <- inflation_data_origin |> filter(origin == 'Kraje poza strefą Euro') |> summary()

summary_eurozone_inflation_data[,3]
summary_other_inflation_data[,3]


###### Visualization - histogram

# Histogram of inflation rate (single)
plot_histogram_single(inflation_euro_other_avg_data, "Histogram poziomów inflacji od 2012 do 2016 roku")

# Histogram of inflation rate differences (single)
plot_histogram_diff_single(inflation_diff_eurozone_other_data, "Histogram różnic poziomów inflacji z miesiąca na miesiąc od 2012 do 2016 roku")



###### Statistical hypothesis verification

# Data preprocessing
eurozone <- t((inflation_euro_other_avg_data |> filter(origin == 'Kraje.w.strefie.Euro'))[3])
other <- t((inflation_euro_other_avg_data |> filter(origin == 'Kraje.poza.strefa.Euro'))[3])


### Kolmogorov-Smirnov test
# Kolmogorov-Smirnov tests (normal distribution)
ks.test(eurozone, "pnorm")
ks.test(other, "pnorm")
# Kolmogorov-Smirnov tests (same distribution)
ks.test(eurozone, other)


### F-test (same variance)
var.test(eurozone, other)


### Chi-squared test (independence)
chisq.test(t(eurozone), t(other))


### Hypothesis
# H0 - Inflation in eurozone countries is equal to non-eurozone countries
# H1 - Inflation in eurozone countries is lower than in non-eurozone countries


### T-Student test
# T-Student test (basic)
t.test(eurozone, other, alternative="less")

# T-Student test (paired)
t.test(eurozone, other, alternative="less", paired=TRUE)


### Wilcoxon test
# Wilcoxon test (basic)
wilcox.test(eurozone, other, alternative="less")

# Wilcoxon test (paired)
wilcox.test(eurozone, other, alternative="less", paired=TRUE)

# wilcox.test(eurozone, other, paired = TRUE)





