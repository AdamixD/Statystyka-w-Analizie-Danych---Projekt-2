###### Visualization - Line

# Line (avg)
plot_rate_lines_avg(inflation_avg_data, "Poziom inflacji od 2005 do 2018 roku", "inflation")
plot_rate_lines_avg(inflation_euro_other_avg_data, "Poziom inflacji od 2005 do 2018 roku", "inflation")

# Line (country) - Eurozone
plot_rate_lines_country(eurozone_inflation_data, "Poziom inflacji w strefie Euro od 2005 do 2018 roku", "inflation")

# Line (country) - Countries outside the Eurozone
plot_rate_lines_country(other_inflation_data, "Poziom inflacji poza strefą Euro od 2005 do 2018 roku", "inflation")

# Line (country) - Multiple zones
plot_rate_lines_country_multiple(eurozone_inflation_data, "Poziom inflacji w strefie Euro od 2005 do 2018 roku",
                                 other_inflation_data, "Poziom inflacji poza strefą Euro od 2005 do 2018 roku",
                                 "inflation")


###### Visualization - Box

# Box - Average inflation in period
plot_rate_boxes(inflation_avg_data, "Poziom inflacji od 2005 do 2018 roku (uśredniona wartość w okresie)", "inflation")
plot_rate_boxes(inflation_euro_other_avg_data, "Poziom inflacji od 2005 do 2018 roku (uśredniona wartość w okresie)", "inflation")

# Box - Inflation in period
plot_rate_boxes(inflation_data_origin, "Poziom inflacji od 2005 do 2018 roku", "inflation")
plot_rate_boxes(inflation_euro_other_data_origin, "Poziom inflacji od 2005 do 2018 roku", "inflation")


###### Summary
summary_eurozone_inflation_data <- inflation_data_origin |> filter(origin == 'Kraje w strefie Euro') |> summary()
summary_other_inflation_data <- inflation_data_origin |> filter(origin == 'Kraje poza strefą Euro') |> summary()

summary_eurozone_inflation_data[,3]
summary_other_inflation_data[,3]


###### Visualization - histogram

# Histogram of differences (single)
plot_histogram_single(inflation_diff_eurozone_other_data, "Histogram różnic poziomów inflacji z miesiąca na miesiąc od 2005 do 2018 roku")

