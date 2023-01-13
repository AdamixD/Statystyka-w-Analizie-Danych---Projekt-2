plot_rate_lines_avg <- function(data, title, type) {
  data |>
    ggplot(aes(x=Period, y= if (type == "inflation") inflation else deposits, group=origin, color=origin)) +
    scale_color_viridis(discrete = TRUE, labels = c("Kraje poza strefą Euro", "Kraje w strefie Euro", "Polska", "UE")) +
    geom_line(size=1.2) +
    guides(fill=guide_legend(title=NULL)) +
    xlab("Data") + ylab(if (type == "inflation") "Inflacja" else "Oprocentowanie depozytów") +
    scale_x_date(date_labels = "%b-%Y") +
    ggtitle(title) +
    theme_ipsum() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=18),
      axis.title.x = element_text(size=12, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
    )
}

plot_rate_lines_country <- function(data, title, type) {
  data |>
    ggplot(aes(x=Period, y=if (type == "inflation") inflation else deposits, group=country, color=country)) +
    scale_color_viridis(discrete = TRUE) +
    geom_line(size=1.2) +
    guides(fill=guide_legend(title=NULL)) +
    xlab("Data") + ylab(if (type == "inflation") "Inflacja" else "Oprocentowanie depozytów") +
    scale_x_date(date_labels = "%b-%Y") +
    ggtitle(title) +
    theme_ipsum() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=18),
      axis.title.x = element_text(size=12, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
    )
}

plot_rate_boxes <- function(data, title, type) {
  data |>
    ggplot(aes(origin, if (type == "inflation") inflation else deposits, fill=origin)) +
    geom_boxplot(size=0.7) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    ylab(if (type == "inflation") "Inflacja" else "Oprocentowanie depozytów") +
    ggtitle(title) +
    theme_ipsum() +
    theme(
      legend.position = "none",
      legend.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=18),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=12, face="bold")
    )
}

plot_rate_lines_country_multiple <-  function(data1, title1, data2, title2, type) {
  ggarrange(plot_rate_lines_country(data1, title1, type),
            plot_rate_lines_country(data2, title2, type),
            ncol = 2, nrow = 1)
}

plot_rate_lines_selected_countries <- function(data, title, type) {
  data |>
      ggplot(aes(x=Period, y=if (type == "inflation") inflation else deposits, group=country, color=country)) +
        scale_color_viridis(discrete = TRUE) +
        geom_line(size=1.2) +
        guides(fill=guide_legend(title=NULL)) +
        xlab("Data") + ylab(if (type == "inflation") "Inflacja" else "Oprocentowanie depozytów") +
        scale_x_date(date_labels = "%b-%Y") +
        ggtitle(title) +
        theme_ipsum() +
        theme(
          legend.title = element_blank(),
          legend.text = element_text(size=12, face="bold"),
          plot.title = element_text(size=18),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
        )
}

plot_actual_increase_lines <- function(data, title, type, start_value=100) {
  data |>
    ggplot(aes(x=Period, y=if (type == "real_value") real_value else simple_value, group=country, color=country)) +
    scale_color_viridis(discrete = TRUE) +
    geom_line(size=1.2) +
    geom_hline(yintercept=start_value, color="#aa1836", size=0.75, linetype="dashed") +
    guides(fill=guide_legend(title=NULL)) +
    xlab("Data") + ylab("Wartość") +
    scale_x_date(date_labels = "%b-%Y") +
    ggtitle(title) +
    theme_ipsum() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=12, face="bold"),
      plot.title = element_text(size=18),
      axis.title.x = element_text(size=12, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
    )
}

plot_actual_increase_lines_two_types <-  function(data, title1, title2) {
  ggarrange(plot_actual_increase_lines(data, title1, "simple_value"),
            plot_actual_increase_lines(data, title2, "real_value"),
            ncol = 1, nrow = 2)
}


plot_histogram_diff_single <- function(data, title) {
  for (c in unique(data$zone)) {
    sub_data <- data |> filter(zone == c)
    mean_val <- mean(sub_data$difference, na.rm=TRUE)
    sd_val <- sd(sub_data$difference, na.rm=TRUE)
    plot <- sub_data |>
      ggplot(aes(difference, group=1, label=c)) +
      geom_histogram(aes(y=..density..), binwidth=0.07, alpha=0.7, fill="#DCE319FF", color="#95D840FF") +
      # geom_density(alpha = 0.2, fill="#481567FF", color="#8b1a89") +
      stat_function(fun = dnorm, n = 101, args = list(mean = mean_val, sd = sd_val), color="#aa1836", size=1) +
      ggtitle(label=paste(title, " - ", c), subtitle=paste("Rozkład normalny N(", signif(mean_val, 4), ", ", signif(sd_val*sd_val, 4), ")")) +
      xlab("Różnica inflacji") + ylab( "Gęstość") +
      theme_ipsum() +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size=12, face="bold"),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
      )
    print(plot)
  }
}

plot_histogram_single <- function(data, title) {
  for (c in unique(data$origin)) {
    sub_data <- data |> filter(origin == c)
    mean_val <- mean(sub_data$inflation, na.rm=TRUE)
    sd_val <- sd(sub_data$inflation, na.rm=TRUE)
    plot <- sub_data |>
      ggplot(aes(inflation, group=1, label=c)) +
      geom_histogram(aes(y=..density..), binwidth=0.3, alpha=0.7, fill="#DCE319FF", color="#95D840FF") +
      # geom_density(alpha = 0.15, fill="#481567FF", color="#8b1a89") +
      # stat_function(fun = dnorm, n = 101, args = list(mean = mean_val, sd = sd_val), color="#aa1836", size=1) +
      # ggtitle(label=paste(title, " - ", c), subtitle=paste("Rozkład normalny N(", signif(mean_val, 4), ", ", signif(sd_val*sd_val, 4), ")")) +
      ggtitle(label=paste(title, " - ", c)) +
      xlab("Poziom inflacji") + ylab( "Gęstość") +
      theme_ipsum() +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size=12, face="bold"),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
      )
    print(plot)
  }
}