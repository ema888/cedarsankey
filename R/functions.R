#' Process data
#'
#' This function takes a dataframe as an input. It assumes that the first column
#' of the dataframe is the row names and the rest of the data in the remaining
#' columns contains the risk values over time, with years as the column names.
#'
#' @param df the dataframe you wish to use
#' @return a new dataframe in the format to create the sankey later on
#' @export
process_data <- function(df){

  risk_data <- data.frame()

  # Turn first column into rownames
  column_1 <- colnames(df)[1]
  risk_data <- (tibble::column_to_rownames(df, var = column_1))

  # Transform the data
  risk_data <- as.data.frame(t(risk_data))

  # Turn the row names (now the years into a column named 'year')
  years <- rownames(risk_data)
  risk_data <- tibble::rownames_to_column(risk_data, var = "year")

  # Pivot longer to list out all combinations
  risk_data <- risk_data %>%
    tidyr::pivot_longer(colnames(risk_data)[c(2:length(colnames(risk_data)))], names_to = "risk_factors")

  # Factorize year and risk factors
  risk_data <- risk_data %>%
    dplyr::mutate(year = factor(year, levels = years),
           risk_factors = as.factor(risk_factors))

  # Assign rank to data by year (ordering the risk_factors by value for each year)
  risk_data <- risk_data %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(rank = rank(-value, ties.method = "first"))

  # Add a graph_value column so that when dataset is used, the values closer to 0 can also be visualized
  risk_data <- risk_data %>%
    dplyr::mutate(graph_value = dplyr::case_when(
      value < 0.01 ~ value * 25,
      value < 0.02 & value >= 0.01 ~ value * 2.5,
      value >= 0.02 ~ value
    ))

  # Add in more values on where to draw vertical line later on
  risk_data <- risk_data %>%
    dplyr::group_by(rank) %>%
    dplyr::mutate(rank_year = rank(as.numeric(year))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x1 = 1.35 + (rank_year - 1),
           x2 = 1.63 + (rank_year - 1))

  # I want to set the last rank vertical line values to NA
  last_rank <- max(risk_data$rank_year)
  risk_data <- risk_data %>%
    dplyr::mutate(x1 = ifelse(rank_year == last_rank, NA, x1),
           x2 = ifelse(rank_year == last_rank, NA, x2))

  # Return new data frame
  return(risk_data)
}




#' Create Visualization
#'
#' This function takes a dataframe as an input. It assumes that the data in the
#' columns contains the risk values over time, with years as the column names and
#' risk factors as the row names.
#'
#' @param df the dataframe you wish to use
#' @param horizontal takes a TRUE or FALSE argument for whether you want horizontal spacing in your sankey chart
#' @param vertical takes a TRUE or FALSE argument for whether you want vertical spacing in your sankey chart
#' @param round takes a numeric argument for what value you want to round your labels to
#' @param lab_col take a vector for the colors you want for your risk factors (in alphabetical order of your risk factors)
#' @return a ggplot of your sankey chart
#' @export
create_viz <- function(df, horizontal = FALSE, vertical = FALSE, round = 2, lab_col = c("#da654f", "#d78f54", "#8aba8f", "#4b6b91", "#7eacc5")){
  risk_data <- process_data(df)

  # create label order used for labeling y-axis later
  rank_order <- order(risk_data[risk_data$year == levels(risk_data$year)[1], ]$rank)
  label_order <- risk_data[rank_order, ]

  label_order <- label_order %>%
    dplyr::arrange(dplyr::desc(rank)) %>%
    dplyr::mutate(cum_sum = cumsum(value))

  label_order <- label_order %>%
    dplyr::mutate(y_pos = dplyr::lag(cum_sum, default = 0) + value/2)

  # how to round the labels
  form <- paste0("%.", round, "f")
  risk_data$rounded_val <- sprintf(form, risk_data$value)

  # create the vertical white lines
  vertical_segments <- list(ggplot2::geom_segment(
    ggplot2::aes(x = x1,
                 xend = x1,
                 y = 0,
                 yend = 1.4),
    color = "white",
    size = 0.5),
    ggplot2::geom_segment(
      ggplot2::aes(x = x2,
                   xend = x2,
                   y = 0,
                   yend = 1.4),
      color = "white",
      size = 0.5))

  # find the largest stacked bar
  sum_risk <- risk_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(size = sum(value))
  y_max <- max(sum_risk$size) + 0.1

  # geom_blank is empty layer
  v <- ggplot2::geom_blank()

  # if else statement
  if(vertical == TRUE){
    v <- vertical_segments
  }else(
    v <- v
  )

  # horizontal segments
  h <- ifelse(horizontal == TRUE, 2, 0)

  main_plot <- risk_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = year,
                   y = graph_value,
                   alluvium = risk_factors,
                   stratum = rank,
                   label = rounded_val)
    ) +
    ggalluvial::geom_alluvium(data = risk_data,
                              ggplot2::aes(fill = risk_factors),
                              size = h,
                              color = "white",
                              width = 0.75,
                              alpha = 1,
                              curve_type = "linear") +
    ggplot2::scale_y_continuous(limits = c(0, y_max)) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Risk Contributors to Stroke in Blacks") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12, hjust = 0.5),
      plot.title = ggplot2::element_text(size = 20, hjust = 0.5, vjust = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 2, r = 1, b = 0.5, l = 2, unit = "cm")
    ) +
    ggplot2::annotate("text",
                      x = 0,
                      y = label_order$y_pos,
                      label = label_order$risk_factors) +
    ggplot2::coord_cartesian(clip = 'off',
                             ylim = c(0, y_max)) +
    ggplot2::geom_label(data = risk_data,
                        ggplot2::aes(fill = risk_factors),
                        stat = "stratum",
                        size = 3,
                        color = "white",
                        fontface = "bold",
                        label.size = 0) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title = "Legend Title",
        override.aes = ggplot2::aes(label = "")
      )) +
    ggplot2::scale_fill_manual(values = lab_col)

  # create a new plot based on ifelse statements
  new_plot <- main_plot + v

  # return plot
  return(new_plot)
}
