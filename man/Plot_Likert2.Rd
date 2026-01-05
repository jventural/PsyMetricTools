\name{Plot_Likert2}
\alias{Plot_Likert2}
\title{Likert Scale Plot with Symmetric Centering and Customizable Text Display}

\description{
This function generates a centered Likert scale plot for visualizing the distribution of responses across items. The plot displays proportions symmetrically from the center, with the option to customize text display and color contrast for improved readability.
}

\usage{
Plot_Likert2(data, prefix, labels, palette, text_size,
             threshold, show_text, item_range)
}

\arguments{
  \item{data}{A dataframe containing the survey data, where the columns represent items prefixed with a common string (e.g., "IS").}
  \item{prefix}{A string representing the prefix used in item labels, typically indicating the start of item columns (e.g., "IS").}
  \item{labels}{A character vector providing descriptive labels for the Likert scale levels.}
  \item{palette}{A string specifying the color palette to be used for the plot, typically a name recognized by \code{scale_fill_brewer}.}
  \item{text_size}{A numeric value that determines the size of the percentage text displayed within the bars.}
  \item{threshold}{A numeric value that sets the minimum percentage value required for displaying text within a bar. Percentages below this threshold will not display text.}
  \item{show_text}{A logical value (TRUE or FALSE) that controls whether percentage text is displayed on the plot.}
  \item{item_range}{An integer vector specifying the range of item numbers to include in the plot, allowing for selective plotting of specific items.}
}

\details{
The function transforms the input dataframe into a long format suitable for plotting, calculates the proportions for each Likert scale response, and then generates a horizontally centered bar plot. The bars extend symmetrically from the center (0%) to emphasize the distribution of positive and negative responses.

Text within the bars can be displayed based on user-defined thresholds to avoid cluttering the plot. Additionally, text color is automatically adjusted to ensure contrast with the bar's background, with darker bars displaying white text for better visibility.

The generated plot provides a clear visual summary of item responses, with customization options to tailor the appearance and focus on specific items or responses.

The function returns the plot as a ggplot object, allowing further customization or saving using \code{ggsave}.
}

\value{
A \code{ggplot} object representing the centered Likert scale plot. The plot includes:
\itemize{
  \item Proportions of each Likert scale response, symmetrically displayed from the center.
  \item Items arranged vertically, ordered by the specified item range.
  \item Optionally displayed percentage text within the bars, with adjustable size and color for readability.
}
}

\examples{
# Example usage of the Plot_Likert2 function

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Example dataframe (replace with actual survey data)
df_filtered <- data.frame(
  IS1 = sample(1:5, 100, replace = TRUE),
  IS2 = sample(1:5, 100, replace = TRUE),
  IS3 = sample(1:5, 100, replace = TRUE),
  IS4 = sample(1:5, 100, replace = TRUE)
)

# Generate the Likert plot
figure1 <- Plot_Likert2(df_filtered,
                        prefix = "IS",
                        labels = c("NO! Not at all like me", "2",
                   "Something similar to my", "4",
                   "Yes! very much like me"),
                        palette = "Blues",
                        text_size = 2.5,
                        threshold = 10,
                        show_text = TRUE,  # Set to FALSE to hide text
                        item_range = 1:4)  # Specify the range of items

# Save the plot
ggsave(filename = "Output/Figura1_Likert.jpg", plot = figure1,
       height = 5, width = 8, dpi = 600, units = "in")
}

\author{
Dr. José Ventura-León
}
