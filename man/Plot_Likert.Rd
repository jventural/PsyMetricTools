\name{Plot_Likert}
\alias{Plot_Likert}
\title{Likert Scale Item Visualization Tool}

\description{
This function creates a series of bar charts for visualizing responses to Likert-scale items. It is designed to handle multiple items across a Likert scale, providing a clear graphical representation of response distributions for each item. This visualization is particularly useful for surveys and questionnaires, facilitating easy interpretation of response patterns and frequencies.
}

\usage{
Plot_Likert(Data, name_items, ranges, exclude = NULL, text_size = 3)
}

\arguments{
  \item{Data}{The data frame containing the Likert scale responses. Each column represents a different item or variable.}
  \item{name_items}{The base name or prefix of the item variables.}
  \item{ranges}{A numeric range specifying the suffixes of item names. For example, `1:25` for items named from 'item1' to 'item25'.}
  \item{exclude}{Optional. A vector of item suffixes to be excluded from the plot. For example, `c(1, 2)` to exclude 'item1' and 'item2'.}
  \item{text_size}{It consists of modifying the size of the percentages within the bar chart.}
}

\details{
The function first constructs the full names of the item variables by combining `name_items` with the numbers specified in `ranges`. It then reshapes the data into a long format suitable for plotting with `ggplot2`. Excluded items, if any, are removed from the plot. Each variable is represented as a separate bar chart, showing the frequency of each response level.
}

\value{
A ggplot object representing the bar charts for the specified Likert scale items. This object can be further modified or directly plotted using ggplot2 functions.
}

\examples{
# Assuming 'Data' is a data frame with Likert scale items named 'CCOV1', 'CCOV2', ..., 'CCOV25'
plot <- Plot_Likert(Data, "CCOV", 1:25, exclude = c(1, 2), text_size = 3)
print(plot)
}

\author{
Dr. José Ventura-León
}
