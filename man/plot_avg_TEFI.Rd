\name{plot_avg_TEFI}
\alias{plot_avg_TEFI}
\title{Average TEFI Visualization Tool}

\description{
This function visualizes the median values of the TEFI metric across different sample sizes, aggregated by algorithm and correlation method. It generates line and point plots to clearly display the trends in TEFI values as functions of sample size, differentiated by algorithm types and their corresponding correlation methods. This visualization tool is instrumental for assessing the stability and consistency of TEFI measures under various conditions in network analysis.
}

\usage{
plot_avg_TEFI(results)
}

\arguments{
  \item{results}{A data frame containing TEFI metrics along with associated algorithm types, correlation methods, and sample sizes. This data frame should include the columns `TEFI`, `Algorithm`, `Correlation_Method`, and `Sample_Size`.}
}

\details{
The function utilizes the `ggplot2` package for constructing the visualizations. It begins by summarizing the input data to calculate the median of TEFI for each combination of algorithm, correlation method, and sample size. The summarized data is then plotted using both line and point geometries in `ggplot2`, allowing for the visual tracing of trends across varying sample sizes. The use of color and shape aesthetics enhances the differentiation between algorithms, while the facet wrapping by correlation method provides a structured comparison across different analytical conditions.
}

\value{
A ggplot object representing the line and point plots of median TEFI values, arranged by algorithm and correlation method across different sample sizes. This output facilitates a comprehensive visual assessment of TEFI variations, aiding in the evaluation of algorithm performance and methodological consistency.
}

\examples{
# Assuming `results` is a data frame containing the necessary columns
plot <- plot_avg_TEFI(results)
print(plot)
}

\author{
José Ventura-León
}
