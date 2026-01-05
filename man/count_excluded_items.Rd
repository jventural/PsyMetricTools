\name{count_excluded_items}
\alias{count_excluded_items}
\title{Count of Excluded Items Across Iterations}

\description{
  The `count_excluded_items` function analyzes the results from an iterative EFA model refinement process to identify and count the frequency of items excluded across all iterations. It provides insights into which items were most frequently problematic or had significant cross-loadings, contributing to decisions on item revision or elimination in scale development and validation processes.
}

\usage{
count_excluded_items(res)
}

\arguments{
  \item{res}{A list containing the results of the iterative EFA model refinement process, specifically including a `processed_results` component that stores details of each iteration, including excluded items.}
}

\details{
  This function parses the `processed_results` from the output of an iterative EFA modeling process to compile a list of all items excluded over the course of model refinement iterations. It then counts the frequency of each item's exclusion, providing a clear picture of which items are consistently problematic. This information can be crucial for researchers looking to refine their measurement instruments by either revising or excluding items that detract from the model's clarity and interpretability.
}

\value{
  Returns a dataframe sorted in descending order of exclusion frequency. Each row corresponds to an item, with columns for the item name and its count of exclusions across iterations, facilitating an easy review of which items were most problematic.
}

\examples{
# Assuming 'iterativeEFAresult' contains the output from 'iterativeModelEFA'
resultCount <- count_excluded_items(iterativeEFAresult)
print(resultCount)  # Display the count of excluded items
}

\author{
Dr. José Ventura-León
}
