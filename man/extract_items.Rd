\name{extract_items}
\alias{extract_items}
\title{Item Extraction Tool for Factor Analysis}

\description{
This function is designed to parse and extract factor items from a structured text input. It is particularly useful for organizing and identifying items associated with different factors in a psychological test or survey. The function reads through each line of the input text, identifying factor names and their corresponding items, and then compiles them into a structured list.
}

\usage{
extract_items(text, prefix = "")
}

\arguments{
  \item{text}{A vector of strings, where each string represents a line of text containing a factor name followed by its associated items. The factor name and items are expected to be separated by a colon (`:`).}
  \item{prefix}{An optional string prefix to be added to item names. It is used to prepend a specific text to item names, which can be useful in differentiating similar items across factors. Default is an empty string.}
}

\details{
The function iterates over each line in the `text` vector, splitting the line at the colon to separate the factor name from its items. Items are expected to be separated by commas. An optional prefix can be added to the items, which is particularly useful when dealing with multiple factors having similar item naming conventions.
}

\value{
A list where each element represents a factor. The names of the list elements are the factor names extracted from the text, and the values are vectors containing the names of the items associated with each factor.
}

\examples{
# Example text input
text_input <- c("Factor1: Item1, Item2, Item3", "Factor2: ItemA, ItemB, ItemC")

# Extract items
extracted_items <- extract_items(text_input)
print(extracted_items)
}

\author{
Professor José Ventura León
}
