\name{rename_items}
\alias{rename_items}
\title{Item Renaming Tool for Survey Data}

\description{
This function is designed to systematically rename survey or test items in a dataset to standardized abbreviations. It is ideal for preparing data for analysis by converting verbose item phrasings to concise, identifiable labels. The function allows for the specification of different prefixes for separate sections or tests within the dataset and can rename a specified range of items based on their current names or positions.
}

\usage{
rename_items(df, prefix1 = "CDE", prefix2 = "A", inici = "ItemStartPhrase", final = "ItemEndPhrase", n_items1 = 23, n_items2 = 10)
}

\arguments{
  \item{df}{The data frame containing the items to be renamed.}
  \item{prefix1}{Prefix for the items of the first test or section.}
  \item{prefix2}{Prefix for the items of the second test or section.}
  \item{inici}{The phrasing or name of the first item to be renamed, or the column name where renaming should start.}
  \item{final}{The phrasing or name of the last item to be renamed, or the column name where renaming should end.}
  \item{n_items1}{Number of items in the first section to be renamed.}
  \item{n_items2}{Number of items in the second section to be renamed.}
}

\details{
The function identifies the range of columns to be renamed based on 'inici' and 'final' arguments. It then generates new names using the specified prefixes and sequential numbers. The user can specify the number of items for each prefix or let the function divide them equally. It ensures that the renaming does not exceed the actual number of columns in the dataset.
}

\value{
A data frame with the specified items renamed according to the provided prefixes. The renaming respects the order and the number of items as specified.
}

\examples{
# Assuming 'df_new' is a data frame with verbose item names
df_new_renamed <- rename_items(df_new,
                               prefix1 = "CDE",
                               prefix2 = "A",
                               inici = "1. Me siento desprotegido(a) cuando estoy solo(a).",
                               final = "10. A veces pienso que no sirvo para nada",
                               n_items1 = 23,
                               n_items2 = 10)
}

\author{
Professor José Ventura León
}

