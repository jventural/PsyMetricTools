\name{smote_multiclass}
\alias{smote_multiclass}
\title{Synthetic Minority Over-sampling Technique for Multi-Class Imbalance}

\description{
This function implements the Synthetic Minority Over-sampling Technique (SMOTE) for datasets with multiple classes. It is particularly useful for addressing class imbalance issues in multi-class classification problems. The function generates synthetic samples for minority classes based on the specified over-sampling percentage and nearest neighbors, aiming to achieve a balanced distribution across all classes. An optional seed parameter is included to ensure reproducibility.
}

\usage{
smote_multiclass(data, outcome, perc_maj = 100, k = 5, seed = NULL)
}

\arguments{
  \item{data}{A data frame containing the features and the class label.}
  \item{outcome}{The name or column index of the target variable in the data frame, which contains the class labels.}
  \item{perc_maj}{Percentage of the majority class size to which the sizes of minority classes will be increased. Default is 100, meaning minority classes will be over-sampled to the size of the majority class.}
  \item{k}{The number of nearest neighbors to use for generating synthetic samples. Higher values will consider more neighbors for creating each synthetic sample.}
  \item{seed}{An optional seed value to set for ensuring reproducibility of the generated synthetic samples.}
}

\details{
The function works by identifying the majority class and then over-sampling the minority classes by creating synthetic samples. These samples are generated based on the k-nearest neighbors algorithm, where each synthetic sample is created by randomly choosing one of the k-nearest neighbors and then randomly interpolating between this neighbor and the original sample. This approach helps to create more diverse samples and reduces the risk of overfitting. Using a seed allows for the results to be reproduced.
}

\value{
A data frame with an improved class balance. The original data is augmented with synthetic samples for the minority classes, based on the specified `perc_maj`, `k`, and `seed` values.
}

\examples{
# Assuming 'df' is a DataFrame with class labels in the 'class_col' column
# To balance minority classes to the level of the majority class
balanced_data <- smote_multiclass(
  df,
  outcome = "class_col",
  perc_maj = 100,
  k = 5,
  seed = 123
)
print(balanced_data)
}

\author{
Dr. José Ventura-León
}
