% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\name{Choice}
\alias{Choice}
\title{Create an item in a choice list}
\usage{
Choice(value, label, ...)
}
\arguments{
\item{value}{A numeric or string value that is unique to the item in
the choice list.}

\item{label}{A string that describes the choice item.  This will be
displayed in the survey.}

\item{...}{Additional arguments, e.g., 'filter'.  Once
'write_xlsform' is called, these arguments will be included as
additional columns in the 'choices' worksheet of the resulting
XLSForm output.}
}
\description{
This function creates a choice item.  It should be
called in the arguments passed to the function 'ChoiceList'.
}
\examples{
ChoiceList(
  soybean_foods,
  Choice(1, "Steamed edamame", filter = "not_tofu"),
  Choice(2, "Firm tofu", filter = "tofu"),
  Choice(3, "Soft tofu", filter = "tofu"),
  Choice(4, "Silken tofu", filter = "tofu"),
  Choice(5, "Frozen tofu", filter = "tofu"),
  Choice(6, "Egg tofu", filter = "tofu"),
  Choice(7, "Soy milk", filter = "not_tofu"),
  Choice(8, "Miso paste", filter = "not_tofu"),
  Choice(99, "Other", filter = "other")
)
}
