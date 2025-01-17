% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objects.R
\name{IfElse}
\alias{IfElse}
\title{Create a skip pattern with two branches}
\usage{
IfElse(cond, if_block, else_block)
}
\arguments{
\item{cond}{A Boolean expression, usually referencing responses to
previous questions or results of previous calculations.  E.g.,
'bell_pepper_count > 0'.}

\item{if_block}{A vector of calls to 'Ask', 'Note', etc., that are
displayed in the survey if the expression in 'cond' is TRUE.}

\item{else_block}{A vector of calls to 'Ask', 'Note', etc., that are
displayed in the survey if the expression in 'cond' is FALSE.}
}
\description{
Create a skip pattern with two branches
}
\examples{
Survey(
  form_id = pepper_survey,
  form_version = auto,
  form_title = "Survey on Peppers",
  Ask(
    bell_pepper_count,
    Integer(),
    "How many bell peppers have you eaten in the past week?"
  ),
  IfElse(
    bell_pepper_count > 0,
    # This is 'if_block':
    c(
      Note(
        color_note,
        "How many bell peppers have you eaten that were ..."
      ),
      Ask(red_bell_pepper_count, "... red?"),
      Ask(green_bell_pepper_count, "... green?"),
      Ask(yellow_bell_pepper_count, "... yellow?")
    ),
    # This is 'else_block':
    Ask(
      non_bell_pepper_count,
      Integer(),
      "How many peppers have you eaten that were not bell peppers?"
    )
  )
)
}
