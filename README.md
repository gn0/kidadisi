# Kidadisi: build questionnaires by writing R code

Kidadisi is an R package to build survey definitions and use those definitions to generate [XLSForm](https://xlsform.org/) questionnaires.
XLSForm is a portable format that is understood by both [SurveyCTO](https://www.surveycto.com/) and [KoboToolbox](https://www.kobotoolbox.org/).
The goals of Kidadisi are:

- **Modularity.**
  - _Reuse choice lists._  Research projects often roll out multiple questionnaires that use the same choice lists, so that each questionnaire codes answers the same way.  You copy and paste your choice lists, but later realize that you need to add a choice item or change the phrasing of a choice label.  Rather than having to manually edit each questionnaire, define choice lists once instead and include the same definitions in each questionnaire.
  - _Reuse questions._  Sometimes the same question or series of questions is asked multiple times in slightly different ways in a questionnaire.  For example, phone numbers with data validation, or multiple-step preloads.  You copy and paste your questions, but later realize that you want to make a change to the phrasing or to the constraints on the answer.  Rather than having to sift through multiple occurrences of the question in order to make sure that they are consistent with each other, define the question as a function instead.  The function can hide the common elements of each occurrence and it lets you focus on those elements that are different.
- **Early error reporting.**  Don't wait for the server to tell you that you made mistakes.  Let R tell you if you made a typo in a variable or choice list name, or if you used the same variable name twice.
- **Amenability to version control.**  Use Git or your favorite version control system to track changes to your questionnaires.

The spiritual predecessor of this package is [Honeybee](https://github.com/gn0/honeybee/) which transpiled survey definitions, written in a domain-specific language, into XLSForm.
Unlike Honeybee, Kidadisi aims to work with tooling such as [RStudio](https://posit.co/products/open-source/rstudio/) which research teams may already be familiar with.
However, Kidadisi works in any R environment and by no means requires RStudio.

## Table of contents

1. [How to install](#how-to-install)
2. [Example](#example)
3. [Useful resources](#useful-resources)

## How to install

Open the R console and enter the following:

```r
install.packages("devtools")
devtools::install_git(url = "https://github.com/gn0/kidadisi/")
```

## Example

```r
form_definition <- Survey(
  form_id = foo_v1,
  form_version = auto,
  form_title = "Foo v1",
  ChoiceList(
    yes_no,
    Choice(1, "Yes"),
    Choice(0, "No")
  ),
  Ask(name, Text(), "First and last name:"),
  Ask(
    vegetables,
    SelectOne(yes_no),
    "Have you eaten your vegetables?"
  ),
  When(
    vegetables == 1,
    Ask(
      vegetables_red,
      SelectOne(yes_no),
      "Have you eaten any red vegetables?",
      hint = "Tomatoes are technically fruits."
    ),
    When(
      vegetables_red == 1,
      Ask(
        vegetables_red_count,
        Integer(),
        "How many kinds of red vegetables have you eaten?",
        constraint = . > 0 & . < 100
      )
    )
  )
)

form_definition |> write_xlsform("foo_v1.xlsx")
```

The resulting survey will render like this with KoboToolbox:

<img src="./examples/screenshot.png" alt="Screenshot of the example survey" width="600" />

The output file, `foo_v1.xlsx`, will contain the following.
In the `survey` worksheet:

| name                 | type              | label                                            | required | hint                             | relevance                                         | constraint          |
|----------------------|-------------------|--------------------------------------------------|----------|----------------------------------|---------------------------------------------------|---------------------|
| name                 | text              | First and last name:                             | yes      |                                  |                                                   |                     |
| vegetables           | select_one yes_no | Have you eaten your vegetables?                  | yes      |                                  |                                                   |                     |
| vegetables_red       | select_one yes_no | Have you eaten any red vegetables?               | yes      | Tomatoes are technically fruits. | `${vegetables} = 1`                               |                     |
| vegetables_red_count | integer           | How many kinds of red vegetables have you eaten? | yes      |                                  | `(${vegetables} = 1) and (${vegetables_red} = 1)` | `. > 0 and . < 100` |

In the `choices` worksheet:

| list_name | value | label |
|-----------|-------|-------|
| yes_no    | 1     | Yes   |
| yes_no    | 0     | No    |


In the `settings` worksheet:

| form_id | form_version | form_title |
|---------|--------------|------------|
| foo_v1  | 2309191945   | Foo v1     |

## Useful resources

KoboToolbox:

- [Restricting Text Responses With Regular Expressions](https://support.kobotoolbox.org/restrict_responses.html)
- [Custom Formatting in Web Forms](https://support.kobotoolbox.org/custom_format_web.html)
- [How to create a dropdown list in KoboToolbox as seen in Excel?](https://community.kobotoolbox.org/t/how-to-create-a-drop-down-list-in-kobotoolbox-as-seen-in-excel/943)

SurveyCTO:

- [Using expressions in your forms: a reference for all operators and functions](https://docs.surveycto.com/02-designing-forms/01-core-concepts/09.expressions.html)
- [Pre-loading data into a form](https://docs.surveycto.com/02-designing-forms/03-advanced-topics/03.preloading.html)
- [Loading multiple-choice options from pre-loaded data](https://docs.surveycto.com/02-designing-forms/03-advanced-topics/04.search-and-select.html)

