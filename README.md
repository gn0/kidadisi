# Kidadisi: an R package for generating XLSForm surveys

Kidadisi is an R package for generating [XLSForm](https://xlsform.org/) surveys.
This is useful because [SurveyCTO](https://www.surveycto.com/) and [KoboToolbox](https://www.kobotoolbox.org/) both support XLSForm as their survey description format.

## How to install

Open the R console and enter the following:

```r
install.packages("devtools")
devtools::install_git(url = "https://codeberg.org/gnyeki/kidadisi/")
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
  If(
    vegetables == 1,
    c(
      Ask(
        vegetables_red,
        SelectOne(yes_no),
        "Have you eaten any red vegetables?",
        hint = "Tomatoes are technically fruits."
      ),
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

The output file, `foo_v1.xlsx`, will contain the following.
In the `survey` worksheet:

| name                 | type              | label                                            | required | hint                             | relevance         | constraint        |
|----------------------|-------------------|--------------------------------------------------|----------|----------------------------------|-------------------|-------------------|
| name                 | text              | First and last name:                             | yes      |                                  |                   |                   |
| vegetables           | select_one yes_no | Have you eaten your vegetables?                  | yes      |                                  |                   |                   |
| vegetables_red       | select_one yes_no | Have you eaten any red vegetables?               | yes      | Tomatoes are technically fruits. | ${vegetables} = 1 |                   |
| vegetables_red_count | integer           | How many kinds of red vegetables have you eaten? | yes      |                                  | ${vegetables} = 1 | . > 0 and . < 100 |

In the `choices` worksheet:

| list_name | value | label |
|-----------|-------|-------|
| yes_no    | 1     | Yes   |
| yes_no    | 0     | No    |


In the `settings` worksheet:

| form_id | form_version | form_title |
|---------|--------------|------------|
| foo_v1  | 2309191945   | Foo v1     |


