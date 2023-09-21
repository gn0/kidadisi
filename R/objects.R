require(rlang)

#' Postpone the evaluation of expression-valued arguments
#'
args <- function(...) {
    rlang::enquos(...)
}

#' Create a survey definition
#'
#' @description This function creates a survey definition.  It can be
#'     combined with the functions 'ChoiceList', 'Ask', 'Note',
#'     'Calculate', 'CalculateHere', 'When', 'IfElse', 'Group',
#'     'TimedGroup', and 'Repeat' in order to specify the survey.
#' @param form_id Form identifier.
#' @param form_version Form version.  It must either be a positive
#'     integer or the phrase "auto".  If it is set to "auto", an
#'     automatic version number is generated based on the current date
#'     and time in UTC.
#' @param form_title Form title.
#' @param ... Form contents created using the functions 'ChoiceList',
#'     'Ask', 'Note', 'Calculate', 'CalculateHere', 'When', 'IfElse',
#'     'Group', 'TimedGroup', and 'Repeat'.
#' @return A list object that can be processed by the function
#'     'write_xlsform'.
#' @examples
#' form_definition <- Survey(
#'   form_id = foo_v1,
#'   form_version = auto,
#'   form_title = "Foo v1",
#'   ChoiceList(
#'     yes_no,
#'     Choice(1, "Yes"),
#'     Choice(0, "No")
#'   ),
#'   Ask(name, Text(), "First and last name:"),
#'   Ask(
#'     vegetables,
#'     SelectOne(yes_no),
#'     "Have you eaten your vegetables?"
#'   ),
#'   When(
#'     vegetables == 1,
#'     Ask(
#'       vegetables_red,
#'       SelectOne(yes_no),
#'       "Have you eaten any red vegetables?",
#'       hint = "Tomatoes are technically fruits."
#'     ),
#'     When(
#'       vegetables_red == 1,
#'       Ask(
#'         vegetables_red_count,
#'         Integer(),
#'         "How many kinds of red vegetables have you eaten?",
#'         constraint = . > 0 & . < 100
#'       )
#'     )
#'   )
#' )
#'
#' @export
Survey <- function(form_id, form_version, form_title, ...) {
    list(
        .type = "survey",
        form_id = rlang::enquo(form_id),
        form_version = rlang::enquo(form_version),
        form_title = form_title,
        block = c(...)
    )
}

#' Create a choice list
#'
#' @description This function creates a choice list that can be
#'     referenced by questions of type 'SelectOne' or 'SelectMultiple'.
#'     It should be called in the arguments passed to the function
#'     'Survey'.
#' @param name Unique choice list name.
#' @param ... Choices created using the function 'Choice'.
#' @examples
#' form_definition <- Survey(
#'   form_id = foo_v1,
#'   form_version = auto,
#'   form_title = "Foo v1",
#'   ChoiceList(
#'     yes_no,
#'     Choice(1, "Yes"),
#'     Choice(0, "No")
#'   ),
#'   ChoiceList(
#'     state,
#'     Choice(1, "Kano", filter = 1),
#'     Choice(2, "Sokoto", filter = 1),
#'     Choice(3, "Enugu", filter = 0),
#'     Choice(4, "Oyo", filter = 0)
#'   ),
#'   Ask(
#'     state_northern,
#'     SelectOne(yes_no),
#'     "Is this state in the North?"
#'   ),
#'   Ask(
#'     state,
#'     SelectOne(state),
#'     "Which state is it?",
#'
#'     # NOTE We need to pass the argument to 'choice_filter' as a
#'     # string rather than as an expression.  This is because the
#'     # expression parser cannot distinguish between 'filter' and
#'     # 'state_northern', so it does not know that we want to leave
#'     # 'filter' unchanged but rewrite the 'state_northern' as
#'     # '${state_northern}'.
#'     #
#'     choice_filter = "filter = ${state_northern}"
#'   )
#' )
#' @export
ChoiceList <- function(name, ...) {
    list(list(
        .type = "choice_list",
        name = rlang::enquo(name),
        block = c(...)
    ))
}

#' Create an item in a choice list
#'
#' @description This function creates a choice item.  It should be
#'     called in the arguments passed to the function 'ChoiceList'.
#' @export
Choice <- function(value, label, ...) {
    list(list(
        .type = "choices_row",
        value = value,
        label = label,
        args = args(...)
    ))
}

#' Create a skip pattern with one branch
#'
#' @export
When <- function(cond, ...) {
    list(list(
        .type = "when",
        cond = rlang::enquo(cond),
        block = c(...)
    ))
}

#' Create a skip pattern with two branches
#'
#' @export
IfElse <- function(cond, if_block, else_block) {
    list(list(
        .type = "ifelse",
        cond = rlang::enquo(cond),
        if_block = if_block,
        else_block = else_block
    ))
}

#' @export
Match <- function(name, ...) {
    list(list(.type = "match", ...))
}

#' Create a group
#'
#' @export
Group <- function(name, label, block, ...) {
    list(list(
        .type = "group",
        name = rlang::enquo(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

#' Create a group with a measure of survey duration
#'
#' @export
TimedGroup <- function(name, label, block, ...) {
    name <- rlang::enquo(name) |> parse_identifier()

    start_name <- paste0(name, "_start")
    end_name <- paste0(name, "_end")
    dur_name <- paste0(name, "_dur")

    c(CalculateHere({{ start_name }}, once(duration())),
      Group(name, label, block, ...),
      CalculateHere({{ end_name }}, once(duration())),
      Calculate({{ dur_name }}, {{ end_name }} - {{ start_name }}))
}

#' Create a repeat group
#'
#' @export
Repeat <- function(name, label, block, ...) {
    list(list(
        .type = "repeat",
        name = rlang::enquo(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

#' Create a question
#'
#' @description This function creates a question.  It should be called
#'     in the arguments passed to the function 'Survey'.
#' @param name Unique question name.
#' @param type The type of the question.  One of 'Text()', 'Integer()',
#'     'Decimal()', 'Range()', 'Geopoint()', 'Date()', 'Time()',
#'     'DateTime()', 'SelectOne(...)', and 'SelectMultiple(...)'.
#' @param label A string that states the question that we want to
#'     display in the survey.
#' @param ... Other named arguments, e.g., 'hint', 'constraint',
#'     'required', 'choice_list'.
#' @export
Ask <- function(name, type, label, ...) {
    list(list(
        .type = "survey_row",
        name = rlang::enquo(name),
        type = type,
        label = label,
        args = args(...)
    ))
}

#' Specify the "integer" type for a question created with 'Ask'
#'
#' @export
Integer <- function() {
    list("integer")
}

#' Specify the "decimal" type for a question created with 'Ask'
#'
#' @export
Decimal <- function() {
    list("decimal")
}

#' Specify the "range" type for a question created with 'Ask'
#'
#' @examples
#' Ask(
#'   tomato_intensity,
#'   Range(),
#'   "How much do you like tomatoes?",
#'   parameters = "start=1 end=5 step=1"
#' )
#' @export
Range <- function() {
    list("range")
}

#' Specify the "text" type for a question created with 'Ask'
#'
#' @examples
#' Ask(name_first, Text(), "What is your first name?")
#'
#' @export
Text <- function() {
    list("text")
}

#' Specify the "select_one" type for a question created with 'Ask'
#'
#' @examples
#' Ask(
#'   paprika,
#'   SelectOne(yes_no),
#'   "Have you consumed any paprika in the past 24 hours?"
#' )
#' @export
SelectOne <- function(list_name) {
    list("select_one", rlang::enquo(list_name))
}

#' Specify the "select_multiple" type for a question created with 'Ask'
#'
#' @examples
#' # Define a choice list with various cabbages.
#' #
#' ChoiceList(
#'   cabbage_species,
#'   Choice(1, "Bok choy"),
#'   Choice(2, "Broccoli"),
#'   Choice(3, "Brussels sprouts"),
#'   Choice(4, "Cauliflower"),
#'   Choice(5, "Choy sum"),
#'   Choice(6, "Kohlrabi"),
#'   Choice(7, "Napa cabbage"),
#'   Choice(8, "Rutabaga"),
#'   Choice(9, "Savoy cabbage"),
#'   Choice(10, "Turnip"),
#'   Choice(99, "Other")
#' )
#'
#' # Multiple-choice question about cabbages.
#' #
#' Ask(
#'   favorite_cabbage,
#'   SelectMultiple(cabbage_species),
#'   "Which are your favorite cabbages?"
#' )
#' @export
SelectMultiple <- function(list_name) {
    list("select_multiple", rlang::enquo(list_name))
}

#' Specify the "date" type for a question created with 'Ask'
#'
#' @export
Date <- function() {
    list("date")
}

#' Specify the "datetime" type for a question created with 'Ask'
#'
#' @export
DateTime <- function() {
    list("datetime")
}

#' Specify the "time" type for a question created with 'Ask'
#'
#' @export
Time <- function() {
    list("time")
}

#' Specify the "geopoint" type for a question created with 'Ask'
#'
#' @export
Geopoint <- function() {
    list("geopoint")
}

#' Create a "note" field
#'
#' @export
Note <- function(name, label, ...) {
    list(list(
        .type = "survey_row",
        name = rlang::enquo(name),
        type = "note",
        label = label,
        args = args(...)
    ))
}

#' Create a "calculate" field
#'
#' @export
Calculate <- function(name, calculation, ...) {
    list(list(
        .type = "survey_row",
        name = rlang::enquo(name),
        type = list("calculate"),
        calculation = rlang::enquo(calculation),
        args = args(...)
    ))
}

#' Create a "calculate_here" field
#'
#' @export
CalculateHere <- function(name, calculation, ...) {
    list(list(
        .type = "survey_row",
        name = rlang::enquo(name),
        type = list("calculate_here"),
        calculation = rlang::enquo(calculation),
        args = args(...)
    ))
}
