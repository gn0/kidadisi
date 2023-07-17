require("rlang")

#' Postpone the evaluation of arguments whose value is an expression.
#'
args <- function(...) {
    enquos(...)
}

Survey <- function(form_id, form_version, form_title, ...) {
    list(
        .type = "survey",
        form_id = enquo(form_id),
        form_version = enquo(form_version),
        form_title = form_title,
        block = c(...)
    )
}

ChoiceList <- function(name, ...) {
    list(list(
        .type = "choice_list",
        name = enquo(name),
        block = c(...)
    ))
}

Choice <- function(value, label, ...) {
    list(list(
        .type = "choices_row",
        value = value,
        label = label,
        args = args(...)
    ))
}

If <- function(cond, block) {
    list(list(
        .type = "if",
        cond = enquo(cond),
        block = block
    ))
}

IfElse <- function(cond, if_block, else_block) {
    list(list(
        .type = "ifelse",
        cond = enquo(cond),
        if_block = if_block,
        else_block = else_block
    ))
}

Match <- function(name, ...) {
    list(list(.type = "match", ...))
}

Group <- function(name, label, block, ...) {
    list(list(
        .type = "group",
        name = enquo(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

TimedGroup <- function(name, label, block, ...) {
    name <- enquo(name) |> parse_identifier()

    start_name <- paste0(name, "_start")
    end_name <- paste0(name, "_end")
    dur_name <- paste0(name, "_dur")

    c(CalculateHere({{ start_name }}, once(duration())),
      Group(name, label, block, ...),
      CalculateHere({{ end_name }}, once(duration())),
      Calculate({{ dur_name }}, {{ end_name }} - {{ start_name }}))
}

Repeat <- function(name, label, block, ...) {
    list(list(
        .type = "repeat",
        name = enquo(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

Ask <- function(name, type, label, ...) {
    list(list(
        .type = "survey_row",
        name = enquo(name),
        type = type,
        label = label,
        args = args(...)
    ))
}

Integer <- function() {
    list("integer")
}

Text <- function() {
    list("text")
}

SelectOne <- function(list_name) {
    list("select_one", enquo(list_name))
}

SelectMultiple <- function(list_name) {
    list("select_multiple", enquo(list_name))
}

Date <- function() {
    list("date")
}

DateTime <- function() {
    list("datetime")
}

Time <- function() {
    list("time")
}

Geopoint <- function() {
    list("geopoint")
}

Note <- function(name, label, ...) {
    list(list(
        .type = "survey_row",
        name = enquo(name),
        type = "note",
        label = label,
        args = args(...)
    ))
}

Calculate <- function(name, calculation, ...) {
    list(list(
        .type = "survey_row",
        name = enquo(name),
        type = list("calculate"),
        calculation = enquo(calculation),
        args = args(...)
    ))
}

CalculateHere <- function(name, calculation, ...) {
    list(list(
        .type = "survey_row",
        name = enquo(name),
        type = list("calculate_here"),
        calculation = enquo(calculation),
        args = args(...)
    ))
}

