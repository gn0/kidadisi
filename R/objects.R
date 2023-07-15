
#' Postpone the evaluation of arguments whose value is an expression.
#'
args <- function(...) {
    result <- substitute(list(...)) |> as.list()

    if (length(result) > 1) {
        result[2:length(result)]
    } else {
        list()
    }
}

Survey <- function(form_id, form_version, form_title, ...) {
    list(
        .type = "survey",
        form_id = substitute(form_id),
        form_version = substitute(form_version),
        form_title = form_title,
        block = c(...)
    )
}

ChoiceList <- function(name, ...) {
    list(list(
        .type = "choice_list",
        name = substitute(name),
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
        cond = substitute(cond),
        block = block
    ))
}

IfElse <- function(cond, if_block, else_block) {
    list(list(
        .type = "ifelse",
        cond = substitute(cond),
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
        name = substitute(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

Repeat <- function(name, label, block, ...) {
    list(list(
        .type = "repeat",
        name = substitute(name),
        label = label,
        block = block,
        args = args(...)
    ))
}

Ask <- function(name, type, label, ...) {
    list(list(
        .type = "survey_row",
        name = substitute(name),
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
    list("select_one", substitute(list_name))
}

SelectMultiple <- function(list_name) {
    list("select_multiple", substitute(list_name))
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
        name = substitute(name),
        type = "note",
        label = label,
        args = args(...)
    ))
}

Calculate <- function(name, calculation, ...) {
    list(list(
        .type = "survey_row",
        name = substitute(name),
        type = list("calculate"),
        calculation = substitute(calculation),
        args = args(...)
    ))
}

