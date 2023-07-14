
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
        form_id = form_id,
        form_version = form_version,
        form_title = form_title,
        block = Block(...)
    )
}

Block <- function(...) {
    list(.type = "block", ...)
}

ChoiceList <- function(name, ...) {
    list(.type = "choice_list", name = name, ...)
}

Choice <- function(value, label, ...) {
    list(.type = "row", value = value, label = label, ...)
}

If <- function(cond, block) {
    list(.type = "if",
         cond = substitute(cond),
         block = if (block[[".type"]] == "block") {
             block
         } else {
             Block(block)
         })
}

IfElse <- function(cond, if_block, else_block) {
    list(.type = "ifelse",
         cond = substitute(cond),
         if_block = if (if_block[[".type"]] == "block") {
             if_block
         } else {
             Block(if_block)
         },
         else_block = if (else_block[[".type"]] == "block") {
             else_block
         } else {
             Block(else_block)
         })
}

Match <- function(name, ...) {
    list(.type = "match", ...)
}

Group <- function(name, label, block, ...) {
    list(
        .type = "group",
        name = substitute(name),
        label = label,
        block = if (block[[".type"]] == "block") {
            block
        } else {
            Block(block)
        },
        args = args(...)
    )
}

Repeat <- function(name, label, block, ...) {
    list(
        .type = "repeat",
        name = substitute(name),
        label = label,
        block = if (block[[".type"]] == "block") {
            block
        } else {
            Block(block)
        },
        args = args(...)
    )
}

Ask <- function(name, type, label, ...) {
    list(
        .type = "row",
        name = substitute(name),
        type = type,
        label = label,
        args = args(...)
    )
}

Integer <- function() {
    "integer"
}

Text <- function() {
    "text"
}

SelectOne <- function(list_name) {
    list("select_one", substitute(list_name))
}

SelectMultiple <- function(list_name) {
    list("select_multiple", substitute(list_name))
}

Date <- function() {
    "date"
}

DateTime <- function() {
    "datetime"
}

Time <- function() {
    "time"
}

Geopoint <- function() {
    "geopoint"
}

Note <- function(name, label, ...) {
    list(
        .type = "row",
        name = substitute(name),
        label = "note",
        args = args(...)
    )
}

Calculate <- function(name, calculation, ...) {
    list(
        .type = "row",
        name = substitute(name),
        calculation = substitute(calculation),
        args = args(...)
    )
}

