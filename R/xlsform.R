require("dplyr")
require("writexl")

form_id <- function(obj) {
    result <- deparse(obj) |> gsub("^\"|\"$", "", x = _)

    if (grepl("[^A-Za-z0-9_]", result)) {
        stop("The form ID can only contain letters, numbers, and '_'.")
    }

    result
}

form_version <- function(obj) {
    result <- deparse(obj) |> gsub("^\"|\"$", "", x = _)

    if (grepl("^[0-9]+$", result)) {
        result
    } else if (result == "auto") {
        format(Sys.time(), "%y%m%d%H%M", tz = "UTC")
    } else {
        stop(paste(
            "Form version must be either an integer or 'auto' but it",
            sprintf("is '%s'.", result)
        ))
    }
}

rows_to_data_frame <- function(rows) {
    lapply(rows, function(row) {
        lapply(row, function(cell) {
            if (is.null(cell)) {
                ""
            } else {
                cell
            }
        }) |> as_tibble()
    }) |>
        do.call("bind_rows", args = _)
}

row_args <- function(obj, known_variables) {
    mapply(
        function(arg_name, arg_value) {
            if (typeof(arg_value) %in% c("symbol", "language")) {
                if (arg_name %in% c("calculation", "constraint",
                                    "relevance", "repeat_count")) {
                    rec_parse_expr(
                        arg_value,
                        bound_names = known_variables
                    )
                } else {
                    deparse(arg_value)
                }
            } else {
                arg_value
            }
        },
        names(obj),
        obj,
        SIMPLIFY = FALSE
    )
}

parse_survey <- function(obj,
                         known_choice_lists,
                         known_variables,
                         inside_choice_list = FALSE) {
    if (typeof(obj) != "list"
        || any(sapply(obj, function(item) typeof(item) != "list"))) {
        stop(sprintf(
            "Input should be list of lists but it is '%s'.\n",
            deparse(obj)
        ))
    } else if (length(obj) == 0) {
        return(list(
            survey_rows = list(),
            choices_rows = list(),
            known_choice_lists = known_choice_lists,
            known_variables = known_variables
        ))
    }

    survey_rows <- list()
    choices_rows <- list()

    for (item in obj) {
        item_type <- item[[".type"]]

        if (item_type == "choice_list") {
            name <- deparse(item[["name"]])

            if (name %in% known_choice_lists) {
                stop(sprintf(
                    "Choice list '%s' is defined more than once.", name
                ))
            }

            result <- parse_survey(
                item[["block"]],
                known_choice_lists = known_choice_lists,
                known_variables = known_variables,
                inside_choice_list = TRUE
            )

            if (length(result[["survey_rows"]]) > 0) {
                stop(paste(
                    "ChoiceList() cannot contain anything other than",
                    "Choice()."
                ))
            }

            new_rows <- lapply(
                result[["choices_rows"]],
                function(row) c(list(list_name = name), row)
            )

            choices_rows <- c(choices_rows, new_rows)
            known_choice_lists <- c(known_choice_lists, name)
        } else if (item_type %in% c("group", "repeat")) {
            name <- deparse(item[["name"]])
            label <- item[["label"]]

            if (length(item[["args"]]) > 0
                && (is.null(names(item[["args"]]))
                    || "" %in% names(item[["args"]]))) {
                func_name <- item_type
                substr(func_name, 1, 1) <- substr(func_name, 1, 1) |>
                    toupper()

                stop(sprintf(
                    paste("%s(%s, ...) was given unexpected unnamed",
                          "arguments.  Did you forget to combine the",
                          "elements in the %s with c(), e.g., as",
                          "%s(%s, %s, c(Ask(...), Ask(...)))?"),
                    func_name,
                    name,
                    ifelse(item_type == "repeat",
                           "repeat group",
                           "group"),
                    func_name,
                    name,
                    deparse(label)
                ))
            }

            begin_row <- list()

            begin_row[["name"]] <- name
            begin_row[["type"]] <- paste("begin", item_type)
            begin_row[["label"]] <- label

            begin_row <- c(begin_row, row_args(item[["args"]],
                                               known_variables)) |>
                lapply(as.character)
            end_row <- list(name = name, type = paste("end", item_type))

            known_variables <- c(known_variables, name)

            result <- parse_survey(
                item[["block"]],
                known_choice_lists = known_choice_lists,
                known_variables = known_variables
            )

            if (length(result[["choices_rows"]]) > 0) {
                stop("Group() cannot contain ChoiceList().")
            }

            survey_rows <- c(
                survey_rows,
                list(begin_row),
                result[["survey_rows"]],
                list(end_row)
            )
            known_variables <- result[["known_variables"]]
        } else if (item_type == "if") {
            cond <- rec_parse_expr(
                item[["cond"]],
                bound_names = known_variables
            )

            result <- parse_survey(
                item[["block"]],
                known_choice_lists = known_choice_lists,
                known_variables = known_variables
            )

            if (length(result[["choices_rows"]]) > 0) {
                stop("If() cannot contain ChoiceList().")
            }

            new_rows <- lapply(
                result[["survey_rows"]],
                function(row) {
                    if (!is.null(row[["relevance"]])) {
                        row[["relevance"]] <- sprintf(
                            "(%s) and (%s)", cond, row[["relevance"]]
                        )
                    } else {
                        row[["relevance"]] <- cond
                    }

                    row
                }
            )

            survey_rows <- c(survey_rows, new_rows)
            known_variables <- result[["known_variables"]]
        } else if (item_type == "ifelse") {
            cond <- rec_parse_expr(
                item[["cond"]],
                bound_names = known_variables
            )

            if_result <- parse_survey(
                item[["if_block"]],
                known_choice_lists = known_choice_lists,
                known_variables = known_variables
            )

            else_result <- parse_survey(
                item[["else_block"]],
                known_choice_lists = known_choice_lists,
                known_variables = known_variables
            )

            if (length(if_result[["choices_rows"]]) > 0
                || length(else_result[["choices_rows"]]) > 0) {
                stop("IfElse() cannot contain ChoiceList().")
            }

            new_rows <- c(
                lapply(
                    if_result[["survey_rows"]],
                    function(row) {
                        if (!is.null(row[["relevance"]])) {
                            row[["relevance"]] <- sprintf(
                                "(%s) and (%s)",
                                cond,
                                row[["relevance"]]
                            )
                        } else {
                            row[["relevance"]] <- cond
                        }
                        row
                    }
                ),
                lapply(
                    else_result[["survey_rows"]],
                    function(row) {
                        if (!is.null(row[["relevance"]])) {
                            row[["relevance"]] <- sprintf(
                                "not((%s)) and (%s)",
                                cond,
                                row[["relevance"]]
                            )
                        } else {
                            row[["relevance"]] <- sprintf(
                                "not(%s)", cond
                            )
                        }
                        row
                    }
                )
            )

            survey_rows <- c(survey_rows, new_rows)
            known_variables <- unique(c(
                if_result[["known_variables"]],
                else_result[["known_variables"]]
            ))
        } else if (item_type == "match") {
            # TODO
            stop("Match() is not implemented yet.")
        } else if (item_type == "choices_row") {
            if (!inside_choice_list) {
                stop("Choice() can only be used inside ChoiceList().")
            }

            value <- item[["value"]]
            label <- item[["label"]]

            new_row <- list()

            if (!is.null(value)) {
                new_row[["value"]] <- value
            }

            if (!is.null(label)) {
                new_row[["label"]] <- label
            }

            new_row <- c(new_row, row_args(item[["args"]],
                                           known_variables)) |>
                lapply(as.character)

            choices_rows <- c(choices_rows, list(new_row))
        } else if (item_type == "survey_row") {
            name <- deparse(item[["name"]])
            type <- item[["type"]]
            label <- item[["label"]]
            calculation <- item[["calculation"]]

            new_row <- list()

            if (name %in% known_variables) {
                stop(sprintf(
                    "Variable '%s' is defined more than once.", name
                ))
            }

            new_row[["name"]] <- name

            if (typeof(type) == "closure") {
                stop(sprintf(
                    paste(
                        "Invalid type for variable '%s'.  Did you",
                        "forget to add parentheses, e.g., did you",
                        "write 'Integer' instead of 'Integer()'?"
                    ),
                    name
                ))
            } else if (!is.null(type)) {
                if (type[[1]] %in% c("select_one", "select_multiple")) {
                    choice_list <- deparse(type[[2]])

                    if (!(choice_list %in% known_choice_lists)) {
                        stop(sprintf(
                            paste("Variable '%s' references choice",
                                  "list '%s' but that choice list is",
                                  "not defined."),
                            name,
                            choice_list
                        ))
                    }

                    new_row[["type"]] <- paste(type[[1]], choice_list)
                } else {
                    new_row[["type"]] <- type[[1]]
                }
            }

            if (!is.null(label)) {
                variable_refs <- regmatches(
                    label,
                    gregexpr("[$][{][^}]+[}]", label)
                ) |> unlist()

                for (ref in variable_refs) {
                    ref_name <- substr(ref, 3, nchar(ref) - 1)

                    if (!(ref_name %in% known_variables)) {
                        stop(sprintf(
                            "Variable '%s' is used but not defined.",
                            ref_name
                        ))
                    }
                }

                new_row[["label"]] <- label
            }

            item_args <- row_args(item[["args"]], known_variables)

            if (is.null(item_args[["required"]])) {
                if (new_row[["type"]] %in% c("calculate", "note")) {
                    item_args[["required"]] <- "no"
                } else {
                    item_args[["required"]] <- "yes"
                }
            }

            if (!is.null(calculation)) {
                new_row[["calculation"]] <- rec_parse_expr(
                    calculation,
                    bound_names = known_variables
                )
            }

            new_row <- c(new_row, item_args) |> lapply(as.character)

            survey_rows <- c(survey_rows, list(new_row))
            known_variables <- c(known_variables, name)
        } else {
            stop(sprintf("Unrecognized object type: %s.", item_type))
        }
    }

    list(
        survey_rows = survey_rows,
        choices_rows = choices_rows,
        known_choice_lists = known_choice_lists,
        known_variables = known_variables
    )
}

survey_to_xlsform <- function(obj) {
    if (obj[[".type"]] != "survey") {
        stop("Input must be created with the Survey() function.")
    }

    settings <- tibble(
        form_id = form_id(obj[["form_id"]]),
        form_version = form_version(obj[["form_version"]]),
        form_title = obj[["form_title"]]
    )

    result <- parse_survey(
        obj[["block"]],
        known_choice_lists = c(),
        known_variables = c()
    )

    sheets <- list(
        survey = rows_to_data_frame(result[["survey_rows"]]),
        choices = rows_to_data_frame(result[["choices_rows"]]),
        settings = settings
    )

    sheets
}

write_xlsform <- function(obj, filename) {
    obj |>
        survey_to_xlsform() |>
        write_xlsx(filename)
}
