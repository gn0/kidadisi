
# This list of function names comes from the SurveyCTO documentation:
#
# https://docs.surveycto.com/02-designing-forms/01-core-concepts/09.expressions.html
#
SURVEYCTO_FUNCTIONS <- c(
    "string",
    "string-length",
    "substr",
    "concat",
    "linebreak",
    "lower",
    "upper",
    "count-selected",
    "selected",
    "selected-at",
    "choice-label",
    "join",
    "join-if",
    "count",
    "count-if",
    "sum",
    "sum-if",
    "min",
    "min-if",
    "max",
    "max-if",
    "index",
    "indexed-repeat",
    "rank-index",
    "rank-index-if",
    "count-items",
    "item-at",
    "item-index",
    "item-present",
    "de-duplicate",
    "rank-value",
    "distance-between",
    "area",
    "geo-scatter",
    "short-geopoint",
    "number",
    "int",
    "format-number",
    "round",
    "abs",
    "pow",
    "log10",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sqrt",
    "exp",
    "pi",
    "duration",
    "today",
    "now",
    "date",
    "date-time",
    "decimal-date-time",
    "decimal-time",
    "format-date-time",
    "enumerator-name",
    "enumerator-id",
    "phone-call-log",
    "phone-call-duration",
    "collect-is-phone-app",
    "relevant",
    "empty",
    "if",
    "not",
    "pulldata",
    "once",
    "random",
    "coalesce",
    "regex",
    "hash",
    "uuid",
    "username",
    "version",
    "device-info",
    "plug-in-metadata"
)

to_surveycto_function <- function(str) {
    result <- gsub("_", "-", str, fixed = TRUE)

    if (!(result %in% SURVEYCTO_FUNCTIONS)) {
        stop(sprintf(
            "Function '%s' is not a known SurveyCTO function.",
            result
        ))
    }

    result
}

#' String literals in XLSForm are written with single quotes.  This
#' function escapes single quotes in the input and puts single quotes at
#' the beginning and the end.
#'
#' @param str The input string to escape.
#' @return \value{A string that contains an XLSForm string literal.}
#'
to_xlsform_string <- function(str) {
    paste0(
        "'",
        gsub(
            "(^|[^\\])((\\\\\\\\)*)'",
            #           ^^^^^^^^ Surprisingly, this pattern matches two
            #                    consecutive backslashes.

            "\\1\\2\\\\'",
            #      ^^^^ Also surprisingly, this inserts just one
            #           backslash before the quote.

            str
        ),
        "'"
    )
}

rec_parse_expr <- function(obj, bound_names) {
    if (typeof(obj) == "language") {
        if (typeof(obj[[1]]) != "symbol") {
            stop(sprintf(
                "Object '%s' does not start with a symbol.",
                deparse(obj[[1]])
            ))
        } else if (length(obj) == 1) {
            stop(sprintf("Object '%s' has length 1.", deparse(obj)))
        }

        symbol_str <- deparse(obj[[1]])
        func_str <- NULL
        func_is_infix <- NULL

        if (symbol_str %in% c("==", "&", "|", "<=", "<", ">=", ">", "+",
                              "-", "*", "/")) {
            if (symbol_str == "==") {
                func_str <- "="
            } else if (symbol_str == "&") {
                func_str <- "and"
            } else if (symbol_str == "|") {
                func_str <- "or"
            } else {
                func_str <- symbol_str
            }

            func_is_infix <- TRUE
        } else if (symbol_str == "(") {
            func_str <- symbol_str
            func_is_infix <- FALSE
        } else if (symbol_str %in% c("ifelse", "!")) {
            if (symbol_str == "ifelse") {
                func_str <- "if"
            } else if (symbol_str == "!") {
                func_str <- "not"
            }

            func_is_infix <- FALSE
        } else {
            # The symbol must be a function name recognized by
            # SurveyCTO.  The function `to_surveycto_function` will
            # panic otherwise.
            #
            func_str <- to_surveycto_function(symbol_str)
            func_is_infix <- FALSE
        }

        arguments <- sapply(
            2:length(obj),
            function(index) rec_parse_expr(obj[[index]], bound_names)
        )

        if (func_str == "(") {
            sprintf("(%s)", paste0(arguments, collapse = ", "))
        } else if (func_is_infix) {
            paste(
                arguments[1],
                func_str,
                arguments[2:length(arguments)]
            )
        } else {
            sprintf(
                "%s(%s)",
                func_str,
                paste0(arguments, collapse = ", ")
            )
        }
    } else if (typeof(obj) == "symbol") {
        symbol_str <- deparse(obj)

        if (symbol_str == ".") {
            symbol_str
        } else {
            # The symbol is a variable name.  We check if the name has
            # been bound already.
            #
            if (!(symbol_str %in% bound_names)) {
                stop(sprintf(
                    "Variable '%s' is used but not defined.",
                    symbol_str
                ))
            }

            sprintf("${%s}", symbol_str)
        }
    } else if (typeof(obj) %in% c("double", "integer")) {
        obj
    } else if (typeof(obj) == "character") {
        to_xlsform_string(obj)
    } else {
        stop(sprintf(
            "Object '%s' has unrecognized type: %s.",
            deparse(obj),
            typeof(obj)
        ))
    }
}

parse_expr <- function(fml, bound_names = c()) {
    if (typeof(fml) != "language"
        || deparse(fml[[1]]) != "~"
        || length(fml) != 2) {
        stop(paste(
            "The expression must be a formula with an empty",
            "left-hand side, e.g.: ~ foo + bar > 4"
        ))
    }

    rec_parse_expr(fml[[2]], bound_names)
}
