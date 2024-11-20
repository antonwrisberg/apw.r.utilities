#' Convert multiple columns in data frame at once
#'
#' Helper function to easily convert multiple columns of a data frame into a
#' speficic type to avoid forcing the type of each column individually.
#'
#' @param type A type to convert the columns into. One of "character".
#' "numeric", "factor", and "logical"
#' @param df A data frame.
#' @param columns A list of column names to change the type of.
#' @returns A data frame.
#' @examples
#' example_df <- data.frame(
#'   condition = c("a", "b", "a", "b"),
#'   participant = c(1, 1, 2, 2)
#' )
#' example_df <- convert_to("factor", example_df, c("condition", "participant"))
#' @export
convert_to_ <- function(type, df, columns) {
  # Check if the input columns are valid
  invalid_columns <- setdiff(columns, colnames(df))
  if (length(invalid_columns) > 0) {
    stop(paste("Invalid columns:", paste(invalid_columns, collapse = ", ")))
  }

  # Convert specified columns to specified type
  if (type == "character") {
    for (column in columns) {
      df[[column]] <- as.character(df[[column]])
    }
  } else if (type == "numeric") {
    for (column in columns) {
      df[[column]] <- as.numeric(df[[column]])
    }
  } else if (type == "factor") {
    for (column in columns) {
      df[[column]] <- as.factor(df[[column]])
    }
  } else if (type == "logical") {
    for (column in columns) {
      df[[column]] <- as.logical(df[[column]])
    }
  } else {
    stop(paste("Invalid type:", type))
  }

  return(df)
}

#' CSS-compliant rgba-definitions
#'
#' Helper function to easily create colours in accordance with CSS standards
#' (red, green, and blue specified in intervals from 0 to 255 and alpha
#' specified in the interval from 0 to 1)
#'
#' @param red An integer between 0 and 255.
#' @param green An integer between 0 and 255.
#' @param blue An integer between 0 and 255.
#' @param alpha An integer between 0 and 1.
#' @returns A hex colour value.
#' @examples
#' theme_colour <- rgba(156, 97, 20)
#' @export
rgba <- function(red, green, blue, alpha = 1) {
  return(grDevices::rgb(red / 255, green / 255, blue / 255, alpha))
}