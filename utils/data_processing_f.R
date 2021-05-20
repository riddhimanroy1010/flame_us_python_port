#' get_matching_names
#'
#' Returns a vector of matched names associated with original names
#' @param original_values Vector of unmatched values
#' @param matching_type Name of the sheet with matching names
#' @param original_source Name of the category for unmatched values
#' @param matched_source Name of the matching category
#' @export
get_matching_names <- function(original_values,
                               matching_type,
                               original_source,
                               matched_source){
  match_dts <- get_input_f(paste0("model_matching_",matching_type))
  own_values <- sapply(original_values,function(x) ifelse(x %in% unlist(strsplit(match_dts[,original_source],";")),match_dts[which(sapply(strsplit(match_dts[,original_source],";"), function(y) x%in%y)),matched_source],NA),simplify = TRUE, USE.NAMES = FALSE)
  return(own_values)
}

#' rename_values
#'
#' Change values of a varibale
#' @param x Vector of original values
#' @param replacing_values list with new names in names, and old names in keys
#' @export
rename_values <- function(x,replacing_values){
  y <- sapply(x,function(e)ifelse(e%in%unlist(replacing_values),names(replacing_values)[sapply(names(replacing_values),function(i)e%in%replacing_values[[i]],USE.NAMES = FALSE)],e),USE.NAMES = FALSE)
  return(y)
}
