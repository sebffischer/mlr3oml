#' @title List Data from OpenML
#'
#' @name list_oml
#' @rdname list_oml
#'
#' @description
#' This function allows to query data sets, tasks, flows, setups, runs, and evaluation measures
#' from \url{https://openml.org/d} using some simple filter criteria.
#'
#' @details
#' Filter values are usually provided as single atomic values (typically integer or character).
#' Provide a numeric vector of length 2 (`c(l, u)`) to find matches in the range \eqn{[l, u]}.
#'
#' Note that only a subset of filters is exposed here.
#' For a more feature-complete package, see \CRANpkg{OpenML}.
#' Alternatively, you can pass additional filters via `...` using the names of the official API,
#' c.f. \url{https://www.openml.org/api_docs}.
#'
#'
#' @param data_id (`integer()`)\cr
#'   Vector of data ids to restrict to.
#' @param data_name (`character(1)`)\cr
#'   Filter for name of data set.
#' @param number_instances (`integer()`)\cr
#'   Filter for number of instances.
#' @param number_features (`integer()`)\cr
#'   Filter for number of features.
#' @param number_classes (`integer()`)\cr
#'   Filter for number of labels of the target (only classification tasks).
#' @param number_missing_values (`integer()`)\cr
#'   Filter for number of missing values.
#' @param tag (`character()`)\cr
#'   Filter for tags. You can provide multiple tags as character vector.
#' @param limit (`integer()`)\cr
#'   Limit the results to `limit` records.
#'   Default is the value of option `"mlr3oml.limit"`, defaulting to 5000.
#' @param ... (any)\cr
#'   Additional (unsupported) filters, as named arguments.
#'
#' @return (`data.table()`) of results, or a null data.table if no data set matches the filter criteria.
#'
#' @references
#' `r format_bib("openml_r", "vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' ### query data sets
#' # search for titanic data set
#' data_sets = list_oml_data(data_name = "titanic")
#' print(data_sets)
#'
#' # search for a reduced version
#' data_sets = list_oml_data(
#'   data_name = "titanic",
#'   number_instances = c(2200, 2300),
#'   number_features = 4
#' )
#' print(data_sets)
#'
#' ### search tasks for this data set
#' tasks = list_oml_tasks(data_id = data_sets$data_id)
#' print(tasks)
#'
#'
#' # query runs, group by number of runs per task_id
#' runs = list_oml_runs(task_id = tasks$task_id)
#' runs[, .N, by = task_id]
#' }
list_oml_data = function(data_id = NULL, data_name = NULL, number_instances = NULL, number_features = NULL,
  number_classes = NULL, number_missing_values = NULL, tag = NULL, limit = getOption("mlr3oml.limit", 5000L), ...) {
  tab = get_paginated_table("data",
    data_id = data_id,
    data_name = data_name,
    number_instances = number_instances,
    number_features = number_features,
    number_classes = number_classes,
    number_missing_values = number_missing_values,
    tag = tag,
    limit = limit,
    ...
  )

  if (nrow(tab)) {
    setnames(tab, "did", "data_id")
    qualities = transpose_name_value(tab$quality, as_integer = TRUE)
    rcbind(remove_named(tab, c("md5_checksum", "file_id", "format", "quality")), qualities)
  }

  return(tab)
}
