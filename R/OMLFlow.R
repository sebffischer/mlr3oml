#' @title Interface to OpenML Flows
#'
#' @description
#' This is the class for flows provided on the [OpenML website](https://new.openml.org/search?type=flow).
#'
#' @references
#' `r format_bib("vanschoren2014")`
#'
#' @export
#' @examples
#' \donttest{
#' flow = OMLFlow$new(id = 19068)
#' learner = flow$convert()
#' }
OMLFlow = R6Class("OMLFlow",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML flow id.
    id = NULL,

    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Initializes a new object of class [mlr3oml::OMLFlow].
    initialize = function(id, cache = getOption("mlr3oml.cache", FALSE)) {
      self$id = assert_count(id, coerce = TRUE)
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Returns the learner object if the binary rds file is available.
    convert = function() {
      file = tempfile(fileext = ".rds")
      withr::defer(unlink(file))
      learner = tryCatch(get_rds(self$desc$binary_url),
        error = function(cond) {
          warning("Could not convert flow, returning NULL.")
          return(NULL)
        }
      )
      get_private(learner)$oml_id = self$id
      return(learner)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLFlow:%i>", self$id)
    }
  ),
  active = list(
    #' @field desc (`list(n)`)\cr
    #' The description as downloaded from OpenML.
    desc = function() {
      if (is.null(private$.desc)) {
        private$.desc = download_flow_desc(self$id)
      }
      private$.desc
    },

    #' @field parameter (`data.table`)\cr
    #' The parameters of the flow.
    parameter = function() self$desc$parameter,

    #' @field tag (`character(n)`)\cr
    #' The tags of the flow.
    tag = function() self$desc$tag,

    #' @field dependencies (`character(n)`)\cr
    #' The dependencies of the flow.
    dependencies = function() self$desc$dependencies,

    #' @field name (`character(1)`)\cr
    #' The name of the flow.
    name = function() self$desc$name,

    #' @field description (`character(1)`)\cr
    #' The description of the flow.
    description = function() self$desc$description
  ),
  private = list(
    .desc = NULL
  )
)
