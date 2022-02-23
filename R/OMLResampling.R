#' @title Interface to OpenML Splits
#'
#' @description
#' This is the class for OpenML splits that belong to [mlr3oml::OMLTask]s.
#'
#' @section mlr3Intergration:
#' Can be converted to an instantiated [mlr3::Resampling] using the `$convert` method.
#'
#' @export
#'
#' @examples
#' \donttest{
#' oml_task = OMLTask$new(59) # mlr3oml::OMLTask
#' oml_resampling = oml_task$resampling # [mlr3oml::OMLResampling]
#' resampling = oml_resampling$convert() # mlr3::Resampling
#' }
#'
OMLResampling = R6Class("OMLResampling",
  public = list(
    #' @field id (`integer(1)`)\cr
    #' OpenML task id to which the resampling belongs.
    id = NULL,
    #' @field task (`OMLTask`)\cr
    #' OMLTask to which the resampling belongs.
    task = NULL,
    #' @template field_cache_dir
    cache_dir = NULL,

    #' @description
    #' Creates a new object of class `OMLResampling`-.
    #' @param task OMLTask, either that or task_id have to be provided.
    #' @param task_id OpenML task id.
    #' @template param_cache
    initialize = function(task = NULL, task_id = NULL, cache = getOption("mlr3oml.cache", FALSE)) {
      assert(is.null(task) || is.null(task_id))
      if (is.null(task_id)) {
        assert_r6(task, "OMLTask")
        self$task = task
        self$id = task$id
      } else {
        self$task = OMLTask$new(task_id, cache = cache)
        self$id = task_id
      }
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },
    #' @description
    #' Creates an instantiated mlr3 Resampling object from the OpenML split.
    convert = function() {
      if (is.null(private$.resampling)) {
        splits = cached(download_task_splits, "task_splits", self$id, self$task$desc,
          cache_dir = self$cache_dir
        )
        train_sets = splits[type == "TRAIN", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")
        ]$row_id
        test_sets = splits[type == "TEST", list(row_id = list(as.integer(rowid) + 1L)),
          keyby = c("repeat.", "fold")
        ]$row_id
        resampling = mlr3::ResamplingCustom$new()
        private$.resampling = resampling$instantiate(self$task,
          train_sets = train_sets,
          test_sets = test_sets
        )
      }
      private$.resampling
    }
  ),
  active = list(
    #' @field estimation_procedure
    #' The estimation procedure.
    estimation_procedure = function() self$task$input$estimation_procedure

  ),
  private = list(
    .resampling = NULL
  )
)
