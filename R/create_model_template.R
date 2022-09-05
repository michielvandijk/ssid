#'@title
#'Creates `sidd` model template
#'
#'@description
#'`create_model_template` creates a folder structure with template R scripts to implement the various
#'steps to run `sidd`.
#'
#'@details
#'`create_model template()` creates several folders in the `template_path`, set by the user. The folders
#'contain template R scripts to implement `sidd`.
#'
#'To run the template scripts, they have to be copied into an RStudio project. We recommend to
#'create such a project first and then use `create_model_template()` to add the scripts to
#'the RStudio project. Note that `create_model_template()` only copies script files if they do not exist
#'yet. In this way, the user cannot accidentally replace already modified scripts.
#'
#'@param template_path folder where template scripts will be copied.
#'
#'@examples
#'\dontrun{
#'create_model_template("c:/temp")
#'}
#'
#'@export
create_model_template <- function(template_path = NULL) {
  if(is.null(template_path)) stop("The template_path is not set")
  if(!dir.exists(template_path))
      dir.create(template_path, showWarnings = TRUE, recursive = TRUE)

  copy_template_files <- function(template_path) {
    template_files <- list.files(system.file("template", package = "sidd"),
                                full.names = TRUE, recursive = FALSE)
    purrr::walk(template_files, function(x) {
      if(file.exists(x)) {
        file.copy(x, template_path, recursive = TRUE)
      }
    })
  }

  copy_template_files(template_path)

  cat("\n=> model template created in", template_path)
}
