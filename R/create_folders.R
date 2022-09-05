#'@title
#'Creates `ssid` folder structure
#'
#'@description
#'`create_folders` creates the folder structure that is needed to store the processed data for
#'`ssid`.
#'
#'@details
#'`create_folders` creates two folders in the `model_path`, set by the user:
#'mappings and processed_data (including subfolders). It copies a number of cvs files
#'into the mappings folder, which contain several data tables that are needed to run
#'the model and, if needed can be adjusted by the user.
#'
#'@param param Object of type `ssid_par` that bundles all `ssid` parameters,
#'  including core model folders, alpha-3 country code and year and administrative unit
#'  level at which the model is solved and type of
#'  model.
#'
#'@examples
#'\dontrun{
#'create_folders(param)
#'}
#'
#'@export
create_folders <- function(param = NULL) {
    stopifnot(inherits(param, "ssid_par"))
    if(!dir.exists(file.path(param$model_path, "processed_data")))
        dir.create(file.path(param$model_path, paste0("processed_data")),
                   showWarnings = TRUE, recursive = TRUE)
    proc_folders <- c("adm",
                      "benchmark",
                      "simulation")
    purrr::walk(proc_folders, function(x) {
        if(!dir.exists(file.path(param$model_path, paste0("processed_data/", x)))) {
            dir.create(file.path(param$model_path, paste0("processed_data/", x)),
                       showWarnings = TRUE,
                       recursive = TRUE)
        }
    })
    cat("\n=> mapspamc folder structure created in", param$model_path)
}
