#' Get a listing of the SDS tutorials including where they are deployed
#'
#' @param pattern an (optional) regex to identify specific apps
#'
#' Returns a data frame giving the names of the SDStutorials and the URL where they are deployed.
#' If `pattern` is given, return the info for just the corresponding tutorials.
#'
#' @examples
#' get_info("bootstrap")
#' @export
get_info <- function(pattern = NULL) {
  Tutorials <-
    tibble(name = dir(path = system.file(path = "tutorials", package="SDStutorials")))
  file_names <-
    dir(path = system.file(package = "SDStutorials"),
        recursive = TRUE,
        pattern = "dcf$")
  #if (length(file_names) == 0) return(Tutorials)
  file_names <- file_names[grepl("rsconnect", file_names)]
  read_dcf_file <- function(name) {
    yaml::read_yaml(paste0(system.file(package = "SDStutorials"), "/", name))
  }
  whole_set <- lapply(file_names, read_dcf_file)
  Res  <- if (length(whole_set) > 0) {
    tibble::tibble(
      name = unlist(lapply(whole_set, function(x) x[["name"]])),
      url = unlist(lapply(whole_set, function(x) x[["url"]])))
    } else {
      Tutorials %>%  mutate(url = "not available")
    }

  Res <- Tutorials %>% left_join(Res, by = "name")

  if (is.null(pattern)) return(Res)
  # look for a matching app
  index <- grep(pattern, Res$name)
  if (length(index) == 0) stop("No such tutorial in the SDStutorials package.")

  Res[index,]
}





