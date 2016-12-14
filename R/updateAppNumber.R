#' Update the app version number
#'
#' Update the version number in \code{welcome.md} file to be the same as the
#' package version number.
#'
#' @author Nathan Eastwood
#'
#' @export
updateAppNumber <- function() {
  # read in the DESCRIPTION and welcome.md files
  desc <- readLines("./DESCRIPTION")
  welc <- readLines("./inst/app/welcome.md")

  # extract the version numbers in each file
  descV <- desc[grepl("Version:", desc)]
  welcVpos <- grep(
    "Version: [0-9].[0-9].[0-9]|Version: [0-9].[0-9].[0-9].[0-9]*",
    welc
  )
  welcV <- welc[welcVpos]

  # replace the version number in the welcome.md file
  welc[welcVpos] <- gsub(
    "Version: [0-9].[0-9].[0-9]|Version: [0-9].[0-9].[0-9].[0-9]*",
    descV,
    welcV
  )

  # write the welcome.md file back to itself
  writeLines(welc, con = "./inst/app/welcome.md")
}
