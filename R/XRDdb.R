#'@title Graphical User Interface for XRD data entry
#'
#'@description This package will help you to enter XRD data easily to the database.
#'@param xrd_db Launch the XRD data entry interface.
#'@export
#'@keywords
#'@seealso
#'@return
#'@aliases
#'@examples xrd_db()
xrd_db <- function() {
  shiny::runApp(system.file("application", package="XRDdb"))
}
