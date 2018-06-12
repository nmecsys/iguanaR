#' @title Get available journals sources from the Iguana API
#' @description This function show the sources available to request news in iguana api
#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble
#' @examples
#' @rdname iguana.get
#' @export



iguanaSources <- function(){
  source = read.csv2(paste0(system.file(package="iguanaR"),"/journals.csv"))
  source = as_tibble(source)
  return(print(source))
}

