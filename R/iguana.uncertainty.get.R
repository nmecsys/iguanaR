#' @title Get uncertainty data from the Iguana API
#' @description FUNCTION_DESCRIPTION
#' @param token A \code{string}. The access code to use Iguana API
#' @param fonte PARAM_DESCRIPTION
#' @param datainicio PARAM_DESCRIPTION
#' @param datafim PARAM_DESCRIPTION
#' @param categoria PARAM_DESCRIPTION
#' @param output PARAM_DESCRIPTION, Default: c("error", "message", "data")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#'  \code{\link[jsonlite]{fromJSON}}
#'  \code{\link[base]{rawToChar}}
#' @rdname iguana.uncertainty.get
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importfrom  stringr str_extract


token <- 'k2rt3f5'

iguana.uncertainty.get <- function(token,fonte,datainicio,datafim,
                                   categoria,output = c("error","message","data")){

    url_base <- 'http://iguana.incertezalab.com/incerteza?token='

    if(missing(token)){
        stop("É preciso inserir um token valido! \n
             Solicite em www.iguana.incertezalab.com/documentation/index.php")
    }else{
        if(missing(fonte) & missing(datainicio) & missing(datafim) & missing(categoria)){
            dados <- httr::GET(paste0(url_base, token))
            dados <- jsonlite::fromJSON(base::rawToChar(httr::content(dados, 'raw')))
        }
    }


    if(length(output) == 3){

    }else{

        if(str_extract(outpur)=="error"){

        }else if(str_extract(outpur)=="message"){

        }else if(str_extract(outpur)=="data"){

        }
    }
    return(dados$data)
}
