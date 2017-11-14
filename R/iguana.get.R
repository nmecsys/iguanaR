#' @title Get data from the Iguana API
#' @description This function acts as a wrapper to the Iguana API, providing an easy access to the data through R.
#' @param token A \code{string}. The access code to use Iguana API
#' @param fonte PARAM_DESCRIPTION
#' @param datainicio PARAM_DESCRIPTION
#' @param datafim PARAM_DESCRIPTION
#' @param categoria PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @keywords get news
#' @importFrom stringr str_extract
#' @imporfrom jsonlite fromJson
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iguana.get
#' @export



iguana.get <- function(token,fonte,datainicio,datafim,categoria){
    url_base = 'iguana.incertezalab.com/jornais?token='
    if(missing(token)){
        stop("É preciso inserir um token valido! \n Solicite em www.iguana.incertezalab.com/documentation/index.php")
    }else{
        if(missing(fonte) & missing(datainicio) & missing(datafim) & missing(categoria)){
            dados = fromJSON(txt=paste0(url_base,token))
        }
    }
    return(dados$data)
}
