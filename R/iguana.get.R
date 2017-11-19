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
#' @imporfrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iguana.get
#' @export



iguana.get <- function(token,fonte,datainicio,datafim,categoria,limite){
    url_base = "http://iguana.incertezalab.com/jornais?token="
    if(missing(token)){
        stop("É preciso inserir um token valido! \n Solicite em www.iguana.incertezalab.com/documentation/index.php")
    }else{

          params = vector(mode="character")
          i=1
          if(!missing(fonte)){
            param_fonte = paste0("&fonte=",fonte)
            params[i] = param_fonte
            i=i+1
          }

          if(!missing(datainicio)){
            param_datainicio = paste0("&datainicio=",datainicio)
            params[i] = param_datainicio
            i=i+1
          }

          if(!missing(datafim)){
            param_datafim = paste0("&datafim=",datafim)
            params[i] = param_datafim
            i=i+1
          }

          if(!missing(limite)){
            param_limite = paste0("&limite=",limite)
            params[i] = param_limite
            i=i+1
          }

          if(!missing(categoria)){
            param_categoria = paste0("&categoria=",limite)
            params[i] = param_categoria
            i=i+1
          }

          dados = fromJSON(txt=paste0(url_base,token,params,collapse = ""))
          noticias = dados
          noticias$manchete = repair_encoding(noticias$manchete,from="UTF-8")
          noticias$noticia  = repair_encoding(noticias$noticia,from="UTF-8")


    }
    return(noticias)
}
