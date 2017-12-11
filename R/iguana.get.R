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
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iguana.get
#' @export



iguana.get <- function(token,fonte,incerteza,datainicio,datafim,limite,categoria = c("Cotidiano", "Educacao", "Esporte", "Poder", "Mundo", "Ilustrada", "Mercado", "Ciencia", "Equilibrio", "Turismo", "BBC Brasil", "Tec", "Podcasts", "Veiculos", "Colunistas", "Opiniao","Comida",
       "Imoveis", "Negocios","Especial", "Equilibrio e Saude","Ambiente", "Empregos", "Folha Corrida")){
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

        if(length(categoria)== 1){
            param_categoria = paste0("%categoria",categoria,collapse="")
            params[i] = param_categoria
            i=i+1
        }

          if(!missing(limite)){
            param_limite = paste0("&limite=",limite)
            params[i] = param_limite
            i=i+1
          }

      if(!missing(incerteza)){
        param_incerteza = paste0("&incerteza=",incerteza)
        params[i] = param_incerteza
        i=i+1
      }


          parametros = paste0(params,collapse = "")
          dados = fromJSON(txt=paste0(url_base,token,parametros))
          noticias = dados$data
          noticias$manchete = repair_encoding(noticias$manchete,from="UTF-8")
          noticias$noticia  = repair_encoding(noticias$noticia,from="UTF-8")
    }
    return(noticias)
}
