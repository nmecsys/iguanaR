#' @title Get uncertainty data from the Iguana API
#' @description FUNCTION_DESCRIPTION
#' @param token A \code{string}. The access code to use Iguana API
#' @param fonte PARAM_DESCRIPTION
#' @param datainicio PARAM_DESCRIPTION
#' @param datafim PARAM_DESCRIPTION
#' @param limite PARAM_DESCRIPTION
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
#' @importFrom jsonlite fromJSON
#' @importfrom  stringr str_extract




iguana.uncertainty.get <- function(token,fonte,datainicio,limite,datafim,
                                   categoria = c("Cotidiano", "Educacao", "Esporte", "Poder", "Mundo", "Ilustrada", "Mercado", "Ciencia", "Equilibrio", "Turismo", "BBC Brasil", "Tec", "Podcasts", "Veiculos", "Colunistas", "Opiniao","Comida", 
       "Imoveis", "Negocios","Especial", "Equilibrio e Saude","Ambiente", "Empregos", "Folha Corrida")){

    url_base <- 'http://iguana.incertezalab.com/incerteza?token='

    if(missing(token)){
        stop("É preciso inserir um token valido! \n
             Solicite em www.iguana.incertezalab.com/documentation/index.php")
    }else{
        if(missing(fonte) & missing(datainicio) & missing(datafim) & missing(categoria)){
          dados = fromJSON(txt=paste0(url_base,token))
        }else{
    

       params = vector(mode="character")
          i = 1
  
       if(!missing(datainicio)){
            param_datainicio = paste0("&datainicio=",datainicio)
            params[i] = param_datainicio
            i = i+1
          }

          if(!missing(datafim)){
            param_datafim = paste0("&datafim=",datafim)
            params[i] = param_datafim
            i = i+1
          }
  

   if(length(categoria)== 1){
            param_categoria = paste0("%categoria",categoria,collapse="")
            i = i+1
        }     
  
  if(!missing(limite)){
            param_limite = paste0("&limite=",limite)
            params[i] = param_limite
            i = i+1
          }
    params[i] = paste0("&nemc=",1)
            i = i+1
          
          dados = fromJSON(txt=paste0(url_base,token,params,collapse = ""))
          noticias = dados$data
          noticias$manchete = iconv(repair_encoding(noticias$manchete,from="UTF-8"),from="UTF-8",to= "latin1")
          noticias$manchete = repair_encoding(noticias$manchete,from="latin1")
          noticias$noticia  = iconv(repair_encoding(noticias$noticia,from="UTF-8"),from="UTF-8",to="latin1")
          noticias$noticia = repair_encoding(noticias$noticia,from="latin1")
          
          
          
     }
  }
  
  
  
  
    return(dados$noticia)
}
