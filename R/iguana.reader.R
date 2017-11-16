#' @name iguana.reader
#'
#' @param token o codigo para acessar a api
#' @param cenario cenario de leitura
#' @param start inicio da janela temporal
#' @param end final da janela temporal
#'
#'
#' @import stringr parallel
#' @export

iguana.reader<-function(token,cenario = c(1,2,3),start,end){
   if(missing(token)){
      stop("Token nao detectado!")
      if(missing(start) & missing(end)){
        stop("E necessario passar a janela de data das noticias!")
      }else if(missing(end)){
        stop("Nao foi introduzido a data final das noticias!")
      }else if(missing(start)){
        stop("Nao foi introduzido a data inicial das noticias!")
      }
  }

  ##jornais
  ##- valor online
  valor_online = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Valor_Economico")
  ##- valor impresso
  valor_impresso = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Valor_impresso")$noticias
  ##- folha online
  folha_online = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Folha_online")$noticias
  ##- folha impresso
  folha_impresso = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Folha_impresso")$noticias
  ##- @estadao
  estadao  = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@estadao")$noticias
  ##- @correio
  correio  = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@correio")$noticias
  ##- @oglobo
  oglobo   = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@oglobo")$noticias
  ##- @zerohora
  zerohora = iguana.get(token=token,datainicio = start,datafim = end,fonte = "zehora")$noticias

}
