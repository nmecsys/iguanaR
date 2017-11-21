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
  valor_online = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Valor_Economico")$noticias
  leitor_valor_online = leitor(valor_online,start_date = start,end_date=end)
  ##- valor impresso
  valor_impresso = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Valor_impresso")$noticias
  leitor_valor_impresso = leitor(valor_impresso,start_date = start,end_date=end)
  ##- folha online
  folha_online = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Folha_online")$noticias
  leitor_folha_online = leitor(folha_online,start_date = start,end_date=end)
  ##- folha impresso
  folha_impresso = iguana.get(token=token,datainicio = start,datafim = end,fonte = "Folha_impresso")$noticias
  leitor_folha_impresso = leitor(folha_impresso,start_date = start,end_date=end)
  ##- @estadao
  estadao  = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@estadao")$noticias
  leitor_estadao = leitor(estadao,start_date = start,end_date=end)
  ##- @correio
  correio  = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@correio")$noticias
  leitor_correio = leitor(correio,start_date = start,end_date=end)
  ##- @oglobo
  oglobo   = iguana.get(token=token,datainicio = start,datafim = end,fonte = "@oglobo")$noticias
  leitor_oglobo = leitor(oglobo,start_date = start,end_date=end)    
  ##- @zerohora
  zerohora = iguana.get(token=token,datainicio = start,datafim = end,fonte = "zehora")$noticias
  leitor_zerohora = leitor(zerohora,start_date = start,end_date=end)

}
