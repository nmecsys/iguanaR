

midia <- function(ano,mes){
  #########################################################################################
  ### IIEBR Mídia

  ## Diretório dados mídia
  #setwd(diretorio)

  ## Lendo as séries históricas de mídia
  ## colocar a série histórica no banco de dados

  data_arquivo = strftime(as.Date(paste0(ano_mes, "-01")) - months(1), "%Y-%m")
  total = read.csv2(paste0("Midia\\Dados\\Series_com_link\\total_", data_arquivo, ".csv"))
  incert = read.csv2(paste0("Midia\\Dados\\Series_com_link\\incert_", data_arquivo, ".csv"))

  ## Valor Econômico online
  #valor <- readRDS("Midia/Dados/setembro_final/2017-09-25_statistic_valor_setembro_parte_5.Rda")
  valor$date <- as.Date(valor$date)
  valor$shortdate <- strftime(valor$date, format="%Y-%m")
  valor<-valor[valor$shortdate >= "2011-08",]
  if(any(is.na(valor[nrow(valor),]))){valor <- valor[-nrow(valor),]}

  aux_valoronline = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                       n_encontrado = incert$Valor_online,
                                       n_total = total$Valor_online,
                                       proporcao = incert$Valor_online/total$Valor_online, stringsAsFactors = FALSE))
  aux_valoronline = aux_valoronline[1:which(aux_valoronline$shortdate == data_arquivo),]

  valoronline_ts = serie_historica(dados_historicos = aux_valoronline, dados = valor, nome_serie = "Valor Econ\u{F4}mico (online)", proporcao = proporcao, limites = limites)

  valoronline = rbind(aux_valoronline, valoronline_ts$base[which(valoronline_ts$base$shortdate2 == ano_mes),])

  valoronline_ts$total_noticias = ts(valoronline$n_total, freq = 12,
                                     start = c(year(as.Date(paste0(valoronline$shortdate2[1], "-01"))), month(as.Date(paste0(valoronline$shortdate2[1], "-01")))))
  valoronline_ts$total_incerteza = ts(valoronline$n_encontrado, freq = 12,
                                      start = c(year(as.Date(paste0(valoronline$shortdate2[1], "-01"))), month(as.Date(paste0(valoronline$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, valoronline_ts$detec_erro)

  ## Valor Econômico impresso
  #valor_impresso <- readRDS("Midia\\Dados/setembro_final/2017-09-26impresso_statistic_valor_impresso_setembro_parte5.Rda")
  valor_impresso$date <- as.Date(valor_impresso$date) #aaaa-mm
  valor_impresso$shortdate <- strftime(valor_impresso$date, format="%Y-%m")
  valor_impresso <- valor_impresso[valor_impresso$shortdate>= "2011-08",]  # a partir de ago/2011

  aux_valorimpresso = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                         n_encontrado = incert$Valor_impresso,
                                         n_total = total$Valor_impresso,
                                         proporcao = incert$Valor_impresso/total$Valor_impresso, stringsAsFactors = FALSE))
  aux_valorimpresso = aux_valorimpresso[1:which(aux_valorimpresso$shortdate == data_arquivo),]

  valorimpresso_ts = serie_historica(dados_historicos = aux_valorimpresso, dados = valor_impresso, nome_serie = "Valor Econ\u{F4}mico (impresso)", proporcao = 0.15, limites = limites)

  valorimpresso = rbind(aux_valorimpresso, valorimpresso_ts$base[which(valorimpresso_ts$base$shortdate2 == ano_mes),])

  valorimpresso_ts$total_noticias = ts(valorimpresso$n_total, freq = 12,
                                       start = c(year(as.Date(paste0(valorimpresso$shortdate2[1], "-01"))), month(as.Date(paste0(valorimpresso$shortdate2[1], "-01")))))
  valorimpresso_ts$total_incerteza = ts(valorimpresso$n_encontrado, freq = 12,
                                        start = c(year(as.Date(paste0(valorimpresso$shortdate2[1], "-01"))), month(as.Date(paste0(valorimpresso$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, valorimpresso_ts$detec_erro)

  ## Folha online
  #folha_online <- readRDS("Midia/Dados/setembro_final/2017-09-25folha_online_setembro_partep4.rda")
  folha_online$date <- as.Date(folha_online$date)
  folha_online$shortdate <- strftime(folha_online$date, format="%Y-%m") # aaaa-mm
  folha_online <- folha_online[folha_online$shortdate >= "2007-06",]    # dados a partir de dez/1999

  aux_folhaonline = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                       n_encontrado = incert$Folha_online,
                                       n_total = total$Folha_online,
                                       proporcao = incert$Folha_online/total$Folha_online, stringsAsFactors = FALSE))
  aux_folhaonline = aux_folhaonline[1:which(aux_folhaonline$shortdate == data_arquivo),]

  folhaonline_ts = serie_historica(dados_historicos = aux_folhaonline, dados = folha_online, nome_serie = "Folha de S\u{E3}o Paulo (online)", proporcao = proporcao, limites = limites)

  folhaonline = rbind(aux_folhaonline, folhaonline_ts$base[which(folhaonline_ts$base$shortdate2 == ano_mes),])

  folhaonline_ts$total_noticias = ts(folhaonline$n_total, freq = 12,
                                     start = c(year(as.Date(paste0(folhaonline$shortdate2[1], "-01"))), month(as.Date(paste0(folhaonline$shortdate2[1], "-01")))))
  folhaonline_ts$total_incerteza = ts(folhaonline$n_encontrado, freq = 12,
                                      start = c(year(as.Date(paste0(folhaonline$shortdate2[1], "-01"))), month(as.Date(paste0(folhaonline$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, folhaonline_ts$detec_erro)

  ## Folha impresso
  #folha_impresso <-readRDS("Midia/Dados/setembro_final/2017-09-25folha_impresso_setembro_partep4.Rda")
  folha_impresso$date <- as.Date(folha_impresso$date)
  folha_impresso$shortdate <- strftime(folha_impresso$date, format="%Y-%m") # aaaa-mm
  folha_impresso <- folha_impresso[folha_impresso$shortdate >= "2000-01",]   # dados a partir de dez/1999

  # Fazendo out/16 a fev/16 igual a zero porque os valores estão estranhos
  folhaimpresso_subst = folha_impresso

  # Máscara com os dias do mês usados para calcular a série mensal
  dias_mes <- c(1:25)
  dias_mes2 <- c(26:31)
  masc_mes <- as.numeric(substr(folhaimpresso_subst$date, 9, 10)) %in% dias_mes
  masc_mes2 <- as.numeric(substr(folhaimpresso_subst$date, 9, 10)) %in% dias_mes2

  # Pegando os dados no período
  folhaimpresso_subst$shortdate2 <- 0
  folhaimpresso_subst[masc_mes,"shortdate2"] <- paste0(year(folhaimpresso_subst[masc_mes,"date"]),"-",month(folhaimpresso_subst[masc_mes,"date"]))
  folhaimpresso_subst[masc_mes2,"shortdate2"] <- paste0(year(folhaimpresso_subst[masc_mes2,"date"]),"-",month(folhaimpresso_subst[masc_mes2,"date"]) + 1)

  # Formatando a data para "aaaa-mm"
  numchar <- nchar(folhaimpresso_subst$shortdate2)
  folhaimpresso_subst[numchar < 7,"shortdate2"] <- paste0(substr(folhaimpresso_subst[numchar < 7,"shortdate2"],1,4),"-0",substr(folhaimpresso_subst[numchar<7,"shortdate2"],6,7))

  # Eliminando mês 13
  mes_13 <- as.numeric(substr(folhaimpresso_subst$shortdate2,6,7)) == 13
  folhaimpresso_subst[mes_13,"shortdate2"] <- paste0(year(folhaimpresso_subst[mes_13,"date"]) + 1,"-01")

  # folhaimpresso_subst[which(folhaimpresso_subst$shortdate2 %in% c("2015-10", "2015-11", "2015-12", "2016-01", "2016-02")), c("n_encontrado", "n_total")] = c(0,0)

  aux_folhaimpresso = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                         n_encontrado = incert$Folha_impresso,
                                         n_total = total$Folha_impresso,
                                         proporcao = incert$Folha_impresso/total$Folha_impresso, stringsAsFactors = FALSE))
  aux_folhaimpresso = aux_folhaimpresso[1:which(aux_folhaimpresso$shortdate == data_arquivo),]

  folhaimpresso_ts = serie_historica(dados_historicos = aux_folhaimpresso, dados = folhaimpresso_subst, nome_serie = "Folha de S\u{E3}o Paulo (impresso)", proporcao = proporcao, limites = limites)

  folhaimpresso = rbind(aux_folhaimpresso, folhaimpresso_ts$base[which(folhaimpresso_ts$base$shortdate2 == ano_mes),])

  folhaimpresso_ts$total_noticias = ts(folhaimpresso$n_total, freq = 12,
                                       start = c(year(as.Date(paste0(folhaimpresso$shortdate2[1], "-01"))), month(as.Date(paste0(folhaimpresso$shortdate2[1], "-01")))))
  folhaimpresso_ts$total_incerteza = ts(folhaimpresso$n_encontrado, freq = 12,
                                        start = c(year(as.Date(paste0(folhaimpresso$shortdate2[1], "-01"))), month(as.Date(paste0(folhaimpresso$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, folhaimpresso_ts$detec_erro)
  # Dando uma roubadinha pra FEV/16 ser outlier
  if(ano_mes %in% c("2015-10", "2015-11", "2015-12", "2016-01", "2016-02")){
    detec_erro[which(detec_erro$Variavel == "Folha de S\u{E3}o Paulo (impresso)"), "Resultado"] = "outlier"
  }


  ## Twitter

  # Correio Braziliense
  # cbo <- readRDS("Midia\\Dados/outubro/2016_twitter_statistic_2@cbonlinedf2016-09-06.Rda")
  #correio <-readRDS("Midia\\Dados/setembro_final//2017_twitter_com_link_setembro_@correio2017-09-26_parte4.Rda")
  # correio <- correio[-c(1,2),]
  # braziliense <- rbind(cbo, correio)
  braziliense <- correio
  braziliense$date <- as.Date(braziliense$date)
  braziliense$shortdate <- strftime(braziliense$date, format="%Y-%m")
  braziliense <- braziliense[braziliense$shortdate >= "2011-01",]

  aux_braziliense = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                       n_encontrado = incert$Braziliense,
                                       n_total = total$Braziliense,
                                       proporcao = incert$Braziliense/total$Braziliense, stringsAsFactors = FALSE))
  aux_braziliense = aux_braziliense[1:which(aux_braziliense$shortdate == data_arquivo),]

  braziliense_ts = serie_historica(dados_historicos = aux_braziliense, dados = braziliense, nome_serie = "Correio Braziliense (twitter)", proporcao = proporcao, limites = limites)

  braziliense = rbind(aux_braziliense, braziliense_ts$base[which(braziliense_ts$base$shortdate2 == ano_mes),])

  braziliense_ts$total_noticias = ts(braziliense$n_total, freq = 12,
                                     start = c(year(as.Date(paste0(braziliense$shortdate2[1], "-01"))), month(as.Date(paste0(braziliense$shortdate2[1], "-01")))))
  braziliense_ts$total_incerteza = ts(braziliense$n_encontrado, freq = 12,
                                      start = c(year(as.Date(paste0(braziliense$shortdate2[1], "-01"))), month(as.Date(paste0(braziliense$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, braziliense_ts$detec_erro)

  # Estadão
  #estadao <- readRDS("Midia\\Dados/setembro_final/2017_twitter_com_link_setembro_@Estadao2017-09-26_parte4.Rda")
  estadao$date <- as.Date(estadao$date)
  estadao$shortdate <- strftime(estadao$date, format = "%Y-%m")
  estadao <- estadao[estadao$shortdate >= "2010-01",]

  aux_estadao = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                   n_encontrado = incert$Estadao,
                                   n_total = total$Estadao,
                                   proporcao = incert$Estadao/total$Estadao, stringsAsFactors = FALSE))
  aux_estadao = aux_estadao[1:which(aux_estadao$shortdate == data_arquivo),]

  estadao_ts = serie_historica(dados_historicos = aux_estadao, dados = estadao, nome_serie = "Estad\u{E3}o (twitter)", proporcao = proporcao, limites = limites)

  estadao = rbind(aux_estadao, estadao_ts$base[which(estadao_ts$base$shortdate2 == ano_mes),])

  estadao_ts$total_noticias = ts(estadao$n_total, freq = 12,
                                 start = c(year(as.Date(paste0(estadao$shortdate2[1], "-01"))), month(as.Date(paste0(estadao$shortdate2[1], "-01")))))
  estadao_ts$total_incerteza = ts(estadao$n_encontrado, freq = 12,
                                  start = c(year(as.Date(paste0(estadao$shortdate2[1], "-01"))), month(as.Date(paste0(estadao$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, estadao_ts$detec_erro)

  # Jornal O Globo
  #oglobo <- readRDS("Midia\\Dados/setembro_final/2017_twitter_link_setembro_@JornalOGlobo2017-09-26_partep4.Rda")
  oglobo$date <- as.Date(oglobo$date)
  oglobo$shortdate <- strftime(oglobo$date, format = "%Y-%m")
  oglobo<-oglobo[oglobo$shortdate >= "2010-01",]

  aux_oglobo = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                  n_encontrado = incert$O_Globo,
                                  n_total = total$O_Globo,
                                  proporcao = incert$O_Globo/total$O_Globo, stringsAsFactors = FALSE))
  aux_oglobo = aux_oglobo[1:which(aux_oglobo$shortdate == data_arquivo),]

  oglobo_ts = serie_historica(dados_historicos = aux_oglobo, dados = oglobo, nome_serie = "O Globo (twitter)", proporcao = proporcao, limites = limites)

  oglobo = rbind(aux_oglobo, oglobo_ts$base[which(oglobo_ts$base$shortdate2 == ano_mes),])

  oglobo_ts$total_noticias = ts(oglobo$n_total, freq = 12,
                                start = c(year(as.Date(paste0(oglobo$shortdate2[1], "-01"))), month(as.Date(paste0(oglobo$shortdate2[1], "-01")))))
  oglobo_ts$total_incerteza = ts(oglobo$n_encontrado, freq = 12,
                                 start = c(year(as.Date(paste0(oglobo$shortdate2[1], "-01"))), month(as.Date(paste0(oglobo$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, oglobo_ts$detec_erro)

  # Zero Hora
  #zero <- readRDS("Midia/Dados/setembro_final/2017_twitter_com_link_setembro_@zerohora2017-09-26_partep4.Rda")
  zero$date <- as.Date(zero$date)
  zero$shortdate <- strftime(zero$date, format = "%Y-%m")
  zero <- zero[zero$shortdate >= "2010-01",]
  # Fazendo ago/16 igual a zero porque os valores estão absurdos
  zero_ago16 = zero

  # Máscara com os dias do mês usados para calcular a série mensal
  dias_mes <- c(1:25)
  dias_mes2 <- c(26:31)
  masc_mes <- as.numeric(substr(zero_ago16$date, 9, 10)) %in% dias_mes
  masc_mes2 <- as.numeric(substr(zero_ago16$date, 9, 10)) %in% dias_mes2

  # Pegando os dados no período
  zero_ago16$shortdate2 <- 0
  zero_ago16[masc_mes,"shortdate2"] <- paste0(year(zero_ago16[masc_mes,"date"]),"-",month(zero_ago16[masc_mes,"date"]))
  zero_ago16[masc_mes2,"shortdate2"] <- paste0(year(zero_ago16[masc_mes2,"date"]),"-",month(zero_ago16[masc_mes2,"date"]) + 1)

  # Formatando a data para "aaaa-mm"
  numchar <- nchar(zero_ago16$shortdate2)
  zero_ago16[numchar < 7,"shortdate2"] <- paste0(substr(zero_ago16[numchar < 7,"shortdate2"],1,4),"-0",substr(zero_ago16[numchar<7,"shortdate2"],6,7))

  # Eliminando mês 13
  mes_13 <- as.numeric(substr(zero_ago16$shortdate2,6,7)) == 13
  zero_ago16[mes_13,"shortdate2"] <- paste0(year(zero_ago16[mes_13,"date"]) + 1,"-01")

  zero_ago16[which(zero_ago16$shortdate2 == "2016-08"), c("n_encontrado", "n_total")] = c(0,0)

  aux_zerohora = na.omit(data.frame(shortdate2 = strftime(as.Date(total$Data), "%Y-%m"),
                                    n_encontrado = incert$Zero_Hora,
                                    n_total = total$Zero_Hora,
                                    proporcao = incert$Zero_Hora/total$Zero_Hora, stringsAsFactors = FALSE))
  aux_zerohora = aux_zerohora[1:which(aux_zerohora$shortdate == data_arquivo),]

  zerohora_ts = serie_historica(dados_historicos = aux_zerohora, dados = zero_ago16, nome_serie = "Zero Hora (twitter)", proporcao = proporcao, limites = limites)

  zerohora = rbind(aux_zerohora, zerohora_ts$base[which(zerohora_ts$base$shortdate2 == ano_mes),])

  zerohora_ts$total_noticias = ts(zerohora$n_total, freq = 12,
                                  start = c(year(as.Date(paste0(zerohora$shortdate2[1], "-01"))), month(as.Date(paste0(zerohora$shortdate2[1], "-01")))))
  zerohora_ts$total_incerteza = ts(zerohora$n_encontrado, freq = 12,
                                   start = c(year(as.Date(paste0(zerohora$shortdate2[1], "-01"))), month(as.Date(paste0(zerohora$shortdate2[1], "-01")))))

  detec_erro = rbind(detec_erro, zerohora_ts$detec_erro)


  ## mts
  series_total <- cbind(valoronline_ts$total_noticias,
                        valorimpresso_ts$total_noticias,
                        folhaonline_ts$total_noticias,
                        folhaimpresso_ts$total_noticias,
                        braziliense_ts$total_noticias,
                        estadao_ts$total_noticias,
                        oglobo_ts$total_noticias,
                        zerohora_ts$total_noticias)
  colnames(series_total) <- c("Valor_online", "Valor_impresso", "Folha_online", "Folha_impresso", "Braziliense", "Estadao", "O_Globo", "Zero_Hora")

  series_incert <- cbind(valoronline_ts$total_incerteza,
                         valorimpresso_ts$total_incerteza,
                         folhaonline_ts$total_incerteza,
                         folhaimpresso_ts$total_incerteza,
                         braziliense_ts$total_incerteza,
                         estadao_ts$total_incerteza,
                         oglobo_ts$total_incerteza,
                         zerohora_ts$total_incerteza)
  colnames(series_incert) <- c("Valor_online", "Valor_impresso", "Folha_online", "Folha_impresso", "Braziliense", "Estadao", "O_Globo", "Zero_Hora")

  # series_proporcao <- cbind(valoronline_ts$total_incerteza/valoronline_ts$total_noticias,
  #                           valorimpresso_ts$total_incerteza/valorimpresso_ts$total_noticias,
  #                           folhaonline_ts$total_incerteza/folhaonline_ts$total_noticias,
  #                           folhaimpresso_ts$total_incerteza/folhaimpresso_ts$total_noticias,
  #                           braziliense_ts$total_incerteza/braziliense_ts$total_noticias,
  #                           estadao_ts$total_incerteza/estadao_ts$total_noticias,
  #                           oglobo_ts$total_incerteza/oglobo_ts$total_noticias,
  #                           zerohora_ts$total_incerteza/zerohora_ts$total_noticias)
  # colnames(series_proporcao) <- c("Valor_online", "Valor_impresso", "Folha_online", "Folha_impresso", "Braziliense", "Estadao", "O_Globo", "Zero_Hora")


  ## Verificando se alguma das séries tem outlier e decidindo se devemos calcular o IIE-Br-Mídia mesmo assim
  if(sum(detec_erro$Resultado == "outlier") > 0){
    print(detec_erro[,c("Variavel", "Tipo", "Resultado")])

    y <- readline("Outliers foram encontrados nas s\u{E9}ries de m\u{ED}dia. Substitu\u{ED}-las usando regress\u{E3}o? (S/N) ")
    if(y == "S"){

      codigos <- data.frame(Serie = rep(c("Valor Econ\u{F4}mico (online)", "Valor Econ\u{F4}mico (impresso)",
                                          "Folha de S\u{E3}o Paulo (online)", "Folha de S\u{E3}o Paulo (impresso)",
                                          "Correio Braziliense (twitter)",
                                          "Estad\u{E3}o (twitter)",
                                          "O Globo (twitter)",
                                          "Zero Hora (twitter)",
                                          "Todas"), each = 2),
                            Serie_R = rep(c("Valor_online", "Valor_impresso", "Folha_online", "Folha_impresso", "Braziliense", "Estadao", "O_Globo", "Zero_Hora", "Todas"), each = 2),
                            Codigo = rep(c("VEO", "VEI", "FSPO", "FSPI", "CBT", "ET", "OGT", "ZHT", "ALL"), each = 2),
                            Tipo = rep(c("Total", "Incerteza"), 9))
      codigos$Codigo_tipo = paste0(codigos$Codigo, rep(c("_tot", "_incert"), 9))
      tipos_series <- data.frame(Tipo = c("Total", "Incerteza"),
                                 Codigo = c("tot", "incert"))

      print(unique(codigos[c("Serie", "Codigo")]))
      print(tipos_series)
      series <- readline("Insira os c\u{F3}digos das s\u{E9}ries que devem ser substitu\u{ED}das separados por espa\u{E7}o. Ex.: VEI_tot ZHT_incert. ")
      series <- strsplit(x = series, split = " ")

      aux_total = series[[1]][grep(pattern = "_tot", series[[1]])]
      aux_incert = series[[1]][grep(pattern = "_incert", series[[1]])]

      # codigos_subst <- as.vector(codigos[which(codigos$Codigo %in% series[[1]]), "Serie_R"])
      # nomes_subst <- as.vector(codigos[which(codigos$Codigo %in% series[[1]]), "Serie"])

      # series_substituicao_total = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Total"),]
      # series_substituicao_incert = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Incerteza"),]
      # if(nomes_subst == "Todas"){
      #
      #   series_substituicao_total = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Total"),]
      #   series_substituicao_incert = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Incerteza"),]
      # }else{
      #
      #   series_substituicao_total = detec_erro[which(as.character(detec_erro$Variavel) %in% nomes_subst & detec_erro$Tipo == "Total"),]
      #   series_substituicao_incert = detec_erro[which(as.character(detec_erro$Variavel) %in% nomes_subst & detec_erro$Tipo == "Incerteza"),]
      # }

      if("ALL_tot" %in% aux_total){

        series_substituicao_total = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Total"),]
        aux_total = codigos[which(codigos$Serie %in% series_substituicao_total$Variavel & codigos$Tipo == "Total"), "Codigo_tipo"]
      }
      if(!"ALL_tot" %in% aux_total){

        nomes_subst_tot = codigos[which(codigos$Codigo_tipo %in% aux_total), "Serie"]
        series_substituicao_total = detec_erro[which(detec_erro$Variavel %in% nomes_subst_tot & detec_erro$Tipo == "Total"),]
      }

      if("ALL_incert" %in% aux_incert){

        series_substituicao_incert = detec_erro[which(detec_erro$Resultado == "outlier" & detec_erro$Tipo == "Incerteza"),]
        aux_incert = codigos[which(codigos$Serie %in% series_substituicao_incert$Variavel & codigos$Tipo == "Incerteza"), "Codigo_tipo"]
      }
      if(!"ALL_incert" %in% aux_incert){

        nomes_subst_incert = codigos[which(codigos$Codigo_tipo %in% aux_incert), "Serie"]
        series_substituicao_incert = detec_erro[which(detec_erro$Variavel %in% nomes_subst_incert & detec_erro$Tipo == "Incerteza"),]
      }

      # codigos_total = as.vector(codigos[which(codigos$Serie %in% series_substituicao_total$Variavel), "Serie_R"])
      # codigos_incert = as.vector(codigos[which(codigos$Serie %in% series_substituicao_incert$Variavel), "Serie_R"])

      codigos_total = as.vector(codigos[which(codigos$Serie %in% series_substituicao_total$Variavel &
                                                codigos$Codigo_tipo %in% aux_total), "Serie_R"])
      codigos_incert = as.vector(codigos[which(codigos$Serie %in% series_substituicao_incert$Variavel &
                                                 codigos$Codigo_tipo %in% aux_incert), "Serie_R"])


      # if(sum(codigos_subst == "Todas") > 0){
      #
      #   lm_fit <- list()
      #   serie_fit <- list()
      #
      #   for(k in 1:ncol(series_proporcao)){
      #
      #     fit = input_dado(dados = series_proporcao, serie_input = colnames(series_proporcao)[k], mes = mes, ano = ano, plot = FALSE)
      #
      #     lm_fit = c(lm_fit, list(fit$lm))
      #     names(lm_fit) = c(names(lm_fit)[-length(lm_fit)], colnames(series_proporcao)[k])
      #
      #     serie_fit = c(serie_fit, list(fit$serie_imputada))
      #     names(serie_fit) = c(names(serie_fit)[-length(serie_fit)], colnames(series_proporcao)[k])
      #   }
      #
      # }else{

      if(length(codigos_total) > 0){

        lm_fit <- list()
        serie_fit <- list()

        for(k in 1:ncol(series_total)){

          if(colnames(series_total)[k] %in% codigos_total){

            fit = input_dado(dados = series_total, serie_input = colnames(series_total)[k], mes = mes, ano = ano)

            lm_fit = c(lm_fit, list(fit$lm))
            names(lm_fit) = c(names(lm_fit)[-length(lm_fit)], colnames(series_total)[k])

            serie_fit = c(serie_fit, list(fit$serie_imputada))
            names(serie_fit) = c(names(serie_fit)[-length(serie_fit)], colnames(series_total)[k])
          }else{

            serie_fit = c(serie_fit, list(na.omit(series_total[,k])))
            names(serie_fit) = c(names(serie_fit)[-length(serie_fit)], colnames(series_total)[k])
          }
        }
      }else{
        aux_fit = data.frame(Data = as.Date(series_total), as.matrix(series_total))
        serie_fit = apply(aux_fit[,-1], 2, function(x) na.omit(ts(x, start = c(year(as.Date(aux_fit[1,1])), month(as.Date(aux_fit[1,1]))), freq = 12)))
      }

      if(length(codigos_incert) > 0){

        lm_fit_incert <- list()
        serie_fit_incert <- list()

        for(k in 1:ncol(series_incert)){

          if(colnames(series_incert)[k] %in% codigos_incert){

            fit_incert = input_dado(dados = series_incert, serie_input = colnames(series_incert)[k], mes = mes, ano = ano)

            lm_fit_incert = c(lm_fit_incert, list(fit_incert$lm))
            names(lm_fit_incert) = c(names(lm_fit_incert)[-length(lm_fit_incert)], colnames(series_incert)[k])

            serie_fit_incert = c(serie_fit_incert, list(fit_incert$serie_imputada))
            names(serie_fit_incert) = c(names(serie_fit_incert)[-length(serie_fit_incert)], colnames(series_incert)[k])
          }else{

            serie_fit_incert = c(serie_fit_incert, list(na.omit(series_incert[,k])))
            names(serie_fit_incert) = c(names(serie_fit_incert)[-length(serie_fit_incert)], colnames(series_incert)[k])
          }
        }
      }else{
        aux2_fit = data.frame(Data = as.Date(series_incert), as.matrix(series_incert))
        serie_fit_incert = apply(aux2_fit[,-1], 2, function(x) na.omit(ts(x, start = c(year(as.Date(aux2_fit[1,1])), month(as.Date(aux2_fit[1,1]))), freq = 12)))
      }
    }
    if(y == "N"){

      serie_fit = list(Valor_online = na.omit(valoronline_ts$total_noticias),
                       Valor_impresso = na.omit(valorimpresso_ts$total_noticias),
                       Folha_online = na.omit(folhaonline_ts$total_noticias),
                       Folha_impresso = na.omit(folhaimpresso_ts$total_noticias),
                       Braziliense = na.omit(braziliense_ts$total_noticias),
                       Estadao = na.omit(estadao_ts$total_noticias),
                       O_Globo = na.omit(oglobo_ts$total_noticias),
                       Zero_Hora = na.omit(zerohora_ts$total_noticias))

      serie_fit_incert = list(Valor_online = na.omit(valoronline_ts$total_incerteza),
                              Valor_impresso = na.omit(valorimpresso_ts$total_incerteza),
                              Folha_online = na.omit(folhaonline_ts$total_incerteza),
                              Folha_impresso = na.omit(folhaimpresso_ts$total_incerteza),
                              Braziliense = na.omit(braziliense_ts$total_incerteza),
                              Estadao = na.omit(estadao_ts$total_incerteza),
                              O_Globo = na.omit(oglobo_ts$total_incerteza),
                              Zero_Hora = na.omit(zerohora_ts$total_incerteza))
    }

    result_lm = list()
    if(exists("lm_fit")){result_lm = c(result_lm, list(lm_fit = lm_fit))}
    if(exists("lm_fit_incert")){result_lm = c(result_lm, list(lm_fit_incert = lm_fit_incert))}
  }else{

    serie_fit = list(Valor_online = na.omit(valoronline_ts$total_noticias),
                     Valor_impresso = na.omit(valorimpresso_ts$total_noticias),
                     Folha_online = na.omit(folhaonline_ts$total_noticias),
                     Folha_impresso = na.omit(folhaimpresso_ts$total_noticias),
                     Braziliense = na.omit(braziliense_ts$total_noticias),
                     Estadao = na.omit(estadao_ts$total_noticias),
                     O_Globo = na.omit(oglobo_ts$total_noticias),
                     Zero_Hora = na.omit(zerohora_ts$total_noticias))

    serie_fit_incert = list(Valor_online = na.omit(valoronline_ts$total_incerteza),
                            Valor_impresso = na.omit(valorimpresso_ts$total_incerteza),
                            Folha_online = na.omit(folhaonline_ts$total_incerteza),
                            Folha_impresso = na.omit(folhaimpresso_ts$total_incerteza),
                            Braziliense = na.omit(braziliense_ts$total_incerteza),
                            Estadao = na.omit(estadao_ts$total_incerteza),
                            O_Globo = na.omit(oglobo_ts$total_incerteza),
                            Zero_Hora = na.omit(zerohora_ts$total_incerteza))

    result_lm = list()
    if(exists("lm_fit")){result_lm = c(result_lm, list(lm_fit = lm_fit))}
    if(exists("lm_fit_incert")){result_lm = c(result_lm, list(lm_fit_incert = lm_fit_incert))}

  }

  ## Séries históricas online e impresso

  # Jornal online
  # online_parte1 <- window(folhaonline_ts$total_incerteza/folhaonline_ts$total_noticias, end = c(2011,7))
  # online_parte2 <- (folhaonline_ts$total_incerteza + valoronline_ts$total_incerteza)/(folhaonline_ts$total_noticias + valoronline_ts$total_noticias)
  # online <- ts(c(online_parte1, online_parte2), freq = 12, start = c(2007,6))
  online_parte1 <- window(serie_fit_incert$Folha_online/serie_fit$Folha_online, end = c(2011,7))
  online_parte2 <- (serie_fit_incert$Folha_online + serie_fit_incert$Valor_online)/(serie_fit$Folha_online + serie_fit$Valor_online)
  online <- ts(c(online_parte1, online_parte2), freq = 12, start = c(2007,6))

  # Twitter
  # Total de notícias
  twiter_total_parte1 <- window(serie_fit$Estadao + serie_fit$O_Globo + serie_fit$Zero_Hora, end = c(year(as.Date(serie_fit$Braziliense)[1] - months(1)), month(as.Date(serie_fit$Braziliense)[1] - months(1))))
  twiter_total_parte2 <- window(serie_fit$Estadao + serie_fit$O_Globo + serie_fit$Zero_Hora + serie_fit$Braziliense, end = c(year(as.Date(serie_fit$Zero_Hora)[length(serie_fit$Zero_Hora)]), month(as.Date(serie_fit$Zero_Hora)[length(serie_fit$Zero_Hora)])))
  twiter_total <- ts(c(twiter_total_parte1, twiter_total_parte2),
                     freq = 12, start = c(year(as.Date(twiter_total_parte1)[1]),month(as.Date(twiter_total_parte1)[1])))

  # Total de notícias sobre incerteza
  twiter_incert_parte1 <- window(serie_fit_incert$Estadao + serie_fit_incert$O_Globo + serie_fit_incert$Zero_Hora, end = c(year(as.Date(serie_fit_incert$Braziliense)[1] - months(1)), month(as.Date(serie_fit_incert$Braziliense)[1] - months(1))))
  twiter_incert_parte2 <- window(serie_fit_incert$Estadao + serie_fit_incert$O_Globo + serie_fit_incert$Zero_Hora + serie_fit_incert$Braziliense, end = c(year(as.Date(serie_fit_incert$Zero_Hora)[length(serie_fit_incert$Zero_Hora)]), month(as.Date(serie_fit_incert$Zero_Hora)[length(serie_fit_incert$Zero_Hora)])))
  twiter_incert <- ts(c(twiter_incert_parte1, twiter_incert_parte2),
                      freq = 12, start = c(year(as.Date(twiter_total_parte1)[1]), month(as.Date(twiter_total_parte1)[1])))

  # Percentual de notícias sobre incerteza (total incerteza/total notícias)
  twiter <- twiter_incert/twiter_total

  # Twitter + jornais online
  online_jornal = window(online, end = c(year(as.Date(twiter)[1] - months(1)), month(as.Date(twiter)[1] - months(1))))
  online_media = (twiter + online)/2
  online_completo = ts(c(online_jornal, online_media),
                       start = c(year(as.Date(online_jornal)[1]), month(as.Date(online_jornal)[1])),
                       freq = 12)
  online_completo <- window(online_completo, start =c(2007,06))

  # Impresso
  impresso_parte1 <- window(serie_fit_incert$Folha_impresso/serie_fit$Folha_impresso, end = c(2011,7))
  impresso_parte2 <- (serie_fit_incert$Folha_impresso + serie_fit_incert$Valor_impresso)/(serie_fit$Folha_impresso + serie_fit$Valor_impresso)
  impresso <- ts(c(impresso_parte1, impresso_parte2), freq = 12, start = c(2000,1))
  impresso_parte1 <- window(impresso, end = c(2009,12))


  ## Verificando se alguma das séries tem outlier e decidindo se devemos calcular o IIE-Br-Mídia mesmo assim
  if(!exists("y")){y = "N"}
  if(sum(detec_erro$Resultado == "outlier") > 0 & y == "N"){
    # print(detec_erro[,c("Variavel", "Tipo", "Resultado")])

    z <- readline("Outliers foram encontrados nas s\u{E9}ries de m\u{ED}dia, mas n\u{E3}o foram substitu\u{ED}dos usando regress\u{E3}o. Prosseguir? (S/N) ")
    if(z == "S"){

      midia1<-window(impresso,end=c(2007,05))
      midia2<-(online_completo + impresso)/2
      midia<-ts(c(midia1,midia2),start=c(2000,01),freq=12)
      # midia<-window(midia,end=c(2016,08))

      midia_mediareferencia = mean(midia[which(as.Date(midia) %in% janela_referencia)]) # média dentro da janela de referência
      midia_desvioreferencia = sd(midia[which(as.Date(midia) %in% janela_referencia)])  # desvio padrão na janela de referência
      midia_referencia = (midia - midia_mediareferencia)/midia_desvioreferencia         # dados padronizados pela média e desvio da janela de referência
    }

    detec_erro = rbind(detec_erro,
                       data.frame(Variavel = "M\u{ED}dia - aceitar outlier?", Tipo = "-", Resultado = ifelse(y == "S", "Sim", "N\u{E3}o"),
                                  li_desvio = 0, ls_desvio = 0, li_proporcao = 0, ls_proporcao = 0, li_boxplot = 0, ls_boxplot = 0))

  }else{
    midia1<-window(impresso,end=c(2007,05))
    midia2<-(online_completo + impresso)/2
    midia<-ts(c(midia1,midia2),start=c(2000,01),freq=12)
    # midia<-window(midia,end=c(2016,08))

    midia_mediareferencia = mean(midia[which(as.Date(midia) %in% janela_referencia)]) # média dentro da janela de referência
    midia_desvioreferencia = sd(midia[which(as.Date(midia) %in% janela_referencia)])  # desvio padrão na janela de referência
    midia_referencia = (midia - midia_mediareferencia)/midia_desvioreferencia         # dados padronizados pela média e desvio da janela de referência
  }


}
