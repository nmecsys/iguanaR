leitor <- function(noticias,start_date,end_date,cenario){

  if(cenario == 1){

  #Cenário 1:
    economic_words <- c("ECONÔ", "ECONO", "-ECON", "MICROECON", "MACROECON", "SOCIOECON")
    uncertainty_words <- c("INCERT","INSTAB","CRISE")
    politic_words <- c("Govern", "Congress", "President", "Presidên","senado", "deput", "impeachment", "eleição")
  }else if(cenario == 2){
  #Cenário 2:
      economic_words <- c("ECONÔ", "ECONO", "-ECON", "MICROECON", "MACROECON", "SOCIOECON")
    uncertainty_words <- c("INCERT","INSTAB","CRISE")
    fiscal_words <- c("Déficit", "superávit", "dívida", "fiscal", "orçament", "imposto", "dominância fiscal")
  }else if(cenario == 3){
  #Cenário 3:
    economic_words <- c("ECONÔ", "ECONO", "-ECON", "MICROECON", "MACROECON", "SOCIOECON")
    uncertainty_words <- c("INCERT","INSTAB","CRISE")
    monetary_words <- c("BCB", "BACEN","Selic", "Juros", "Copom", "Monet", "Infla","Banco Central")
  }


  final_data <- data.frame(
    date = character(),
    n_encontrado = integer(),
    n_total = integer()

  )
  noticias = x[,2]
  datas = x[,1]
  for(i in 1:length(noticias)){
    total = 0
    total_incerteza = 0
    contador = 0
    contador = contador + 1
    total = total + 1

        fileName <- files[j]
        artigo <- readChar(noticias[i])
        artigo <- toupper(artigo)
        combina_termos = do.call(paste, expand.grid(economic_words,uncertainty_words))

        for(k in 1:length(combina_termos)){

          termo = unlist(strsplit(combina_termos[k]," "))

          resultado = all(str_detect(artigo,termo))

          if(resultado){
            total_incerteza = total_incerteza + 1
            break
          }

        }

      }

      new_row <- data.frame(
        date = data,
        n_encontrado = total_incerteza,
        n_total = total
      )

      final_data <- rbind(final_data,new_row)
      return(final_data)

}
