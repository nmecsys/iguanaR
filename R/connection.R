#' @title Connection with the server
#' @description  Make the connection with the server
#' @import DBI RMySQL


connection = function(){
  tryCatch({
    conn = dbConnect(MySQL(),db="",user="",password="",host="",port=3306)
    return(conn)
  },
  error = function(e){
  })
}
