connect <- function() {
  DB_HOST='server2053.cs.technik.fhnw.ch' # or 86.119.36.94 depending on the network
  DB_PORT = 5432
  DB_DBNAME = 'bank_db' # or 'warenkorb_db'
  DB_USERNAME = 'db_user'
  DB_PASSWORD = 'db_user_pw'
  
  drv <- dbDriver("PostgreSQL")
  con <<- dbConnect(drv, dbname = DB_DBNAME,
                   host = DB_HOST, port = DB_PORT,
                   user = DB_USERNAME, password = DB_PASSWORD)  
}

check_connection <- function() {
  if(!exists("con")) {
    connect()
  }
}

disconnect <- function() {
  all_cons <- dbListConnections(dbDriver("PostgreSQL"))
  for(con in all_cons)
    +  dbDisconnect(con)
}