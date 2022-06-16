db_connect <- function() {
  dotenv::load_dot_env()
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("BTO_DB_HOST"),
    port = Sys.getenv("BTO_DB_PORT"),
    dbname = Sys.getenv("BTO_DB_DBNAME"),
    user = Sys.getenv("BTO_DB_USER"),
    password = Sys.getenv("BTO_DB_PASSWORD")
  )
}