library(RPostgreSQL)
m <- PostgreSQL()
con <- dbConnect(m, user="ssefick", password="monobrodobro", dbname="SERDP_Physical")
