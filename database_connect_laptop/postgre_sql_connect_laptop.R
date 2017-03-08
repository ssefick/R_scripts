library(RPostgreSQL)
m <- PostgreSQL()
con <- dbConnect(m, user="ssefick", password="monobrodobro", dbname="SERDP_Physical", host="131.204.120.137", port="5432")
