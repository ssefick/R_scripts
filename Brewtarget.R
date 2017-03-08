brewtarget <- function(...){

library("RSQLite")

setwd("/usr/local/share/brewtarget/")

con <- dbConnect("SQLite", "default_db.sqlite")

dbListTables(con)

x <- dbReadTable(con, "fermentable_in_recipe")

}
