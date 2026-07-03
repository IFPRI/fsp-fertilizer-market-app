

setwd("C:/Users/BRICE/Dropbox (IFPRI)/FertilizerPageApp/FertilizerDashboard13Jan22")
dbname=Sys.getenv('DB_NAME')
host=Sys.getenv('DB_HOST')
port=3306
user=Sys.getenv('DB_USER')
password=Sys.getenv('DB_PASSWORD')

connection <- data.frame(dbname, host, password, port, user)

save(connection, file = "connection.RData") 

save(connection, file = "C:/Users/BRICE/IFPRI Dropbox/Brendan Rice/R Shiny_fertilizer dashboard Asia/FertilizerDashboard13Jan22/connection.RData") 
