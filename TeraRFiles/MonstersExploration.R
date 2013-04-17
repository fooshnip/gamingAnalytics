library(RPostgreSQL)
db <- dbConnect(PostgreSQL(), 
                user = "postgres", #This needs to be adjusted for your login details
                host = "localhost",
                dbname = "gaming", 
                password = "postgres") #This needs to be adjusted for your login details
#Some example pulls
dbListTables(db)
monsters <- dbGetQuery(db,"Select * from monsters")
monsters$bit <- ifelse(monsters$action=="Kill",1,0)
detach("package:RPostgreSQL", unload=TRUE)
library(sqldf)
success <- sqldf("Select creature, sum(bit)/count(*) as successrate from monsters group by creature order by successrate asc")
classrate <- sqldf("Select class, creature, sum(bit)/count(*) as successrate from monsters group by class, creature order by successrate asc")