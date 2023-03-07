# Load libraries
library(formula1data)
library(DBI)
library(dplyr)

# Create local database
con <- createF1db(type = "sqlite")

# Connect to an existing database
con <- F1dbConnect("f1_db.sqlite")

# Examine database
dbListTables(con)

# Look for Sebastian Vettel driverId
dbGetQuery(con, paste("SELECT driverId",
                      "FROM drivers",
                      "WHERE driverRef = 'vettel'"))

# Load Sebastian Vettel results
vet <- dbGetQuery(con,
                  paste("SELECT ",
                        "raceId,",
                        "constructorId,",
                        "number,",
                        "grid,",
                        "position,",
                        "positionText,",
                        "positionOrder,",
                        "points,",
                        "rank,",
                        "statusId",
                        "FROM results",
                        "WHERE driverId = 20"
                        ))

constructors <- dbGetQuery(con,
                           paste("SELECT constructorId, constructorRef, name",
                                 "FROM constructors"))
races <- dbGetQuery(con,
                    paste("SELECT",
                          "raceId,",
                          "year,",
                          "round,",
                          "name",
                          "FROM races"))

status <- dbGetQuery(con, paste("SELECT *",
                                "FROM status"))


# Joins
vet <-  vet %>% 
  inner_join(constructors, by = "constructorId") %>% 
  inner_join(races, by = "raceId", suffix = c("_constructor", "_race")) %>% 
  inner_join(status, by = "statusId")


# Select  and order interesting columns
colnames(vet)
vet <- vet %>% 
  select(raceId,
         year,
         name_race,
         round,
         number,
         name_constructor,
         grid,
         position,
         positionText,
         positionOrder,
         points,
         status,
         constructorRef) %>% 
  group_by(year, round) %>% 
  arrange(year)

# save to csv
write.csv(vet, "vet.csv")
