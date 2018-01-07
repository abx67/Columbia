# Class Script, Lecture 13
# December 8
setwd("D:/R project/intro_to_ds/Week13")
# library(DBI)
# library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname="baseball.db")

dbListTables(con)
dbListFields(con, "Batting")
dbListFields(con, "Pitching")


batting <- dbReadTable(con, "Batting")
class(batting)
dim(batting)


# Check Yourself
library(plyr)

salaries <- dbReadTable(con, "Salaries")

my.sum.func <- function(team.yr.df) {
  return(sum(team.yr.df$salary))
}

payroll <- ddply(salaries, .(yearID, teamID), my.sum.func)
payroll <- payroll[payroll$yearID == 2010, ]
payroll <- payroll[order(payroll$V1, decreasing = TRUE), ]
payroll[1:3, ]
payroll[nrow(payroll), ]


# Querying

query <- paste("SELECT playerID, yearID, AB, H, HR",
               "FROM Batting LIMIT 10")
query
dbGetQuery(con, query)

batting[1:10, c("playerID", "yearID", "AB", "H", "HR")]


query <- paste("SELECT playerID, yearID, AB, H, HR",
               "FROM Batting",
               "ORDER BY HR DESC",
               "LIMIT 10")
query
dbGetQuery(con, query)

# Check Yourself
query1 <- paste("SELECT playerID, yearID, AB, H, HR",
               "FROM Batting",
               "WHERE yearID >= 1990 AND yearID <= 2000",
               "ORDER BY HR DESC",
               "LIMIT 10")
dbGetQuery(con, query1)


bat.ord <- batting[order(batting$HR, decreasing = TRUE), ]
subset  <- bat.ord$yearID >= 1990 & bat.ord$yearID <= 2000
columns <- c("playerID", "yearID", "AB", "H", "HR")

head(bat.ord[subset, columns], 10)

query2 <- paste("SELECT playerID, yearID, MAX(HR)",
                "FROM Batting")
dbGetQuery(con, query2)

batting[which.max(batting$HR), c("playerID", "yearID", "HR")]


# Computations
query <- paste("SELECT AVG(HR), AVG(H)",
                "FROM Batting")
dbGetQuery(con, query)

mean(batting$HR, na.rm = TRUE)

query <- paste("SELECT playerID, AVG(HR)",
               "FROM Batting",
               "GROUP BY playerID",
               "ORDER BY AVG(HR) DESC",
               "LIMIT 5")
dbGetQuery(con, query)

query <- paste("SELECT playerID, AVG(HR)",
               "FROM Batting",
               "WHERE yearID >= 1990",
               "GROUP BY playerID",
               "ORDER BY AVG(HR) DESC",
               "LIMIT 5")
dbGetQuery(con, query)

# Check YourSelf

query <- paste("SELECT teamID, AVG(HR)",
               "FROM Batting",
               "WHERE yearID >= 1990",
               "GROUP BY teamID",
               "ORDER BY AVG(HR) DESC",
               "LIMIT 5")
dbGetQuery(con, query)

bat.sub <- batting[batting$yearID >= 1990, ]

my.mean.func <- function(team.df) {
  return(mean(team.df$HR, na.rm = TRUE))
}
avg.hrs <- daply(bat.sub, .(teamID), my.mean.func)
avg.hrs <- sort(avg.hrs, decreasing = TRUE)
head(avg.hrs, 5)



query <- paste("SELECT teamID, AVG(HR) as avgHR",
               "FROM Batting",
               "WHERE yearID >= 1990",
               "GROUP BY teamID",
               "ORDER BY AVG(HR) DESC",
               "LIMIT 5")
dbGetQuery(con, query)


query <- paste("SELECT teamID, AVG(HR) as avgHR",
               "FROM Batting",
               "WHERE yearID >= 1990",
               "GROUP BY teamID",
               "HAVING avgHR >= 4.5")
dbGetQuery(con, query)


# Check Yourself 

query <- paste("SELECT teamID, SUM(salary) as SUMsal",
               "FROM Salaries",
               "WHERE yearID == 2010",
               "GROUP BY teamID",
               "ORDER BY SUMsal DESC",
               "LIMIT 3")
dbGetQuery(con, query)

# JOIN
query <- paste("SELECT *",
               "FROM Salaries",
               "ORDER BY PlayerID",
               "LIMIT 8")
dbGetQuery(con, query)

query <- paste("SELECT yearID, teamID, lgID, playerID, HR",
               "FROM Batting",
               "ORDER BY PlayerID",
               "LIMIT 8")
dbGetQuery(con, query)

query <- paste("SELECT yearID, playerID, salary, HR",
               "FROM Batting JOIN Salaries USING(yearID, playerID)",
               "ORDER BY PlayerID",
               "LIMIT 8")
dbGetQuery(con, query)

merged <- merge(x = batting, y = salaries, 
                by.x = c("yearID", "playerID"), by.y = c("yearID", "playerID"))
names <- c("yearID", "playerID", "salary", "HR")
merged[order(merged$playerID), ][1:8, ]


query <- paste("SELECT yearID, playerID, salary, HR",
               "FROM Batting LEFT JOIN Salaries USING(yearID, playerID)",
               "ORDER BY PlayerID",
               "LIMIT 8")
dbGetQuery(con, query)


query <- paste("SELECT playerID, AVG(salary), AVG(HR)",
               "FROM Batting JOIN Salaries USING(yearID, playerID)",
               "GROUP BY PlayerID",
               "ORDER BY AVG(HR) DESC",
               "LIMIT 10")
dbGetQuery(con, query)




# Check Yourself


query <- paste("SELECT playerID, yearID, E",
               "FROM Fielding",
               "WHERE yearID >= 1990",
               "ORDER BY E DESC",
               "LIMIT 10")
dbGetQuery(con, query)

query <- paste("SELECT playerID, yearID, E, salary",
               "FROM Fielding LEFT JOIN Salaries USING(yearID, playerID)",
               "WHERE yearID >= 1990",
               "ORDER BY E DESC",
               "LIMIT 10")
dbGetQuery(con, query)



# Debugging

my.plotter <- function(x, y, my.list = NULL) {
  if (!is.null(my.list)) {
    print("Here is my.list:")
    print(my.list)
    print("Now about to plot my.list")
    plot(my.list, main = "A plot of my.list!")
  } else {
    print("Here is x:"); print(x)
    print("Here is y:"); print(y)
    print("Now about to plot x, y")
    plot(x, y, main = "A plot from x, y!")
  }
}

my.plotter(x = 1:8, y = 1:8)
my.plotter(my.list = list(x = -10:10, y = (-10:10)^3))


my.plotter()


