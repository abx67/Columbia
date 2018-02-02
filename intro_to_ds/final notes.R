##SQL
# library(DBI)
# library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname="baseball.db")

dbListTables(con)
dbListFields(con, "Batting")
dbListFields(con, "Pitching")

library(plyr)

salaries <- dbReadTable(con, "Salaries")

my.sum.func <- function(team.yr.df) {
  return(sum(team.yr.df$salary))
}

payroll <- ddply(salaries, .(yearID, teamID), my.sum.func)

query1 <- paste("SELECT playerID, yearID, AB, H, HR",
                "FROM Batting",
                "WHERE yearID >= 1990 AND yearID <= 2000",
                "ORDER BY HR DESC",
                "LIMIT 10")
dbGetQuery(con, query1)

##merge (reshape)
snoq.melt <- melt(snoq, id.vars = "year", variable.name = "day", 
                  value.name = "precip")
head(snoq.melt)
#   year day precip
# 1 1948   1    136
# 2 1949   1     17
# 3 1950   1      1
# 4 1951   1     34
# 5 1952   1      0
# 6 1953   1      2

## aggregate

aggregate(states[, 1:8], list(states$Region), mean)
#         Group.1 Population   Income Illiteracy Life.Exp    Murder  HS.Grad    Frost      Area
# 1     Northeast   5495.111 4570.222   1.000000 71.26444  4.722222 53.96667 132.7778  18141.00
# 2         South   4208.125 4011.938   1.737500 69.70625 10.581250 44.34375  64.6250  54605.12
# 3 North Central   4803.000 4611.083   0.700000 71.76667  5.275000 54.51667 138.8333  62652.00
# 4          West   2915.308 4702.615   1.023077 71.23462  7.215385 62.00000 102.1538 134463.00

## split
states.by.reg = split(states, f = states$Region)
lapply(states.by.reg, FUN = head, 2)

## ggplot
ggplot(data.frame(samp_means)) +
  geom_histogram(aes(x = Values, y = ..density..)) +
  geom_density(aes(x = Values)) +
  facet_wrap(~SampleSize)

ggplot(data = mpg) +
geom_point(mapping = aes(x=displ, y=hwy, color=class))

ggplot(data = mpg) +
geom_point(mapping = aes(x = displ, y = hwy)) +
geom_smooth(mapping = aes(x = displ, y = hwy)) +
geom_point(mapping = aes(x=3, y=30), color = "purple") +
geom_text(mapping = aes(x=3, y=31, label = "New Point"), size=4) +
labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")
  
## regular expression
worth.lines <- grep("td class=\"worth\"", rich)
length(worth.lines)
## [1] 100
net_worths <- gregexpr("\\$[0-9]+,?[0-9]? B", rich[worth.lines])
net_worths <- unlist(regmatches(rich[worth.lines], net_worths))
length(net_worths)
## [1] 100
net_worths[1:5]
## [1] "$72 B" "$58,5 B" "$41 B" "$36 B" "$36 B"