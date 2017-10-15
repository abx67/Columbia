####9.29.2017####
mode("d")
mode("cat, squirrel")
class("d")
class("cat, squirrel")
class(" ")
nchar(" "); nchar(" "); nchar("")
class("\n"); class("\""); class("\t")
nchar("\n"); nchar("\""); nchar("\t")

length("cat, squirrel, hedgehog")
length(c("cat", "squirrel", "hedgehog"))
nchar("cat, squirrel, hedgehog")
nchar(c("cat", "squirrel", "hedgehog"))

print("cat, squirrel")
cat("cat, squirrel")
x <- 6
y <- 7
cat("I have", x, "cats and", y, "hedgehogs as pets.")
print("I have", x, "cats and", y, "hedgehogs as pets.")
?cat

print("cat, \n squirrel")
cat("cat, \nsquirrel")
print("In R, an \"array\" is a multi-dimension matrix.")
cat("A group of hedgehogs is called an \"array\".")

phrase <- "Christmas Bonus"
substr(phrase, start = 8, stop = 12)
substr(phrase, start = 13, stop = 13) <- "g"
phrase
fav_animals <- c("cat", "squirrel", "hedgehog")
substr(fav_animals, start = 1, stop = 2)
substr(fav_animals, nchar(fav_animals)-1, nchar(fav_animals))
substr(fav_animals, start = 4, stop = 4)

todo <- "Lecture, Lab, Homework"
strsplit(todo, split = ",")
strsplit(todo, split = ", ")
strsplit(c(todo, "Midterm, Final"), split = ",")


paste("cat", "squirrel", "hedgehog")
paste("cat", "squirrel", "hedgehog", sep = ", ")
animals <- c("cat", "squirrel", "hedgehog")
paste(animals, 1:3)
paste(animals, 1:2)
paste(animals, "(", 1:3, ")")
paste(animals, "(", 1:3, ")", sep = "")
paste(animals, " (", 1:3, ")", sep = "")
paste(animals, " (", 1:3, ")", sep = "", collapse = "; ")

temp<-paste(c("Columbia", "slumber party", "sugarplum")," [",
            c(3,2,7),"-",c(5,4,9),"]",sep='')
cat(temp,sep="; ")


HC <- readLines("HonorCode.txt")
HC2 <- scan("HonorCode.txt", what = "")
length(HC)
head(HC, 5)


grep("students", HC)      #search a word in a string vector
grep("Students", HC)
head(grepl("students", HC), 15)
grep("students", HC)
HC[grep("students", HC)]


HC <- paste(HC, collapse = " ") # One long string
HC
HC.words <- strsplit(HC, split = " ")[[1]] # List output
head(HC.words, 10)

word_count <- table(HC.words)
word_count <- sort(word_count, decreasing = TRUE)
head(word_count, 10)

head(word_count, 10)
tail(word_count, 10)

semicolons <- grep(";", HC)
HC[semicolons]
HC <- gsub(";", "", HC)
HC[semicolons]


fav_animals <- "cat,squirrel, hedgehog, octopus"
strsplit(fav_animals, split = ",")
strsplit(fav_animals, split = " ")
strsplit(fav_animals, split = ", ")



grep("cat|dog", c("categorize", "work doggedly"))
grep("A|b", c("Alabama", "blueberry", "work doggedly"))
grep("A\\|b", c("Alabama", "blueberry", "work doggedly", "P(A|b)"))


HC <- readLines("HonorCode.txt")
HC <- paste(HC, collapse = " ")
HC.words <- strsplit(HC, split=" ")[[1]] # Last Time
head(HC.words, 10)
tail(HC.words, 10)
HC.words <- strsplit(HC, split="(\\s|[[:punct:]])+")[[1]]
head(HC.words, 10)
tail(HC.words, 10)

quakes <- readLines("NCEDC_Search_Results.html", warn = FALSE)
head(quakes)
tail(quakes)
length(quakes)
quakes[8:15]

date_express <- "^[0-9]{4}/[0-9]{2}/[0-9]{2}"
head(grep(quakes, pattern = date_regex))
tail(grep(quakes, pattern = date_regex))
head(grep(quakes, pattern = date_regex, value = TRUE))
head(grep(quakes, pattern = date_regex, value = TRUE))
grep(quakes, pattern = date_regex, invert = TRUE, value = TRUE)

# Is there a match?
grep("a[a-z]", "Alabama")
# Information about the first match.
regexpr("a[a-z]", "Alabama")
# Information on all matches.
gregexpr("a[a-z]", "Alabama")
# What are the matches?
regmatches("Alabama", gregexpr("a[a-z]", "Alabama"))

quakes[11:15]
coord_exp <- "-?[0-9]+\\.[0-9]{4}"
full <- paste(coord_exp, "\\s+", coord_exp, sep = "")

?grepl
head(grepl(quakes, pattern = full), 15)
coord_log <- grepl(quakes, pattern = full)
matches <- gregexpr(pattern = full, text = quakes[coord_log])
head(matches, 1)
coords <- regmatches(quakes[coord_log], matches)
head(coords, 4)
coords_split <- sapply(coords, strsplit, split="\\s+")
head(coords_split, 3)
coords_mat <- matrix(unlist(coords_split), ncol = 2,
                     byrow = TRUE)
colnames(coords_mat) <- c("Latitude", "Longitude")
head(coords_mat)

#install.packages('maps')
library(maps)
map("world")
points(coords_mat[,"Longitude"], coords_mat[,"Latitude"],
       pch = 19, col = "red", cex = .5)

con <- url("http://www.columbia.edu", "r")
x <- readLines(con, warn = FALSE)
head(x, 20)
length(x)
