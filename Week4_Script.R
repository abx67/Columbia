# Class Script
# September 29

class("d")
class("Columbia University")

class(" ")
nchar(" "); nchar("  ")

class("\n"); nchar("\n")
class("\""); nchar("\"")

length("cat, squirrel, hedgehog")
length(c("cat", "squirrel", "hedgehog"))

nchar("cat, squirrel, hedgehog")
nchar(c("cat", "squirrel", "hedgehog"))

print("cat, squirrel")
cat("cat, squirrel")

x <- 6
y <- 7

cat("I have", x, "cats and ", y, "hedgehogs as pets")
print(c("I have", x, "cats and ", y, "hedgehogs as pets"))

print("cat, \n squirrel")
cat("cat, \n squirrel")

print("Columbia\tUniversity")
cat("Columbia\tUniversity")

nchar("Columbia\tUniversity")
nchar("Columbia University")

# Substring

phrase <- "Christmas Bonus"
substr(phrase, start = 8, stop = 12)
substr(phrase, start = 13, stop = 13) <- "g"

fav_animals <- c("cat", "squirrel", "hedgehog")
substr(fav_animals, start = 1, stop = 2)
substr(fav_animals, start = nchar(fav_animals)-1, stop = nchar(fav_animals))
substr(fav_animals, start = 4, stop = 4)

todo <- "Lecture, Lab, Homework"
strsplit(todo, split = ",")
strsplit(todo, split = ", ")

strsplit(c(todo, "Midterm, Final"), split = ",")

paste("cat", "squirrel", "hedgehog")
paste("cat", "squirrel", "hedgehog", sep = ", ")

animals <- c("cat", "squirrel", "hedgehog")
paste(animals, 1:3)
paste(animals, "(", 1:3, ")")
paste(animals, "(", 1:3, ")", sep = "")
paste(animals, "(", 1:3, ")", sep = "", collapse = "; ")

lum.vec <- c("Columbia University", "slumber party", "sugarplum")

substr(lum.vec, start = c(3, 2, 7), stop = c(5, 4, 9))
strsplit(lum.vec, split = "lum")

paste(lum.vec, " [", c(3, 5, 7), "-", c(5, 4, 9), "]", sep = "", collapse = "; ")

HC  <- readLines("HonorCode.txt")
HC2 <- scan("HonorCode.txt", what = "")

HC[grep("Students", HC)]
grep("students", HC)

HC <- paste(HC, collapse = " ")
HCwords <- strsplit(HC, split = " ")[[1]]

word_count <- table(HCwords)
word_count <- sort(word_count, decreasing = TRUE)

head(word_count, 10)

semicolons <- grep(";", names(word_count))
names(word_count)[semicolons]


names(word_count) <- gsub(";", "", names(word_count))
names(word_count)[semicolons]

fav_animals <- "cat,squirrel, hedgehog,  octopus"

strsplit(fav_animals, split = ",")
strsplit(fav_animals, split = ", ")

# Regular Expression

grep("cat|dog", c("categorize", "work doggedly", "CAT"))
grep("A|b", c("Alabama", "categorize", "blueberry"))
grep("A\\|b", c("Alabama", "P(A|b)"))


grep("^[^A-Z]", c("Hello!", "cat,", "1dsad93a"))
grep("[a-z,]$", c("Hello!", "cat,", "1dsad93a"))


# Earthquakes
quakes <- readLines("NCEDC_Search_Results.html", warn = FALSE)


date_regex <- "^[0-9]{4}/[0-9]{2}/[0-9]{2}"

head(grep(quakes, pattern = date_regex))
tail(grep(quakes, pattern = date_regex))

head(grep(quakes, pattern = date_regex, value = TRUE))
tail(grep(quakes, pattern = date_regex, value = TRUE))

grep(quakes, pattern = date_regex, invert = TRUE, value = TRUE)


id_express <- "[0-9]{12}$"

head(grep(quakes, pattern = id_express))
tail(grep(quakes, pattern = id_express))

grep(quakes, pattern = id_express, invert = TRUE, value = TRUE)

grep("a[a-z]", "Alabama")
grepl("a[a-z]", "Alabama")
regexpr("a[a-z]", "Alabama")
gregexpr("a[a-z]", "Alabama")
regmatches("Alabama", gregexpr("a[a-z]", "Alabama"))


coord_exp <- "-?[0-9]+\\.[0-9]{4}"
full  <- paste(coord_exp, "\\s+", coord_exp, sep = "")


head(grepl(quakes, pattern = full), 15)
coord_log <- grepl(quakes, pattern = full)

matches <- gregexpr(pattern = full, text = quakes[coord_log])
head(matches, 1)

coords <- regmatches(quakes[coord_log], matches)
head(coords, 4)
tail(coords, 4)

coords_split <- sapply(coords, strsplit, split = "\\s+")
head(coords_split)

cs_unlisted <- unlist(coords_split)

coords_mat <- matrix(cs_unlisted, ncol = 2, byrow = TRUE)
head(coords_mat)

colnames(coords_mat) <- c("Latitude", "Longitude")
head(coords_mat)

install.packages("maps")
library(maps)

map("world")
points(coords_mat[, "Longitude"], coords_mat[, "Latitude"], pch = 19, col = "red", cex = .5)




con <- url("http://www.columbia.edu", "r")
x <- readLines(con, warn = FALSE)


head(x, 20)