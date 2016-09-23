# (c) Deben Oldert
# Checks the difference between multiple movie rating from different websites

# Load libraries
suppressMessages(library(dplyr))
suppressMessages(library(stringr))

# Set global vars
wd <- getwd()

# Set common functions

returnPath <- function(pth) {
  return(paste(wd, pth, sep = "/"))
}

# 1. IMDB
# 2. GroupLens
# 3. Netflix

set_names <- c("IMDB", "GroupLens", "Netflix")

# Loading .CSV's
print("Prepairing IMDB...")
imdb <- read.csv(returnPath("datasets/imdb/imdb.csv"), row.names=1)
imdb <- select(tbl_df(imdb), title, rating, year)
imdb <- mutate(imdb, title = paste(title, " (", year, ")", sep = ""))
print("DONE")

print("Prepairing GroupLens...")
groupLens_movie <- read.csv(returnPath("datasets/groupLens/movies_frmt.csv"))
groupLens_movie <- select(tbl_df(groupLens_movie), movieId, title, year)
# Extract the year
#groupLens_movie <- mutate(groupLens_movie, year = as.integer(str_match(title, "([0-9]{4})")[,1]))

groupLens_rating <- read.csv(returnPath("datasets/groupLens/ratings.csv"))
groupLens_rating <- tbl_df(groupLens_rating)
groupLens_rating <- group_by(groupLens_rating, movieId)
groupLens_rating <- summarise(groupLens_rating, rating = mean(rating, na.rm = TRUE))

groupLens <- merge(groupLens_movie, groupLens_rating, by = intersect(names(groupLens_movie), names(groupLens_rating)), all = TRUE)

# Cleanup
remove(groupLens_rating)
remove(groupLens_movie)

print("DONE")

print("Prepairing Netflix...")
# Gonna be a clusterfuck w/ 2GB data (±100 million reviews)
netflix_movie <- read.csv(returnPath("datasets/netflix/movie_titles.csv"))
netflix_movie <- select(tbl_df(netflix_movie), movieId, title, year)
netflix_movie <- mutate(netflix_movie, title_frmt = paste(title, " (", year, ")", sep = ""))

# Create new and empty dataframe for final results
netflix <- data.frame(title_frmt = character(0), year = integer(0), rating = numeric(0))

# We need to loop through every movieId to find its .csv file
# Then we calculate the average rating for the movie and store it in a new data.frame
for (i in 1:nrow(netflix_movie)) {
  row <- netflix_movie[i,]

  print(i)

  # Format the file name
  # E.G:
  # mv_0000001.csv
  # mv_0027640.csv
  csv_ <- "mv_"
  for(j in nchar(row$movieId):6){
    csv_ <- paste(csv_, "0", sep = "")
  }
  csv_ <- paste(csv_, row$movieId, ".csv", sep = "")

  # Prepend the filesystem location (working directory)
  csv_ <- paste(wd, "datasets/netflix", csv_, sep = "/")

  netflix_rating <- read.csv(csv_)

  if(is.null(netflix_rating)) { # If csv is empty => skip it
    print(paste("Empty:", csv_))
    next
  }

  netflix_rating <- tbl_df(netflix_rating)
  netflix_rating <- summarise(netflix_rating, ratings = mean(rating, na.rm = TRUE))

  # Append result to the netflix table
  netflix <- bind_rows(netflix, data.frame(title_frmt = as.character(row$title_frmt), year = as.integer(as.character(row$year)), rating = netflix_rating$ratings))

  # Cleanup
  remove(csv_)
  remove(netflix_rating)
  remove(j)
}

# Cleanup
remove(netflix_movie)
remove(row)
remove(i)
remove(wd)

print("DONE")

print("Done Loading")

print("Working on question no. 1...")

# Define ranges
y = 10

x_min <- min(min(imdb$year, na.rm = TRUE), min(groupLens$year, na.rm = TRUE), min(netflix$year, na.rm = TRUE))
x_max <- max(max(imdb$year, na.rm = TRUE), max(groupLens$year, na.rm = TRUE), max(netflix$year, na.rm = TRUE))
# Define colors

color <- c("blue", "red", "green")

imdb_year_avg <- imdb %>%
                group_by(year) %>%
                summarise(rating = mean(rating, na.rm = TRUE))

groupLens_year_avg <- groupLens %>%
  group_by(year) %>%
  summarise(rating = mean(rating, na.rm = TRUE) * 2)

netflix_year_avg <- netflix %>%
  group_by(year) %>%
  summarise(rating = mean(rating, na.rm = TRUE) * 2)

plot(imdb_year_avg,
     type = "l",
     ylim = c(0, 10),
     col = color[1],
     axes = F,
     ann = T,
     xlab = "Years",
     ylab = "Avg. rating",
     cex.lab=0.8, lwd=2)

#text(axTicks(1), par("usr")[3] - 2, srt=45, adj = 1, labels = x_min:x_max, xpd=T, cex=0.8)

box()

lines(groupLens_year_avg, type = "l", lty = 2, lwd = 2, col = color[2])
lines(netflix_year_avg, type = "l", lty = 3, lwd = 2, col = color[3])

legend("topleft", set_names, cex = 0.8, col = color, lty=1:3, lwd = 2, bty="n")



