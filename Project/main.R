# (c) Deben Oldert, Rens Zuurbier
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
groupLens_movie <- read.csv(returnPath("datasets/groupLens/movies.csv"))
groupLens_movie <- select(tbl_df(groupLens_movie), movieId, title)
# Extract the year
groupLens_movie <- mutate(groupLens_movie, year = as.integer(str_match(title, "([0-9]{4})")[,1]))

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
# Gonna be a clusterf*ck w/ 2GB data (±100 million reviews)
netflix_movie <- read.csv(returnPath("datasets/netflix/movie_titles.csv"))
netflix_movie <- select(tbl_df(netflix_movie), movieId, title, year)
netflix_movie <- mutate(netflix_movie, title_frmt = paste(title, " (", year, ")", sep = ""))

# Create new and empty dataframe for final results
netflix <- data.frame(title = character(0), year = integer(0), rating = numeric(0))

# We need to loop through every movieId to find its .csv file
# Then we calculate the average rating for the movie and store it in a new data.frame
percent_ <- 0
max_ <- nrow(netflix_movie)
for (i in 1:max_) {
  row <- netflix_movie[i,]

  # Since this takes a long time we print the percentage so you know it's still running
  p_ <- as.integer((i / max_) * 100)
  if(p_ > percent_) {
    print(
      paste(
        p_,
        "%",
        sep = ""
      )
    )
    percent_ <- p_
  }

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
  suppressWarnings(netflix <- bind_rows(netflix,
                                       data.frame(title = as.character(row$title_frmt),
                                                  year = as.integer(as.character(row$year)),
                                                  rating = netflix_rating$ratings
                                                  )
                                       )
                  )

  # Cleanup
  remove(csv_)
  remove(netflix_rating)
  remove(j)
  remove(p_)
}

# Cleanup
remove(netflix_movie)
remove(row)
remove(i)
remove(percent_)
remove(max_)

print("DONE")

print("Done Loading")

print("Working on question no. 1...")

# Define ranges
y = 10

x_min <- min(min(imdb$year, na.rm = TRUE), min(groupLens$year, na.rm = TRUE), min(netflix$year, na.rm = TRUE))
x_max <- max(max(imdb$year, na.rm = TRUE), max(groupLens$year, na.rm = TRUE), max(netflix$year, na.rm = TRUE))

# Define colors
color <- rainbow(3)

# Calculate average score for each year per provider
# Make all scores from 1-10
imdb_year_avg <- imdb %>%
  group_by(year) %>%
  summarise(rating = mean(rating, na.rm = TRUE))

groupLens_year_avg <- groupLens %>%
  group_by(year) %>%
  summarise(rating = mean(rating, na.rm = TRUE) * 2)

netflix_year_avg <- netflix %>%
  group_by(year) %>%
  summarise(rating = mean(rating, na.rm = TRUE) * 2)


# imdb =>       rating.x
# groupLens =>  rating.y
# netflix =>    rating

# Merge all the average so we only have years where all 3 provider have data from
yearList <- merge(imdb_year_avg, groupLens_year_avg, by = "year")
yearList <- merge(yearList, netflix_year_avg, by = "year")
yearList <- mutate(yearList, mean = ((rating + rating.x + rating.y) / 3))

# Start the image output for question 1
png(filename=returnPath("output/Q1.png"), height = 400, width = 900, bg = "white")

# Create a line graph
plot(yearList$rating.x,
     type = "l",
     ylim = c(0, y),
     col = color[1],
     axes = F,
     ann = T,
     xlab = "Years",
     ylab = "Avg. rating",
     cex.lab=0.8,
     lwd=2,
     main = "In what movie release year where the average ratings the highest?"
     )

# Format the X and Y axis
axis(1, at=1:length(yearList$year), labels = yearList$year, pos = 0)
axis(2, las = 1, at = 2*0:y, pos = 1)

# Add the other lines (grouplens + netflix)
lines(yearList$rating.y,
      type = "l",
      pch=23,
      lty = 2,
      col = color[2],
      lwd = 2
      )
lines(yearList$rating,
      type = "l",
      pch=23,
      lty = 3,
      col = color[3],
      lwd = 2
      )

# This is the mean line (average of all 3 providers)
# Uncomment below to see the result as a 4th line
# lines(yearList$mean,
#       type = "l",
#       pch=23,
#       lty = 4,
#       col = "yellow",
#       lwd = 2
# )

# Sort the dataframe on descending the mean column (highest mea above)
sorted <- arrange(yearList, desc(mean))

# Get the first row (highest)
highest <- sorted[1,]

# Round the mean to 3 digits
highest$mean <- round(highest$mean, digits = 3)

# Add point in the line graph to show the point with the highest rating
points(
  sum(
    between(sorted$year, min(sorted$year),highest$year)
    ),
  highest$mean,
  pch = 8,
  lwd = 2,
  cex = 2
  )

# Display the rating above the point we just created
text(
  sum(
    between(sorted$year, min(sorted$year),highest$year)
  ) - 0.3,
  highest$mean + 0.7,
  labels = highest$mean
)

# Answer the question
text(
  50,
  10,
  labels = paste(
    "The highest average rating was in:",
    highest$year
  )
)

# Draw a f*cking legend (not a picute of me)
legend(1, 3, set_names, cex = 0.8, col = color, lty=1:3, lwd = 2, bty="n")

# Save the image
suppressMessages(dev.off())

# Pint question + answer to console
print("In what year are the ratings the highest?")

print(paste("That was in:", highest$year, "Score:", highest$mean))

# Cleanup
remove(sorted)
remove(highest)

# Do question no. 2
print("Working on question no. 2...")

# Get for each provider the mean of all ratings and round to 3 digits
netflix_ <- round(mean(yearList$rating), digits = 3)
imdb_ <- round(mean(yearList$rating.x), digits = 3)
groupLens_ <- round(mean(yearList$rating.y), digits = 3)

# Put 'em in a vector
vct <- c(imdb_, groupLens_, netflix_)

# Start image output for question 2
png(filename=returnPath("output/Q2.png"), height = 500, width = 450, bg = "white")

# Create a new bar graph
barplot(
  vct,
  col = color,
  names.arg = set_names,
  ylim = c(0, y),
  axes = TRUE,
  xlab = "Data Provider",
  ylab = "Avg. score",
  main = "Which provider has the highest avrerage score?"
  )

# Print the mean rating for each provider above the bar
text((1 - 0.3), (imdb_ + 0.2), labels = imdb_, col = color[1])
text((2 - 0.1), (groupLens_ + 0.2), labels = groupLens_, col = color[2])
text((3 + 0.1), (netflix_ + 0.2), labels = netflix_, col = color[3])

# Asnwer the question
text(1.5, y - 1, labels = paste("Provider with hightest average score is:", set_names[which.max(vct)]))

# Save the image
suppressMessages(dev.off())

# Print the question + answer
print("Which provider has the highest avrerage score?")

print(paste("Provider with hightest average score is:", set_names[which.max(vct)]))

# Cleanup
remove(netflix_)
remove(groupLens_)
remove(imdb_)
remove(vct)

print("Working on question 3...")

# Create a new list and order it by rating of netflix desc
movieList <- merge(imdb, groupLens, by = "title")
movieList <- merge(movieList, netflix, by = "title")
movieList <- select(movieList, title, rating.x, rating.y, rating)
movieList <- mutate(movieList, rating.y = rating.y * 2, rating = rating *2)
movieList <- arrange(movieList, desc(rating))

# Get the top 5 of the list
top5 <- movieList[1:5,]

# Put in a data frame with 3 rows where column name is movie title
mrx5 <- data.frame(y = 1:3)

# Insert data in data frame
for (i in 1:nrow(top5)) {
  row <- top5[i,]
  mrx5[[row$title]] = c(
    row$rating,
    row$rating.x,
    row$rating.y
  )
}
# Delete empty row
mrx5$y <- NULL

# Start image output
png(filename=returnPath("output/Q3.png"), height = 600, width = 600, bg = "white")

# Plot the graph
barplot(
  as.matrix(mrx5),
  beside = TRUE,
  col = color,
  ylim = c(0, y),
  names.arg = c("", "", "", "", ""),
  main = "How much differce the top 5 of Netflix with other providers?",
  ylab = "Score"
  )

# Add movie titles below the graph
text(
  c(2.5, 6.5, 10.5, 14.5, 18.5),
  par("usr")[3] - 0.3,
  srt=20,
  adj=1,
  labels=names(mrx5),
  xpd=T,
  cex=0.6
  )

# Add legend to graph
legend(6, 10, rev(set_names), cex = 0.8, fill = color, bty="n")

# Save the image
suppressMessages(dev.off())

# Cleanup
remove(mrx5)
remove(top5)
remove(i)

print("The answer of question 3 is in graph Q3")

print("You can find the graphs in the output folder.")
