music <- read.csv("./lastfm.csv")
View(music)
attach(music)

# Attributes / Column names
names(music)

# Column datatypes
sapply(music, class)

# Dimensions
dim(music)

# Check if there are cells with unknown values
any(is.na(music))

# Summary
summary(music)

users = unique(subset(music, select=c("user", "sex", "country")))

# Number of users
dim(users)

# Remove user ID
users = subset(users,select = -user)

library(Rcmdr)

# Sex distribution
with(users, Barplot(sex, xlab="sex", ylab="Percent", scale="percent"))

# Top 15 countries
topCountries = as.data.frame(head(sort(table(users$country),decreasing=TRUE),15))

# Top 15 artists
artistsPerUser = unique(subset(music, select = c("user","artist")))
head(sort(table(artistsPerUser$artist),decreasing=TRUE),15)

# Number of artists
length(unique(music$artist))

# Number of countries
length(unique(music$country))

# Top 15 artists per country
artistsPerCountry = unique(subset(music, select = c("user","artist", "country")))
countPerCountry = count(artistsPerCountry, vars=c("artist", "country"))
countPerCountry[countPerCountry$country %in% topCountries$Var1, ]


###############################
# Binarization
##############################

library(arules)

mba <- split(x = music[,"artist"], f = music$user)
mba <- lapply(mba, unique)
transactions <- as(mba, "transactions")


# Dimensions
dim(transactions)

# Summary
summary(transactions)

# Labels
itemLabels(transactions)

####################################
# Generate association rules
####################################

rules <- apriori(transactions, parameter=list(support=0.03, confidence = 0.03, minlen=2))

# Sort by confidence
rules <-sort(rules, by=c("confidence", "support"), decreasing=TRUE)
inspect(head(rules,20))

# Sort by confidence
rules <-sort(rules, by=c("support"), decreasing=TRUE)
inspect(head(rules,20))

# Sort by confidence and support
rules <-sort(rules, by=c("confidence", "support"), decreasing=TRUE)
inspect(head(rules,20))
