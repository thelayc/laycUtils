library(dplyr)
# Load data
list.files('./raw-data/raw_nwea')
nwea <- load_csv('./raw-data/raw_nwea/NWEA Reading Fall SY 2013-14.csv')
list.files('./raw-data')
eto <- load_csv('./raw-data/students_list.csv')


# Create custom id --------------------------------------------------------
# Ensure that name of matching variable are the same in both dataset
nwea <- rename(nwea, fname = StudentFirstName, lname = StudentLastName)


eto$my_id <- create_id(eto, var = c('fname', 'lname'))
eto$raw_id <- paste0(eto$fname, eto$lname)
nwea$my_id <- create_id(nwea, var = c('fname', 'lname'))
nwea$raw_id <- paste0(nwea$fname, nwea$lname)

xx <- unique(nwea[, c('my_id', 'raw_id')])
rm(nwea)
yy <- unique(eto[, c('my_id', 'raw_id')])
rm(eto)

# Perfect match -----------------------------------------------------------
#The first matching pass - are there any rows in the two lists that have exactly the same signature?
xy = merge(xx,yy,by = 'my_id', all = T)

matched = subset(xy,subset = (!(is.na(raw_id.x)) & !(is.na(raw_id.y))))

# Identify perfect matches
matched$match_status = "perfect match"


# Partial match -----------------------------------------------------------
#Grab the rows from the first list that were unmatched - that is, no matching item from the second list appears
todo = subset(xy,subset = (is.na(raw_id.y)))

todo <- select(todo, my_id, raw_id = raw_id.x)

# partial match
todo$partials = as.character(sapply(todo$my_id, FUN = agrep, yy$my_id, max.distance = 0.1, value = T))
todo$partials[todo$partials == "character(0)"] <- NA
#todo$dist = as.character(sapply(todo$my_id, agrep, yy$my_id))

#Bring the original text into the partial match list based on the sig key.
partial.matched = merge(todo, yy, by.x = 'partials', by.y = 'my_id', aall.x = TRUE)

#Find the items that were actually partially matched, and pull out the columns relating to signatures and raw text
partial.matched = subset(partial.matched, subset = (!(is.na(raw_id.x)) & !(is.na(raw_id.y))))

#Label these rows as partial match items
partial.matched$match_status = "Partial"
partial.matched <- select(partial.matched, -partials)

# Unmatched ---------------------------------------------------------------
#Find the rows that still haven't been matched
unmatched = subset(todo, subset = (is.na(partials)))
unmatched$match_status <- 'unmatched'

#Add the set of partially matched items to the set of duplicate matched items
matched = rbind(matched,partial.matched, unmatched)











