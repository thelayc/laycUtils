## ---- fig.show='hold'----------------------------------------------------
data(eto)
head(eto)

data(nwea)
head(nwea)

## ---- fig.show='hold'----------------------------------------------------
eto$my_id <- create_id(eto, var = c('lname', 'fname'))
head(eto)

nwea$my_id <- create_id(nwea, var = c('StudentLastName', 'StudentFirstName'))
head(nwea)

## ---- fig.show='hold'----------------------------------------------------
df <- fuzzy_join(x = nwea, y = eto, by = 'my_id')

head(df)

