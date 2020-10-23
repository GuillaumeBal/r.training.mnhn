## --------------------------
## 3. Data manipulation in R:
## --------------------------

## ------------------------------------------------------------------
## 3.1 Accessing/extracting a part of a data set - case of vectors:

## Cf. previous example file!

## ----------------------------------------
## 3.2.1 When there are several dimensions:
(mat <- matrix(data = 1:12, ncol=3,
               dimnames = list(letters[1:4], LETTERS[1:3])))

mat[1, ]        # First row
mat[ , 2]       # Second column

mat[3:4, 2:3]    # Extraction by position
mat[4:3, 2:3]    # The same inversing the rows order.

mat[c(TRUE, FALSE, TRUE, FALSE), ]   # Extraction by logical indexation #note empty comma after column, shows working on rows
                                     # (also works for columns).


mat[ , c("C", "A")]   # Extraction by name (also works for rows). Note 1st empty comma. Shows working on columns

## Remarks on matrix indexation:

# Different methods can be combined:
mat[c(3, 1) , c(TRUE, FALSE, TRUE)]

# "simplification" by R:
mat[ , 1]         # You've selected one column but you get one "row"!!?
class(mat)
class(mat[ , 1])  # ... Okay, it's been changed in a vector

mat[ , 1, drop=FALSE]         # That sounds better
class(mat[ , 1, drop=FALSE])  # ... much better (at least if you aimed at keeping it as a matrix)



## ------------
## 3.2.2 Lists:
list1 <- list(vect1 = 1:5,
              mat = matrix(letters[1:9], nrow=3),
              empty = NULL)

print(list1)

## Extraction of one element by position:
list1[[1]]   # Notice the double sq. brakets.

list1[[2]][ , 1:2]    # Return a matrix, of which a part can be extracted.

## Extraction of one element by name:
list1[["vect1"]]

el <- "vect1"   # Note that a viariable storing the element name can be used
list1[[el]]

list1$mat   # Used extensively, name of the element without quotes.

## Range of elements with [...]
list1[c(1, 3)]

list1[c("mat", "vect1")]   # Mind the order!

list1[c(FALSE, TRUE, FALSE)]  # Note that even when only one element selected, it is still a list.

## ------------------
## 3.2.3 Data.frames:

dataf <- data.frame(Indiv = letters[1:5],
                    treatment = rep(c(FALSE, TRUE), length.out = 5),
                    value = rnorm(n = 5, mean = 10, sd = 1))

dataf

## It's a list, so we can extract a list element (column) using list specific methods:
dataf$value  # return the column as a vector.
             # Extensively used!

dataf[[1]]   # Column 1 ; almost never used!

# Just to check it indeed behave like a list:
dataf[c("Indiv", "treatment")]   # Extraction of a range of columns
dataf["treatment"]               # Just as for the list, the data.frame structure is kept
                                 # even if only one column selected

# This is rarely used (probably because it can be confunded with the access through dimensions).

## But it also has two dimensions:
dim(dataf)
nrow(dataf)
ncol(dataf)

dataf[dataf$treatment, ]  # Selection of rows using   #$ used to identify a row or column name in a bigger dataset
dataf[ ! dataf$treatment, ]

# But I'm actually only interested in the value for those who had the treatment:
dataf[dataf$treatment, "value"]  # Okay, thats it.
                                 # But if for some reason I need to keep it a data.frame:

(dataf2 <- dataf[dataf$treatment, "value", drop=FALSE])
class(dataf2)

dataf[dataf$treatment, 3, drop=FALSE]
dataf[dataf$treatment, "value"]
dataf[dataf$treatment, c("value", "Indiv"), drop=FALSE]

dataf[ ! dataf$treatment, , drop=FALSE]

dataf[dataf$value > 10 , , drop = FALSE]
dataf[which(dataf$value > 10) , , drop = FALSE]

dataf3 <- dataf
dataf3[1, "value"] <- NA
dataf3
dataf3[dataf3$value > 10 , , drop = FALSE]
dataf3[which(dataf3$value > 10) , , drop = FALSE]

# Only notable difference with indexation of matrices,
# a single row will NOT be converted in a vector (possibly different data types):
dataf[1, ]  # Compare with mat[1, ] / mat[1, , drop = FALSE]

## ------------------------------------
## 3.3 Modification of subsets of data:

## Change as you access:

# Vector: see previous example file!

# Data.frame:
dataf[ ! dataf$treatment, "value"] <- dataf[ ! dataf$treatment, "value"] - 1
dataf

dataf$value[! dataf$treatment] <- dataf[ ! dataf$treatment, "value"] + 2
dataf

## Further example of the "change as you access" rule:
names(list1)   # You can query the names using names().
names(list1) <- c("Index", "someLetters", "nothing") # but you can also change element names
list1                                                # using the function names()

colnames(mat)
colnames(mat)[2]
colnames(mat)[2] <- "varB"  # You can even uses indices to only change some of the names.
colnames(mat)

## -----------------------------------
## Indices and names for adding stuff:
## shortNamedVect <- 1:5
## names(shortNamedVect) <- letters[1:5]
shortNamedVect[8] <- 12     # Just for your information...
shortNamedVect              # You'll generally rather concatenate vectors instead (function c(...)).
shortNamedVect["ZZ"] <- 0
shortNamedVect

mat
mat[4, 4:5] <- 0   # But it won't work for matrices

# Adding/removing columns in data.frames:
dataf$calculatedField <- (dataf$value - 3) ^ 2
dataf[ , "otherField"] <- NA
dataf

dataf$otherField <- NULL  # Suppress a column.
dataf


## -----------------------------
## 3.4 Cleaning/Subsetting data:


(datasub1 <- dataf[dataf$value > 10.5, ])

dataf[c(-1, -4), ]   # removing rows by position (equivalent dataf[-c(1, 4), ])
dataf <- dataf[ , -4]         # removing columns by position.


datasub2 <- subset(x = dataf,
                   value > 10.5)  # Note that the name of the column can be directly
                                  # used as a vector

## Note the difference in handling NAs in tests:
(datasub3 <- dataf3[dataf3$value > 10.5, ])
(datasub4 <- subset(x = dataf3,
                    value > 10.5))

datasub2  # strictly equivalent to the former method.

subset(x = dataf,
       value > 10 & treatment)[ ,     # Combination of tests...
                                c("Indiv", "value")]  # ...and selection of columns.

subset(x = dataf,
       value > 10 & treatment,
       select = c("Indiv", "value")) # Even better, uses the features of "subset"!

## -----------------------
## 3.5 Conversion of data:

## -------------------
## 3.5.1 Generalities:

## Characters to numeric
as.numeric(c("1", "4.5", NA, "-12", "ImNotANumber"))


## To dates:
(date1 <- as.Date(10419, origin="1970-01-01"))  # From numeric
                                                # [call as.Date.numeric() in the background.]

(date1 <- as.Date(c("2015-10-21")))   # From characters.
class(date1)

as.Date("31/12/2012")   # R is not very happy with non-US formats
                        # (well, I guess it may depends on the locals)

(date3 <- as.Date(x = "31/12/2012",     #  [call as.Date.character() in the background.]
                  format = "%d/%m/%Y"))  # ...so you have to specify the format

?strptime  # for a comprehensive description of the format argument.

## -----------------------------------
## 3.5.2 [!!!] The duality of factors:

fact1 <- factor(sample(x = c(letters[1:5], LETTERS[1:5]),
                       size = 10,
                       replace = TRUE))

fact1
as.numeric(fact1)    # Your factor will sometimes behave like a numeric.

fact2 <- factor(as.character(1:11))

fact2
as.numeric(fact2)    # !!! You may have a "numeric label" which doesn't match the numeric value!
as.character(fact2)
as.numeric(as.character(fact2))

age <- factor(c(1:10, "11+"))
age

as.numeric(age)
as.numeric(as.character(age))

# => Always do explicitely the conversion if you have any doubt about
#    which way the factor will behave.

## -------------------------
## Formatting to characters:

## To characters:
# as.character will work with almost anything without further argument.

as.character(c(1, 5.65842, -Inf, NA))

as.character(fact1)

# But sometimes you wants more control (formating):

format(c(5, 1002.569874, sqrt(2))  # Formatting numerics.
       ,trim = FALSE
       ,digit = 3
       ,nsmall = 2
       ,justify = "left"
       #,width = 15
       )

?format  # but also ?sprintf, ?formatC

format(c(date1, date3),  # Formatting dates [call format.Date()].
       format = "%d/%m")

(now <- Sys.time())
format(x = now,                 # Formating POSIXct works just the same.
       format="%d/%m/%Y %H:%M") # ?strptime again for help on the format


## ------------------
## 3.6 Ordering data:

shortNamedVect <- 1:5
names(shortNamedVect) <- letters[1:5]
shortNamedVect[3] <- NA

shortNamedVect  # Unordered vector
sort(shortNamedVect)
sort(shortNamedVect, na.last=TRUE, decreasing=TRUE)
sort(shortNamedVect, na.last=FALSE)

sort(dataf$value)

## Changing the order in data.frame
order(dataf$value)
dataf[order(dataf$value), ]  # ordering data by value.

dataf <- dataf[order(dataf$treatment,
                     dataf$value,
                     decreasing = TRUE), ]  # ordering data by treatment, then value.

dataf

