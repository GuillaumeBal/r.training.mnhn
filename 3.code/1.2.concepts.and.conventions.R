#-*- coding: latin-1 -*-

### File: 1.2.Concepts.and.conventions.R
### Time-stamp: <2018-09-10 09:08:41 yreecht>
###
### Created: 17/11/2017	15:13:25
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

## -------------------
## 1. GETTING STARTED:
## -------------------

#####################################################################################
## -----------------------------
## 1.2 Concepts and conventions:

## ---------------
## 1.2.1 Console / Scripts / Comments:

## This file is a script!
##
## Usefull tip in R-studio: send the code line by line (or a selection) to the console using
## ctrl + Enter

# I am a comment, I won't be evaluated!
### ... and the number of "#" does not really matters


message("Hello ",    # I can also comment the end of a line...
        "World")     # ...even if the instruction isn't completed!

## Note that the former block is strictly equivalent to:

message("Hello ", "World")  # ...so instructions can be splitted
                            # on several lines.


## --------------------------------------
## 1.2.2 Variable declaration/assignment:
myVar <- 1
print(myVar)

## !!!
_myvar <- 2     # a variable name cannot begin with _
1myvar <- 2     # nor a digit.

my_var1 <- 2    # ... but they can be used somewhere else.
my_var1         # Note that when an object is evaluated in the console
                # it is simply printed

## !!! case sensitive:
myvar
myVaR

## ----------------------------
## 1.2.3 Invoquing a functions:

## A function usually takes arguments and use them to fulfil a task.

rep(x=myVar, times=5)  # using named arguments.
rep(times=5, x=myVar)  # ... the order doesn't matter.

rep(myVar, 5)   # using the order of arguments (risk of error!)
rep(5, myVar)   # !!! to use only with usual functions which have few - surely known - arguments.


## Note that the result of a function may very well be stored in a variable:
myVar5rep <- rep(x=myVar, times=5)
print(myVar5rep)

## --------------------------------
## 1.2.4 Getting help on functions:
?rep   # have a look to the help panel if in R-studio!
?mode

help(rep)   # Equivalent to ?rep

## Help on a topic:
help.search('generalized linear model')
??"working directory"

## Search tool in a browser:
help.start()
