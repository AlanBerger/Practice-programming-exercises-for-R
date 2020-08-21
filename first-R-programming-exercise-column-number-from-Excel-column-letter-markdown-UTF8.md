### Alan Berger Aug 20, 2020

Introduction
------------

This is the first in a sequence of programming exercises intended to
fill the gap between learning the correct syntax of basic R commands and
the programming assignments in the R Programming course in the Johns
Hopkins University Data Science Specialization on Coursera. In this
sequence of exercises in "composing" an R function to carry out a
particular task, the idea is to practice correct use of R constructs and
built in functions (functions the "come with" the basic R installation),
while "putting together" a correct sequence of commands that will obtain
the desired result.

In these exercises, there will be a statement of what your function
should do (what are the input variables and what the function should
return) and a sequence of "hints". To get the most out of these
exercises, try to write your function using as few hints as possible.  
Note there are often several ways to write a function that will obtain
the correct result. For these exercises the directions and hints may
point toward a particular approach intended to practice particular
constructs in R and a particular line of reasoning.  
There well may be an existing R function or package that will do what is
stated for this practice exercise, but here (unlike other aspects of the
R Programming course) the point is to practice "putting together" a
logical sequence of steps, with each step a section of code, to obtain a
working function, not to find an existing solution or a quick solution
using a more powerful R construct that is better addressed later on.

Motivation for this exercise
----------------------------

The input data for many functions will be or include a tab delimited
text file that comes from or is naturally viewed as an Excel
spreadsheet. For example, the last programming assignment in the R
Programming course uses a .csv (comma separated variables) text file
appropriately read in using R's read.csv function. It has a total of 46
columns, only 5 of which are relevant to the assignment (Excel columns
B, G, K, Q and W). For this and many other situations, it would be
convenient to have an R function that would take as its input the Excel
column name (as a character variable) and output the corresponding
column number.

### Instruction for this function

For the first version of your function, just have it work for column
letters a through z and only take as input lower case letters. The
skeleton of your function should "look like"

    colnameToNumber <- function(colname) {
    # convert an Excel column name to the 
    # corresponding column number
    # colname should be a lower case letter
    # "between" a and z (including a and z)

    # coding lines

        return(colnumber)
    }

Directions: do NOT use any if statements (yes, one could do 26 if
statements of the form `if(colname == "a") colnumber = 1` and so on,
but, among other things, writing all those out gets pretty boring).

Note that the built in R variable (R object) **letters** is the
character vector with entries "a", "b", ... , "z" and note the built in
R function **which** takes as input a logical vector (often defined by
whether some condition is or is not true) and outputs the integer vector
of the *indices* of the input vector for which the logical value is
**TRUE**, so for example `which(1:4 < 3)` is equal `c(1, 2)`, and
`which(sqrt(1:4) == 2)` is the single value 4 (since `sqrt(1:4)` is the
vector `c(1, 1.414214, 1.732051, 2)` (to the number of digits printed).

Your function, when applied to b, g, k, q and w should return 2, 7, 11,
17 and 23, respectively, and `colnameToNumber("zz")` should "throw an
error" along the lines of

Error in colnameToNumber("zz") : no match to colname

This comes from the R statement: `stop("no match for colname")` which
you should have occur in your function if it is the case that the input
was not a lower case letter between a and z.

Try writing your program now before going to the additional hints below
(the more you do "on your own" the faster you will gain skill at
programming).

### Further hints

You can use the **which** function to "pick out" the index k of the
**letters** vector for which `letters[k]` equals colname, this is the
number you want to return.

How might you check for whether colname was a letter between "a" and
"z"? What will the **which** function return if colname was not a
"valid" value for the colnameToNumber function as currently constructed?

Things to think about: how would you "extend" your function to treat
some additional columns, say through column "dz" (check out the
**paste** function, you could use it along with **letters** to construct
vectors containing additional column names and concatenate them
together). You could do this with several lines of code to get the
vector of "a" through "dz" (a through z; aa through az, ba through bz;
ca through cz; da through dz; then concatenate them into 1 vector). For
many more columns you would want to use a for loop to construct and
concatenate blocks of column names.

How would you extend your function to easily deal with upper case
letters or a "mix" of upper and lower case letters, as in column name Cz
(some people like me are sloppy typists, or will forget about a lower
case restriction and use capital letters as with Excel).  
Now is the time to make use of Google to search for a built in R
function that will convert any upper case letters in a character
variable to lower case (and leave other characters as is).

How would you extend your function to treat a vector (of length &gt; 1)
of column names?

A working version of this function is given below, but try to do your
own function before looking.

    colnameToNumber <- function(colname){
    # convert an Excel column name to the 
    # corresponding column number
    # should not depend on whether the
    # letter(s) in colname are upper or
    # lower case or a mixture of upper and
    # lower case

       colname <- tolower(colname)
    # so only need to deal with lower case

       az <- letters  # c("a", ... , "z")
       aaz <- paste("a", az, sep = "")  
    #  c("aa". "ab", ..., "az")
    # additional column names
       baz <- paste("b", az, sep = "")
       caz <- paste("c", az, sep = "")
       daz <- paste("d", az, sep = "")

    # concatenate the column names
       colnames <- c(az, aaz, baz, caz, daz)

    # get the number corresponding to the input column name
    #  using the which function
       colnumber <- which(colnames == colname)
       if(length(colnumber) != 1) stop("no match to colname")
       return(colnumber)
    }
