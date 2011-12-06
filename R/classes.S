

setClass("RDBMSTable", 
          representation(table = "character",
                         dbConnection = "DBIConnection"))


setClass("RDBMSTableWithNames", 
          representation("RDBMSTable", colNames = "character"))

setClass("RDBMSTableRowIndexed", 
          representation("RDBMSTableWithNames", rowIdVariableName = "character"))


#setMethod("initialize",
#           "RDBMSTable",
#           function(.Object, table, dbConnection, virtual = FALSE) {
#
#               .Object@table = table
#               .Object@dbConnection = dbConnection
#
#               .Object
#           })

RDBMSTable =
#
# This is currently very general, possibly too general.
# It takes the name of the table being represented
# and the connection to the database on which queries
# can be made.
#
#  One can call this constructor and specify the desired class 
# assuming that it is a derivative of RDBMSTable.
#
# If left to its own devices, this will attempt to compute the 
# names of the attributes/variables in the table and also see if there
# is an auto_increment variable in the table.

#
# The logic is not quite correct at present. (28 May 05)
#
  
function(table, con, names = NULL, class = "RDBMSTable", 
         virtual = FALSE,
         description = NULL, autoIncrement = character())
{
   if(!virtual && !dbExistsTable(con, table))
       stop("No such table ", table)


   if(extends(class, "RDBMSTableRowIndexed") 
          || (is.logical(autoIncrement) && autoIncrement)
                   || (is.character(autoIncrement) && length(autoIncrement) == 0)) {
     description = dbGetQuery(con,  paste("DESCRIBE ", table))
     if("Extra" %in% colnames(description)) {
       w = grep("auto_increment", description[, "Extra"])
       if(length(w) == 1) {
         autoIncrement = description[w, 1]
         class = "RDBMSTableRowIndexed"
       } else if(length(w) > 1)
          stop("Ambiguous")
     }
   }

   obj = new(class, table = table, dbConnection = con)

     # Clean this up to handle RowIndexed and WithNames in a single
     # call to DESCRIBE.

   if((is.logical(names) && names) || (!is.character(names) || length(names)) && is(obj, "RDBMSTableWithNames")) {
     if(!is.null(description))
       names = description[,1]
     else
       names = names(obj)
   }


   
   if(length(names)) {
     obj = as(obj, ifelse(missing(class), "RDBMSTableWithNames", class))
     obj@colNames = names
   }

   obj
}



setMethod("show", "RDBMSTable",
          function(object) {
             cat("<Remote database table ", object@table, " in ", dbGetInfo(object@dbConnection)$dbname, ">\n", sep = "")
          })
 



# Start with the computations in here.
# But then calling nrow(x) means two queries being sent with only one necessary.
# So reverse the order in which things are called; dim calls nrow and ncol to 
# build up the vector.



setMethod("nrow", "RDBMSTable", 
          function(x) {
              dbGetQuery(x@dbConnection, paste("SELECT COUNT(*) FROM ", x@table))[1,1]             
          })

setMethod("ncol", "RDBMSTable", 
             function(x) {
               nrow(dbGetQuery(x@dbConnection, paste("DESCRIBE", x@table)))
          })


setMethod("dim", "RDBMSTable", 
          function(x) {
             c(nrow(x), ncol(x))                           
          })

# Not needed: setGeneric("dim") 

setMethod("names", "RDBMSTable", 
          function(x) {
             dbGetQuery(x@dbConnection, paste("DESCRIBE", x@table))[,1]
          })


setMethod("names", "RDBMSTableWithNames", 
          function(x) {
             if(length(x@colNames))
               return(x@colNames)

               # if not cached, used inherited method
             callNextMethod()
          })


setMethod("dimnames", "RDBMSTable", 
          function(x) {
             list(NULL, names(x))
          })



getRDBMSTableByColumn =
function(x, i, j, ..., drop = TRUE) 
{

     if(missing(j))
        j = "*"
     else if(is(j, "numeric"))  
        j = names(x)[j]     # Assuming we have an RDBMSTable.  Used to be in method.
           
     cmd = paste("SELECT", paste(j, collapse = ", "), " FROM", x@table)
     ans = dbGetQuery(x@dbConnection, cmd)

     if(drop && ncol(ans) == 1)
        ans = ans[,1]

     ans
}



getRDBMSTableRows =
function(x, i, j, ..., drop = TRUE) {
       if(missing(j))
          j = "*"

       query = paste("SELECT" , paste(j, collapse = ", "), "FROM", x@table)

       rs = dbSendQuery(x@dbConnection, query)
       on.exit(dbClearResult(rs))

       # Fetch the first row of interest.  So we fetch from row 1 up to  i[i].
       # and stick this into 

      n = length(i)
      oi = order(i)
      i = sort(i)

      d = fetch(rs, i[1])

      d = d[nrow(d),]
      d = data.frame(lapply(d, function(x) {
                                   ans = rep(x, n)
                                   if(is(x, "character"))
                                     I(ans)
                                   else    
                                     ans
                               }))

      

       # Ensure that this first element
      d[oi[1],] = d[1,]
     

      for(pos in seq(2, length(i))) {
         # Now go and get the next row of interest, so we read from
         # the next available row up to the index of interest.
         tmp = fetch(rs, i[pos] - i[pos-1])

          # stick the last row of our recently retrieved result.
         d[oi[pos], ] = tmp[nrow(tmp), ]
      }


# Turn the AsIs elements into factors
# dbGetQuery  doesn't do this, so we probably shouldn't bother either.
if(FALSE)  # asFactors)
      d = data.frame(lapply(d, function(x) {
                                   if(is(x, "AsIs") && typeof(x) == "character")
                                     as.factor(x)
                                   else    
                                     x
                               }))


      d[, ...] #, drop = drop
}

if(FALSE) {
# Need to fix the drop  = missing.
setMethod("[", c("RDBMSTable", "missing"), getRDBMSTableByColumn)


setMethod("[", c("RDBMSTable", "numeric") , # Want to handle columns here too.
                  getRDBMSTableRows)

}

# Inheritance.
# Cached values.


# To handle tt[  x > 1 && y < 2, ]
# need to catch before i is evaluated and break the computations up.



# Between a AND b
# IS NOT

SelectFuncs = c("<" = "<", ">" = ">", "<=" = "<=", ">=" = ">=", 
                "&" = "AND",  "|" = "OR", 
                "!=" = "<>", "==" = "=",
                "%in%" = "IN") 


#
# This version allows queries of the form
#
#   table[ A > 100 & A < B || C == 1,  c(1,3) ]
#
#  which is expanded to an SQL query 
#    SELECT A, C FROM  TABLE WHERE  A > 100 AND A < B OR C = 1 
#  more things can be done, and not all computations are possible.
#  
#
# To add:
#    table1[  a > table2$x ]
#
#
#  Can't handle the following:
#
#  both remote and local variables in the same query
#     table[ a > x ]
#  where a is local and x is remote (i.e. on the table).
#
#    a > log(d)   
#
#
#
#  There is no mechanism currently to handle defining additional methods
# for particular types of i and j.  Defining these would cause the method dispatch
# to evaluate the arguments and that would lead to errors since the variables are not
# local, but remote.
# An entirely different approach is to construct an environment for the call
#  that provides the remote variables as local variables and handles their access 
# cleverly.  
# This is a mini-interpreter/dispatch mechanism.  It is not elegant, because of the
#  need for lazy evaluation in the context of eager evaluation for method dispatch.
#
#
#

setMethod("[", c("RDBMSTable", "ANY", "ANY"),
           function(x, i, j, ..., drop = TRUE) {
               if(missing(i)) {
                  # callNextMethod()
                  return(getRDBMSTableByColumn(x, , j, ..., drop = drop))
                }

               if(missing(j))
                  j = "*"

               if(is(j, "numeric"))
                 j = names(x)[j]

               cl = sys.call()[[3]] # i
               if(!is(cl, "call")) {
                  # callNextMethod()
                  return(getRDBMSTableRows(x, i, j, ..., drop = drop))
               }


               if(!(as.character(sys.call()[[3]][[1]]) %in% names(SelectFuncs))) {
                  return(getRDBMSTableRows(x, i, j, ..., drop = drop))
               }

               cmd = paste("SELECT ", paste(j, collapse = ", "), "FROM", x@table,
                             "WHERE", expandCallRestriction(cl, names(x), sys.parent()))


               dbGetQuery(x@dbConnection, cmd)
           })



# o = expression(x > 1 & y < x | z == 1)[[1]]
#

#

expandCallRestriction =
#
#  Here, we are given a call and a collection of variables that are in the table in the database.
# Our job is to expand the call into an SQL command that makes sense.
#  The intent is to have this return the WHERE clause of the SQL command.
#  This may need to return not just the restriction, but also any AS aliases.
  
#  We possibly need the names of the other tables in the database so that we can link
#  to those

#
#  We need to add things like mean(x) -> AVERAGE(x) and recognize these calls
#  and possibly make complex maps like  median(x) into separate calls.
#  Do we need to the database connection at this point.
#
function(call, variableNames, envir = sys.parent())
{
  op = match(as.character(call[[1]]), names(SelectFuncs))

  if(is.na(op))
     stop("Unknown operation")

  op = SelectFuncs[op]
  if(as.character(call[[1]]) %in% c("|", "&")) {
    paste(expandCallRestriction(call[[2]], variableNames, envir), op, expandCallRestriction(call[[3]], variableNames, envir))
  } else {
     a = call[[2]]
     b = call[[3]]


     if(is(a, "character"))
        a = paste("'", a, "'", sep = "", collapse = ", ")

    if(op == "IN") {
      b = eval(call[[3]], envir)
      if(is(b, "character"))
        b = paste("(", paste("'", b, "'", sep = "", collapse = ", "), ")")
    } else {
       if(is(b, "character"))
          b = paste("'", b, "'", sep = "", collapse = ", ")
    }
       
     paste(a, op, b)
  }
}





# Orthogonal inheritance for a "slow" 





setClass("CachedRDBMSTable",
          representation("RDBMSTable", state = "environment"))


setMethod("initialize", "CachedRDBMSTable",
            function(.Object, table, dbConnection, state = new.env()) {
                   #XXX need to have the arguments explicitly here.
               .Object = callNextMethod(.Object, table = table, dbConnection = dbConnection)

                  # initialize the cache.
               if(!exists("data", state))
                    state$data = data.frame()

               if(!exists("indices", state))
                    state$indices = integer(0)

               .Object@state = state
                
               .Object
            })

setMethod("getRDBMSTableRows",
           "CachedRDBMSTable",
            function(x, i, j, ..., drop = TRUE) {

               if(all(i %in% x@state$indices)) {
                 return(x@state$data[i, ])
               }

               vals = callNextMethod()


               update(x, vals, i, j)

               vals               
            })


setMethod("update",
           "CachedRDBMSTable",
            function(object, vals, i, j) {

              object@state$indices = unique(c(object@state$indices, i)) 

              if(length(object@state$data) == 0)
                  object@state$data = vals
               else {
                 object@state$data[i, j] = vals
               }

               object
            })
