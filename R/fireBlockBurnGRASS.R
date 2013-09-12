## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/fireSim/fireSim.org::*fireBlockBurn%20(./R/fireBlockBurnGRASS.R)][fireBlockBurn\ \(\./R/fireBlockBurnGRASS\.R\):1]]
##' Fire function for block burning
##'
##' This function simulates block burning, i.e. the burning of whole
##' blocks. The blocks are defined by the polygons in the vector \code{input}
##' layer and this function simly identifies blocks which are burning and
##' adjusts the attributes table of \code{input} (see below for details)
##' accordingly and returns a \code{output} raster layer containing \code{1}
##' for cell burned and \code{NULL} for not burned.
##' 
##' The adjustments of the attributre table of the vector layer \code{input}
##' are as follow:
##' 
##' \enumerate{
##' 
##' \item \bold{\code{burns}}: This \code{logical} column is added if it does
##' not exist yet or owerwritten. It contains \code{logical} values indicating
##' if the corresponding block burns. The values will be overwritten each time
##' the function is called.
##' 
##' \item \bold{\code{burns.YEAR}}: This \code{logical} column will be added
##' if it does not exist or overwritten. YEAR is replaced by the argument
##' \code{year}.  The values are identical to the column \code{burns} but will
##' not be overwritten if the function is called with a different \code{year}
##' argument. Will be overwritten if the function is called with the same
##' \code{year} argument.
##' 
##' \item \bold{\code{countFires}}: This \code{integer} column is added if it
##' does not exist or it will be updated. It contains \code{integer} values
##' indicating the number of times this block has burned. It will be updated
##' each time the function is called.
##' 
##' \item \bold{\code{lastFire}}: This \code{integer} column is added if it
##' does not exist or it will be updated. It contains the \code{year} when the
##' last time the block has burned. If it has not burned yet, the value is
##' \code{NA}. It will be updated each time the function is run.
##' 
##' }
##' 
##' To determine if a block burns, an \R function is passed as the argument
##' \code{doesBlockBurnFunction}. This function has to have the following
##' form:
##'
##' \code{doesBlockBurnFunction <- function(fires)\{...\}}
##'
##' where
##'
##' \code{fires}
##' 
##' is a \code{data.frame} containing the attribute table of the \code{input}
##' layer. This \code{fires} \code{data.frame} has the columns as mentioned
##' above, although they might not contain any values if the function
##' \code{firesBlockBurn} has not been called yet.
##' 
##' The function has to return a \code{logical} vector of the same length as
##' the number of rows in the \code{fires} dataframe.
##'
##' An example would be:
##'
##' \code{doesBlockBurn = function(fires) \{ return(as.logical(rbinom(n=nrow(fires), size=1, 0.5))) \}}
##' 
##' This function \bold{does not} respects MASK in GRASS.
##'  
##' 
##' @usage fireBlockBurnGRASS(input, output, pathToGrassDB, year, doesBlockBurnFunction)
##' @name fireBlockBurnGRASS
##' @title Block burn
##' 
##' @param input name of the vector layer containing the polygons outlining
##' the blocks
##' @param output name of the output raster layer indicating cells which
##' burned (==1) and which did not burn (==NULL)
##' @param pathToGrassDB path to the \bold{sqlite} grass db containing the
##' attribute table of the layer
##' @param year year of evaluation
##' @param doesBlockBurnFunction \R function determining if a certain block
##' burns, taking the "fires" table in the as input
##' 
##' @return invisible returns the updated attribute table of \code{input} as
##' \code{data.frame}
##' @author Rainer M Krug <Rainer@@krugs.de>
##'
##' @export
fireBlockBurnGRASS <- function(
    input,
    output,
    pathToGrassDB,
    year,
    doesBlockBurnFunction
    ) {
    ## Connect to grass sqlite db
    m <- dbDriver("SQLite")
    con <- dbConnect(m, pathToGrassDB)
    ## load fire_blocks table
    fires <- dbReadTable(con, input)
    ## add column burns if it does not exist
    if (!("burns" %in% names(fires))) {
        fires$burns <- FALSE
    }
    ## add column countFires if it does not exist
    if (!("countFires" %in% names(fires))) {
        fires$countFires <- 0
    }
    ## add column lastFire if it does not exist
    if (!("lastFire" %in% names(fires))) {
        fires$lastFire <- NA
    }
    ## determine which blocks burn
    burn <- doesBlockBurnFunction(fires)
    ## update column burnsThisYear
    fires$burnsThisYear <- burn
    ## add column burns%YEAR
    fires[paste("burns", year, sep=".")] <- burn
    ## increase countFires for these
    fires$countFires <- fires$countFires + as.integer(burn)
     ## update lastFire to this year
    fires$lastFire[burn] <- as.integer(year)
    ## write table back
    dbWriteTable(con, "fire_blocks", fires, overwrite=TRUE)
    ## close connection
    dbDisconnect(con)
    ## create fireLayerName(year) fire raster layer
    execGRASS(
        "v.to.rast",
        input = input,
        output = output,
        use = "attr",
        attrcolumn = "burnsThisYear",
        flags = c("overwrite")
        )
    ## set "no fire in cell" to null()
    execGRASS(
        "r.mapcalc",
        expression = paste(
            output, 
            " = ",
            "if(", output, ", 1, null())"
            ),
        flags = "overwrite"
        )
    invisible(fires)
}
## fireBlockBurn\ \(\./R/fireBlockBurnGRASS\.R\):1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
