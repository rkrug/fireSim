## [[file:~/Documents/Projects/AlienManagementDrakensberg/sim/packages/fireSim/fireSim.org::*fireBlockBurn%20(./R/fireBlockBurn.R)][fireBlockBurn\ \(\./R/fireBlockBurn\.R\):1]]
##' Fire procedure for block burning
##'
##' This function adjusts the attributre table of the vector layer #fireBlocksLayerName# and adds the following columns:
##' 1) burnsThisYear: containing boolean values indicating if block burns this year (will be overwritten at next call of fireBlockBurn)
##' 2) burns.YEAR: YEAR is replaced by the argument year. containing boolean value indicating if fire occurs in the year YEAR. Will be overwritten at enxt call of fireBlockBurn *if argument year is identical*
##' 3) countFires: creates column if it does not exist and increases by one in blocks which burn
##' 4) lastFire: creates if does not exist and sets to year in blocks which do burn
##' 
##' Uses the data in the vector layer fireBlocksName and the function doesBlockBurnFunction(fireTable) to determine which blocks burn
##' @title 
##' @param pathToGrassDB path to the grass db (*must be sqlite*) containing the attribute table of the layer)
##' @param fireBlocksLayerName name of the vector layer
##' @param doesBlockBurnFunction function determining if a certain block burns, taking the "fires" table as input
##' @param rasterFireOutputLayer name of the to be created raster layer indicating cells which burned (==1) and which did not burn (==NULL)
##' @param year year of evaluation 
##' @return invisible returns the table "fires"
##' @author Rainer M Krug
fireBlockBurn <- function( pathToGrassDB,
                          fireBlocksLayerName = "fire_blocks",
                          doesBlockBurnFunction,
                          rasterFireOutputLayer,
                          year
                          ) {
  ## Connect to grass sqlite db
  m <- dbDriver("SQLite")
  con <- dbConnect(m, pathToGrassDB)
  ## load fire_blocks table
  fires <- dbReadTable(con, fireBlocksLayerName)
  ## determine which blocks burn
  burn <- doesBlockBurnFunction(fires)
  ## add column burnsThisYear
  fires$burnsThisYear <- burn
  ## add column burns%YEAR
  fires[paste("burns", year, sep=".")] <- burn
  ## increase countFires for these
  if (is.null(fires$countFires)) {
    fires$countFires <- as.integer(burn)
  } else {
    fires$countFires <- fires$countFires + as.integer(burn)
  }
  ## set lastFire to this year
  if (is.null(fires$lastFire)) {
    fires$lastFire <- NA
  }
  fires$lastFire[burn] <- as.integer(year)
  ## write table back
  dbWriteTable(con, "fire_blocks", fires, overwrite=TRUE)
  ## close connection
  dbDisconnect(con)
  ## create fireLayerName(year) fire raster layer
  execGRASS(
    "v.to.rast",
    input = fireBlocksLayerName,
    output = rasterFireOutputLayer,
    use = "attr",
    attrcolumn = "burnsThisYear",
    flags = c("overwrite")
    )
  ## set "no fire in cell" to null()
  execGRASS(
    "r.mapcalc",
    expression = paste(
      rasterFireOutputLayer, 
      " = ",
      "if(", rasterFireOutputLayer, ", 1, null())"
      ),
    flags = "overwrite"
    )
  invisible(fires)
}
## fireBlockBurn\ \(\./R/fireBlockBurn\.R\):1 ends here
