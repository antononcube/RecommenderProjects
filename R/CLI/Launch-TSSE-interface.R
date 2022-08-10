#!/usr/local/bin/lr

##===========================================================
## Launching of Time Series Search Engine (TSSE) R script
##
## BSD 3-Clause License
##
## Copyright (c) 2022, Anton Antonov
## (See the complete license below)
##===========================================================

# load docopt package from CRAN
suppressMessages(library(docopt))       # we need docopt (>= 0.3) as on CRAN

## configuration for docopt
doc <- "Usage: Launch-TSSE-interface [-f FOLDER] [-s SMRNAME] [-i INTERVALSIZE] [-t TAGTYPE] [-d DATETAGTYPE] [-0 1] [-1 2] [--error] [-h]

-f --folder FOLDER              Directory (folder) with models [default: NULL].
-s --smrname SMRNAME            SMR object filename [default: NULL].
-i --intervalsize INTERVALSIZE  Interval size in days [default: 14].
-t --tagtype TAGTYPE            Tag type to make time series with [default: NULL]
-d --datetagtype TAGTYPE        Tag type to make time series with [default: 'creation_date']
-0 --downvalue DOWNVALUE        Down value for search vectors [default: 1]
-1 --upvalue DOWNVALUE          Up value for search vectors [default: 2]
-e --error                      Throw error and halt instead of a warning [default: FALSE]
-h --help                       Show this help text"

## docopt parsing
opt <- docopt(doc)

#print(opt)

if (opt$folder == "NA" || opt$folder == "NULL" || is.null(opt$folder)) {
  opt$folder <- "../SMR-objects"
}

if (opt$smrname == "NA" || opt$smrname == "NULL" || is.null(opt$smrname)) {
  opt$smrname <- "smrForDataAnalysis"
}

opt$intervalsize <- as.numeric(opt$intervalsize)
if (is.na(opt$intervalsize) || opt$intervalsize == "NA" || opt$intervalsize == "NULL" || is.null(opt$intervalsize)) {
  opt$intervalsize <- 14L
}

if (is.na(opt$tagtype) || opt$tagtype == "NA" || opt$tagtype == "NULL" || is.null(opt$tagtype)) {
  warning("No tag type was provided (the option --tagtype), making time series over recommender's items.")
  opt$tagtype <- NA
}

if (is.na(opt$datetagtype) || opt$datetagtype == "NA" || opt$datetagtype == "NULL" || is.null(opt$datetagtype)) {
  opt$datetagtype <- "creation_date"
}


opt$downvalue <- as.numeric(opt$downvalue)
if (is.na(opt$downvalue) || opt$downvalue == "NA" || opt$downvalue == "NULL" || is.null(opt$downvalue)) {
  opt$downvalue <- 1
}

opt$upvalue <- as.numeric(opt$upvalue)
if (is.na(opt$upvalue) || opt$upvalue == "NA" || opt$upvalue == "NULL" || is.null(opt$upvalue)) {
  opt$upvalue <- 1
}


suppressMessages(library(TTRCreation))

if (opt$error) {

}

params <- list(folder = opt$folder,
               recommender = opt$smrname,
               intervalSize = opt$intervalsize,
               tagType = opt$tagtype,
               dateTagType = opt$datetagtype,
               downIntervalSize = opt$intervalsize,
               initOffset = opt$intervalsize,
               downvalue = opt$downvalue,
               upvalue = opt$upvalue,
               k = 10)

## ----setup--------------------------------------------------------------------
require(stringi)
require(stringr)

require(SparseMatrixRecommender)
require(SMRMon)
require(SparseMatrixRecommenderInterfacesNoDT)

fileName <- file.path(params$folder, paste0(params$recommender, ".RData"))

if( !file.exists(fileName) ) {
  stop(paste("The file name:", fileName), ", does not exist.", call. = TRUE)
}

load(file = fileName)
smrForDataAnalysis <- get(params$recommender)


## -----------------------------------------------------------------------------
if( is.character(params$tagType) ) {

  tsMat <- SMRCrossTabulateTagTypes(smr = smrForDataAnalysis, tagType1 = params$tagType, tagType2 = params$dateTagType)

} else {

  tsMat <- SMRSubMatrix(smr = smrForDataAnalysis, tagType = opt$dateTagType)

}

## -----------------------------------------------------------------------------
rownames(tsMat) <- stringr::str_split_fixed(rownames(tsMat), ":", n=2)[,2]
colnames(tsMat) <- stringr::str_split_fixed(colnames(tsMat), ":", n=2)[,2]

tsDotMat <- SMRApplyTermWeightFunctions( docTermMat = tsMat, globalWeightFunction = "None", localWeightFunction = "None", normalizerFunction = "Cosine")


## -----------------------------------------------------------------------------
smrTSSeller <- SMRCreateFromMatrices( matrices = c(tsDotMat),
                                      tagTypes = c("NormalizedTimeSeries"),
                                      itemColumnName = "ID",
                                      imposeSameRowNamesQ = TRUE,
                                      addTagTypesToColumnNamesQ = FALSE )


## -----------------------------------------------------------------------------
tssmrSeller <- TSCorrSMRCreate( timeSeriesMatrix = tsMat, smr = smrTSSeller, smrNRecs = 200 )


## -----------------------------------------------------------------------------
tsSearchVectors <- MakeTimeSeriesSearchVectors( tsMat = tsMat )
length(tsSearchVectors)


## -----------------------------------------------------------------------------
intervalSize <- params$intervalSize
lsPInds <- purrr::map(1:(ncol(tsMat)-intervalSize), ~ .:(.+intervalSize))

tsSearchVectors2 <- purrr::map( lsPInds, function(x) { res <- rep_len( x = params$downvalue, length.out = ncol(tsMat) ); res[x] <- params$upvalue; res})
names(tsSearchVectors2) <- purrr::map( lsPInds, function(x) { paste0("∏:", colnames(tsMat)[[x[[1]]]], "+", length(x))})

tsSearchVectors3 <- purrr::map( lsPInds, function(x) { res <- rep_len( x = params$downvalue, length.out = ncol(tsMat) ); res[x[[1]]:ncol(tsMat)] <- params$upvalue; res})
names(tsSearchVectors3) <- purrr::map( lsPInds, function(x) { paste0("S:", colnames(tsMat)[[x[[1]]]])})

tsSearchVectors4 <- purrr::map( lsPInds, function(x) { res <- rep_len( x = params$downvalue, length.out = ncol(tsMat) ); res[1:x[[1]]] <- params$upvalue; res})
names(tsSearchVectors4) <- purrr::map( lsPInds, function(x) { paste0("Z:", colnames(tsMat)[[x[[1]]]])})


## -----------------------------------------------------------------------------
intervalSize <- 2 * intervalSize
lsPInds <- purrr::map(1:(ncol(tsMat)-intervalSize), ~ .:(.+intervalSize))

tsSearchVectors5 <- purrr::map( lsPInds, function(x) {
  res <- rep_len( x = params$downvalue, length.out = ncol(tsMat) )

  res[x] <- x - x[[1]] + 1

  #res[x[[length(x)]]:ncol(tsMat)] <- length(x)+1

  res[rev(x+intervalSize)] <- x - x[[1]] + 1

  res
})
names(tsSearchVectors5) <- purrr::map( lsPInds, function(x) { paste0("A:", colnames(tsMat)[[x[[1]]]], "-", length(x))})


## -----------------------------------------------------------------------------
tsSearchVectors6 <-
  purrr::map( 1:params$k, function(n) {
    vec <-
      c(rep_len(x = params$downvalue, length.out = params$initOffset),
        rep_len( x = c(rep_len( x = params$upvalue, length.out = params$intervalSize),
                       rep_len( x = params$downvalue, length.out = n*params$downIntervalSize)),
                 length.out = ncol(tsMat)
        )
      )
    vec <- vec[1:ncol(tsMat)]
  })
names(tsSearchVectors6) <- paste0("P:", 1:params$k)


## -----------------------------------------------------------------------------
tsSearchVectorsAll <- c(tsSearchVectors, tsSearchVectors2, tsSearchVectors3, tsSearchVectors4, tsSearchVectors5, tsSearchVectors6)


## -----------------------------------------------------------------------------
shiny::runApp( TSCorrSMRCreateSearchInterface( tsSMR = tssmrSeller,  tsSearchVectors = tsSearchVectorsAll, theme = "superhero") )

##===========================================================
## Launching of Time Series Search Engine (TSSE) R script
##
## BSD 3-Clause License
##
## Copyright (c) 2022, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
##
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
## Windermere, Florida, USA.
##===========================================================