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
doc <- "Usage: Launch-TSSE-interface [-f FOLDER] [-s SMRNAME] [-i INTERVALSIZE] [-t TAGTYPE] [-d DATETAGTYPE] [-0 1] [-1 2] [--error] [-v] [-h]

-f --folder FOLDER               Directory (folder) with models [default: NULL].
-s --smr SMRNAME                 SMR object filename [default: NULL].
-i --interval-size INTERVALSIZE  Interval size in days [default: 14].
-t --tag-type TAGTYPE            Tag type to make time series with [default: NULL]
-d --date-tag-type TAGTYPE       Tag type to make time series with [default: creation_datez]
-0 --down-value DOWNVALUE        Down value for search vectors [default: 1]
-1 --up-value DOWNVALUE          Up value for search vectors [default: 2]
-v --verbose                     Show exectution steps log [default: FALSE]
-e --error                       Throw error and halt instead of a warning [default: FALSE]
-h --help                        Show this help text"

## docopt parsing
opt <- docopt(doc)

if (opt$folder == "NA" ||
  opt$folder == "NULL" ||
  is.null(opt$folder)) {
  opt$folder <- "../SMR-objects"
}

if (opt$smr == "NA" ||
  opt$smr == "NULL" ||
  is.null(opt$smr)) {
  opt$smr <- "smrForDataAnalysis"
}

opt$interval_size <- as.numeric(opt$interval_size)
if (is.na(opt$interval_size) ||
  opt$interval_size == "NA" ||
  opt$interval_size == "NULL" ||
  is.null(opt$interval_size)) {
  opt$interval_size <- 14L
}

if (is.na(opt$tag_type) ||
  opt$tag_type == "NA" ||
  opt$tag_type == "NULL" ||
  is.null(opt$tag_type)) {
  warning("No tag type was provided (the option --tagtype), making time series over recommender's items.")
  opt$tag_type <- NA
}

if (is.na(opt$date_tag_type) ||
  opt$date_tag_type == "NA" ||
  opt$date_tag_type == "NULL" ||
  is.null(opt$date_tag_type)) {
  opt$date_tag_type <- "creation_date"
}

opt$down_value <- as.numeric(opt$down_value)
if (is.na(opt$down_value) ||
  opt$down_value == "NA" ||
  opt$down_value == "NULL" ||
  is.null(opt$down_value)) {
  opt$down_value <- 1
}

opt$up_value <- as.numeric(opt$up_value)
if (is.na(opt$up_value) ||
  opt$up_value == "NA" ||
  opt$up_value == "NULL" ||
  is.null(opt$up_value)) {
  opt$up_value <- 1
}

if (opt$error) {

}

if(opt$verbose) {
  print(opt)
}

params <- list(folder = opt$folder,
               recommender = opt$smr,
               intervalSize = opt$interval_size,
               tagType = opt$tag_type,
               dateTagType = opt$date_tag_type,
               downIntervalSize = opt$interval_size,
               initOffset = opt$interval_size,
               downvalue = opt$down_value,
               upvalue = opt$up_value,
               verbose = opt$verbose,
               k = 10)

## ----setup--------------------------------------------------------------------
require(stringi)
require(stringr)

require(SparseMatrixRecommender)
require(SMRMon)
require(SparseMatrixRecommenderInterfacesNoDT)

fileName <- file.path(params$folder, paste0(params$recommender, ".RData"))

if (params$verbose) {
  cat("\nSMR file name:", fileName, "\n")
}

if (!file.exists(fileName)) {
  stop(paste("The file name:", fileName), ", does not exist.", call. = TRUE)
}

load(file = fileName)
smrForDataAnalysis <- get(params$recommender)

if (params$verbose) {
  cat("\nLoaded SMR object.\n")
}

## -----------------------------------------------------------------------------

if (is.character(params$dateTagType)) {
  if (!(params$dateTagType %in% SMRMonTakeTagTypes(smrForDataAnalysis))) {
    stop(paste("The date tag type",
               params$dateTagType,
               "(--datetagtype) is not found in the recommender. The known tag types are:",
               paste(SMRMonTakeTagTypes(smrForDataAnalysis), collapse = ", "),
               "."), call. = TRUE)
  }
} else {
  stop("Do not know what to do with the given --datetagtype .")
}

if (is.character(params$tagType)) {

  if (!(params$tagType %in% SMRMonTakeTagTypes(smrForDataAnalysis))) {
    stop(paste("The tag type",
               params$tagType,
               "(--tagtype) is not found in the recommender. The known tag types are:",
               paste(SMRMonTakeTagTypes(smrForDataAnalysis), collapse = ","),
               "."), call. = TRUE)
  }

  tsMat <- SMRCrossTabulateTagTypes(smr = smrForDataAnalysis, tagType1 = params$tagType, tagType2 = params$dateTagType)

  if (params$verbose) {
    cat("\nCross tabulated ", params$tagType, " and ", params$dateTagType, "\n")
  }

} else {

  tsMat <- SMRSubMatrix(smr = smrForDataAnalysis, tagType = opt$dateTagType)

  if (params$verbose) {
    cat("\nTook sub-matrix of ", params$dateTagType, "\n")
  }

}

## -----------------------------------------------------------------------------
rownames(tsMat) <- stringr::str_split_fixed(rownames(tsMat), ":", n = 2)[, 2]
colnames(tsMat) <- stringr::str_split_fixed(colnames(tsMat), ":", n = 2)[, 2]

tsDotMat <- SMRApplyTermWeightFunctions(docTermMat = tsMat, globalWeightFunction = "None", localWeightFunction = "None", normalizerFunction = "Cosine")

if (params$verbose) {
  cat("\nCosine normalized\n")
}

## -----------------------------------------------------------------------------
smrTSLocal <- SMRCreateFromMatrices(matrices = c(tsDotMat),
                                    tagTypes = c("NormalizedTimeSeries"),
                                    itemColumnName = "ID",
                                    imposeSameRowNamesQ = TRUE,
                                    addTagTypesToColumnNamesQ = FALSE)


if (params$verbose) {
  cat("\nCreated time series dot-search SMR\n")
}

## -----------------------------------------------------------------------------
tssmrLocal <- TSCorrSMRCreate(timeSeriesMatrix = tsMat, smr = smrTSLocal, smrNRecs = 200)

if (params$verbose) {
  cat("\nCreated time series main (correlation) SMR\n")
}


## -----------------------------------------------------------------------------
tsSearchVectors <- MakeTimeSeriesSearchVectors(tsMat = tsMat)
length(tsSearchVectors)

if (params$verbose) {
  cat("\nMade \"standard\" search vectors\n")
}


## -----------------------------------------------------------------------------
intervalSize <- params$intervalSize
lsPInds <- purrr::map(1:(ncol(tsMat) - intervalSize), ~.:(. + intervalSize))

tsSearchVectors2 <- purrr::map(lsPInds, function(x) { res <- rep_len(x = params$downvalue, length.out = ncol(tsMat)); res[x] <- params$upvalue; res })
names(tsSearchVectors2) <- purrr::map(lsPInds, function(x) { paste0("∏:", colnames(tsMat)[[x[[1]]]], "+", length(x)) })

tsSearchVectors3 <- purrr::map(lsPInds, function(x) { res <- rep_len(x = params$downvalue, length.out = ncol(tsMat)); res[x[[1]]:ncol(tsMat)] <- params$upvalue; res })
names(tsSearchVectors3) <- purrr::map(lsPInds, function(x) { paste0("S:", colnames(tsMat)[[x[[1]]]]) })

tsSearchVectors4 <- purrr::map(lsPInds, function(x) { res <- rep_len(x = params$downvalue, length.out = ncol(tsMat)); res[1:x[[1]]] <- params$upvalue; res })
names(tsSearchVectors4) <- purrr::map(lsPInds, function(x) { paste0("Z:", colnames(tsMat)[[x[[1]]]]) })

if (params$verbose) {
  cat("\nMade additional search vectors\n")
}

## -----------------------------------------------------------------------------
intervalSize <- 2 * intervalSize
lsPInds <- purrr::map(1:(ncol(tsMat) - intervalSize), ~.:(. + intervalSize))

tsSearchVectors5 <- purrr::map(lsPInds, function(x) {
  res <- rep_len(x = params$downvalue, length.out = ncol(tsMat))

  res[x] <- x - x[[1]] + 1

  #res[x[[length(x)]]:ncol(tsMat)] <- length(x)+1

  res[rev(x + intervalSize)] <- x - x[[1]] + 1

  res
})
names(tsSearchVectors5) <- purrr::map(lsPInds, function(x) { paste0("A:", colnames(tsMat)[[x[[1]]]], "-", length(x)) })


## -----------------------------------------------------------------------------
tsSearchVectors6 <-
  purrr::map(1:params$k, function(n) {
    vec <-
      c(rep_len(x = params$downvalue, length.out = params$initOffset),
        rep_len(x = c(rep_len(x = params$upvalue, length.out = params$intervalSize),
                      rep_len(x = params$downvalue, length.out = n * params$downIntervalSize)),
                length.out = ncol(tsMat)
        )
      )
    vec <- vec[1:ncol(tsMat)]
  })
names(tsSearchVectors6) <- paste0("P:", 1:params$k)


## -----------------------------------------------------------------------------
tsSearchVectorsAll <- c(tsSearchVectors, tsSearchVectors2, tsSearchVectors3, tsSearchVectors4, tsSearchVectors5, tsSearchVectors6)


## -----------------------------------------------------------------------------
if (params$verbose) {
  cat("\nReady to launch Shiny-app...\n")
}

shiny::runApp(TSCorrSMRCreateSearchInterface(tsSMR = tssmrLocal, tsSearchVectors = tsSearchVectorsAll, theme = "superhero"))

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