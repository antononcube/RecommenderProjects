##===========================================================
## SMR creation for data analysis in R
##
## BSD 3-Clause License
##
## Copyright (c) 2019, Anton Antonov
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


library(Matrix)
library(tidyverse)
library(lubridate)
library(SparseMatrixRecommender)
library(SMRMon)

#' Creation of data analysis SMR
#' @param dirName Directory name of the SMR matrix. (Long form.)
#' @param fileName File name of long form of the SMR matrix.SMR
#' @param itemColumnName The item column name.
#' @param tagTypeColumnName Tag type column name.
#' @param weightColumnName Weight column name.
#' @param dateTagType Tag type for date.
#' @param noLowerCasing Should the lower case be applied to the tags or not?
#' @param minDate Minimum date to filter the data with.
CreateDataAnalysisSMR <- function(dirName = getwd(),
                                  fileName = "data/dfSMRMatrix.csv",
                                  itemColumnName = "ID",
                                  tagTypeColumnName = "TagType",
                                  tagColumnName = "Tag",
                                  weightColumnName = "Weight",
                                  dateTagType ="CreationDate",
                                  noLowerCasing = NULL,
                                  minDate = NULL) {

  require(Matrix)
  require(tidyverse)
  require(lubridate)
  require(SparseMatrixRecommender)
  require(SMRMon)


  ## ----ingest-matrix-csv-------------------------------------------------
  cat("\n\tingest-matrix-csv\n")
  system.time({
    dfMatrix <- read.csv(file.path(dirName, fileName))
    dfMatrix <- dfMatrix %>% dplyr::mutate(X = NULL)
  })
  cat("\n\t\tdim(dfMatrix) :\n")
  print(dim(dfMatrix))



  ## ----head-dfMatrix------------------------------------------------------
  cat("\n\thead-dfMatrix\n")
  cat("\n\t\thead(dfMatrix) :\n")
  print(head(dfMatrix))



  ## ----unique-tag-types---------------------------------------------------------
  cat("\n\tunique-tag-types\n")
  cat("\n\t\tunique(dfMatrix$tag_type) :\n")
  print(unique(dfMatrix[[tagTypeColumnName]]))


  ## ----create-wide-form-with-datetime-taxonomies--------------------------------
  cat("\n\tcreate-wide-form-with-datetime-taxonomies\n")
  print(system.time({

    dfMatrixDateTime <- dfMatrix[dfMatrix[[tagTypeColumnName]] == dateTagType, ]

    dfMatrixDateTime$datetime <- as.POSIXct(dfMatrixDateTime[[tagColumnName]])

    dfMatrixDateTime <-
      dfMatrixDateTime %>%
      dplyr::mutate( date = as.character(as.Date(datetime, format = "%Y-%m-%d")),
                     month = paste0(formatC(as.integer(lubridate::month(datetime, label = FALSE)), digits=1, flag=0),
                                    "-",
                                    lubridate::month(datetime, label = TRUE, abbr = TRUE)),
                     weekday = paste0(lubridate::wday(datetime, week_start=1, label = FALSE), "-", lubridate::wday(datetime, label = TRUE, abbr = TRUE)),
                     week = as.character(lubridate::isoweek(datetime)))

    if( !is.null(minDate) ) {
      dfMatrixDateTime <-
        dfMatrixDateTime %>%
        dplyr::filter( datetime >= as.Date(minDate))
    }

    dfMatrixDateTime <- dfMatrixDateTime[,  c(itemColumnName, "date", "month", "weekday", "week")]
  }))


  ## ----create-long-form-with-datetime-taxonomies--------------------------------
  cat("\n\tcreate-long-form-with-datetime-taxonomies\n")
  dfMatrixDateTime <-
    dfMatrixDateTime %>%
    tidyr::pivot_longer(cols = -c(itemColumnName), names_to = tagTypeColumnName, values_to = tagColumnName)

  dfMatrixDateTime[[tagTypeColumnName]] <- paste0("creation_", dfMatrixDateTime[[tagTypeColumnName]])
  dfMatrixDateTime[[weightColumnName]] <- 1

  cat("\n\t\tdfMatrixDateTime :\n")
  print(head(dfMatrixDateTime))



  ## ----head-dfMatrixDateTime----------------------------------------------
  cat("\n\thead-dfMatrixDateTime\n")
  cat("\n\t\thead(dfMatrixDateTime) :\n")
  print(head(dfMatrixDateTime))


  ## ----make-dfLongForm----------------------------------------------
  cat("\n\tmake-dfLongForm\n")
  dfLongForm <-
    rbind( dfMatrix[, c(itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName)],
           dfMatrixDateTime )

  if ( is.null(noLowerCasing) ) {

    dfLongForm[[tagColumnName]] <- tolower(dfLongForm[[tagColumnName]])

  } else if (is.character(noLowerCasing)) {

    dfLongForm[[tagColumnName]] <-
      ifelse(
        dfLongForm[[tagTypeColumnName]] %in% noLowerCasing,
        dfLongForm[[tagColumnName]],
        tolower(dfLongForm[[tagColumnName]]))

  }

  ## ----create-smrDateTime-------------------------------------------------
  cat("\n\tcreate-smrDateTime\n")
  print(system.time(
    smrDateTime <-
      SMRCreateFromLongForm( dfLongForm,
                             itemColumnName = itemColumnName,
                             tagTypeColumnName = tagTypeColumnName,
                             valueColumnName = tagColumnName,
                             weightColumnName = weightColumnName,
                             addTagTypesToColumnNamesQ = TRUE)
  ))


  ## ----get-smrDateTime-sub-matrices---------------------------------------
  cat("\n\tget-smrDateTime-sub-matrices\n")
  lsSMats <- purrr::map( smrDateTime %>% SMRMonTakeTagTypes, function(x) SMRSubMatrix(smrDateTime, x))
  names(lsSMats) <- smrDateTime %>% SMRMonTakeTagTypes


  ## ----print-sub-matrices-dimensions, rows.print=20------------------------------
  print(as.data.frame(purrr::map_df(lsSMats, ~ data.frame( NRows = nrow(.), NColumns = ncol(.) ), .id = "TagType")))


  ## -----------------------------------------------------------------------------
  # assertthat::assert_that( mean(sort(colnames(lsSMats[["creation_date"]])) == colnames(lsSMats[["creation_date"]])) == 1 )

  ## Result
  list( LongForm = dfLongForm, SMR = smrDateTime, Matrices = lsSMats)
}

