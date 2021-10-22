# TODO
#  - Think more tests
#  - Better output?
#


library(testthat)
data("ToothGrowth")


# Parameter configurations and expected output
# |-------------------|----------------------|----------------------|------------------|
# |       x           |           ...        |          by          |      ERROR       |
# |-------------------|----------------------|----------------------|------------------|
# |     NULL          |          NULL        |          NULL        |   MISSING_X
# |-------------------|----------------------|----------------------|------------------|
# |  NON num vector   |          NULL        |          NULL        |   MEAN_NOT_NUMERIC
# |-------------------|----------------------|----------------------|------------------|
# |  numeric vector   |          NULL        |          NULL        |       ---
# |  numeric vector   |          NULL        | vector diff length   |  DIFF_LEN_VECTOR
# |  numeric vector   |          NULL        |          vector      |       ---
# |  numeric vector   |  NOT_NUMERIC_VECTOR  |          NULL        |       ---   <- non conforming data is rejected
# |  numeric vector   |   numeric vector     |          NULL        |       ---   <- conforming data is included in the analisis
# |  numeric vector   |   mixed vectors      |          NULL        |       ---   <- conforming data is included in the analisis
# |-------------------|----------------------|----------------------|------------------|
# |      DF           |          NULL        |          NULL        |       ---   <- will analyze all numeric variables (2 in this case)
# |      DF           |          NULL        |          vector      |       ---   <- will analyze all numeric variables (2 in this case) by the vector provided
# |      DF           | extra_numeric_vector |          NULL        |       ---   <- added to the data.frame
# |      DF           |      var.name        |          NULL        |       ---   <- data.frame limited to variables specified
# |      DF           |      var.names(same) |          NULL        |       ---   <- if the variable names repeat numbers will be added to avoid collision
# |      DF           |  var.name/s + vector |          NULL        |       ---   <- data.frame will be formed with selected columns +  vectors included
# |      DF           |  duplicated vectors  |          NULL        |      Error: `data` must be uniquely named but has duplicate columns
# |      DF           |          NULL        |          DF          |      BY_IS_DATA_FRAME
# |-------------------|----------------------|----------------------|------------------|
# |      n=2          |          NULL        |          DF          |      BY_IS_DATA_FRAME
#
#
# There are errors that must




context("[media] x empty")
test_that("[media] x empty",
  {
    expect_error(feR::media(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
  }
)


context("[media] x non-numeric vector")
test_that("[media] x non-numeric vector",
  {
    expect_error(feR::media(x=as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("MEAN_NOT_NUMERIC", lang="en"))
  }
)


context("[media] x numeric vector")
test_that("[media] x numeric vector",
  {
    expect_equal(feR::media(ToothGrowth$len, stop.on.error = T, lang = "en")$mean,mean(ToothGrowth$len))
    expect_error(feR::media(ToothGrowth$len, by = "ONLY_ONE_ITEM", stop.on.error = T, lang = "en"),
                 feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
    expect_equal(feR::media(ToothGrowth$len,ToothGrowth$len, by=ToothGrowth$supp , stop.on.error = T, lang = "en")$mean[1]
           ,mean(ToothGrowth$len[ToothGrowth$supp == "OJ"]))
    expect_equal(feR::media(ToothGrowth$len,as.character(ToothGrowth$len), stop.on.error = T, lang = "en")$mean,
           mean(ToothGrowth$len))
    expect_equal(feR::media(ToothGrowth$len,ToothGrowth$len, stop.on.error = T, lang = "en")$mean,
                    c(mean(ToothGrowth$len),
                      mean(ToothGrowth$len)))
    expect_equal(feR::media(ToothGrowth$len, ToothGrowth$len, as.character(ToothGrowth$len), stop.on.error = T, lang = "en")$mean,
                 c(mean(ToothGrowth$len),
                   mean(ToothGrowth$len))
                 )
  }
)

context("[media] x data.frame")
test_that("[media] x data.frames",
  {
    expect_equal(feR::media(ToothGrowth, stop.on.error = T, lang = "en")$mean,
                      c(mean(ToothGrowth$len),
                        mean(ToothGrowth$dose))
           )

    expect_equal(feR::media(ToothGrowth, by = ToothGrowth$supp, stop.on.error = T, lang = "en")$mean,
                     c(mean(ToothGrowth$len[ToothGrowth$supp=="OJ"]),
                       mean(ToothGrowth$len[ToothGrowth$supp=="VC"]),
                       mean(ToothGrowth$dose[ToothGrowth$supp=="OJ"]),
                       mean(ToothGrowth$dose[ToothGrowth$supp=="VC"]))
           )

    expect_equal(feR::media(ToothGrowth, ToothGrowth$len + 10, stop.on.error = T, lang = "en")$mean,
                      c(mean(ToothGrowth$len),
                        mean(ToothGrowth$dose),
                        mean(ToothGrowth$len+10)))

    expect_equal(feR::media(ToothGrowth, "len",stop.on.error = T, lang = "en")$mean, mean(ToothGrowth$len))
    expect_equal(feR::media(ToothGrowth, "len", "len",stop.on.error = T, lang = "en")$mean, c(mean(ToothGrowth$len),mean(ToothGrowth$len)))
    expect_equal(feR::media(ToothGrowth, "len", ToothGrowth$dose,stop.on.error = T, lang = "en")$mean,
                 c(mean(ToothGrowth$len),mean(ToothGrowth$dose)))
    expect_error(feR::media(ToothGrowth, ToothGrowth$dose, ToothGrowth$dose,stop.on.error = T, lang = "en"),
                 "`data` must be uniquely named but has duplicate columns")

    expect_error(feR::media(ToothGrowth, by=data.frame(s=ToothGrowth$supp,c=ToothGrowth$supp), stop.on.error = T, lang = "en"),
                 feR:::.error.msg("BY_IS_DATA_FRAME", lang="en"))
  }
)

