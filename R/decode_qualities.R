decode_qualities <- function(qualities, offset = 33){

  assertthat::assert_that(assertthat::is.scalar(offset), msg = "offset value must be a scalar")
  assertthat::assert_that(offset == 33 | offset == 64, msg = "offset value must be 33 or 64")
  values <- as.integer(charToRaw(qualities))
  phred <- values - offset
  assertthat::assert_that(sum(!phred > 0) == 0, msg = "negative phred scores found")
  phred
}
