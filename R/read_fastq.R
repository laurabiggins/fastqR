gc_content <- function(seq){

  assertthat::assert_that(is.character(seq), msg = "sequence must be a character vector")
  seq <- toupper(seq)
  if(any(stringr::str_detect(seq, "[^GATC]"))) {
    warning("Non GATC characters found in sequence")
  }
  short_seq <- stringr::str_replace_all(seq,"[^GC]","")
  nchar(short_seq)/nchar(seq)
}

read_fastq <- function(filename){

  assertthat::assert_that(assertthat::see_if(file.exists(filename)))
  assertthat::assert_that(assertthat::has_extension(filename, "fq"))
  file_contents <- scan(filename,character())

  id <- file_contents[c(TRUE, FALSE, FALSE, FALSE)]
  assertthat::assert_that(!any(duplicated(id)), msg = "duplicated ids found")

  seq <- file_contents[c(FALSE, TRUE, FALSE, FALSE)]
  id2 <- file_contents[c(FALSE, FALSE, TRUE, FALSE)]
  qual <- file_contents[c(FALSE, FALSE, FALSE, TRUE)]

  assertthat::assert_that(!any(nchar(seq) != nchar(qual)),
                          msg = "found unequal length of sequence and qualities")

  tibble::tibble(id, seq, id2, qual)
}

