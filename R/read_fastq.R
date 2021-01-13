#' Calculate GC content of DNA sequence
#'
#' @param seq DNA sequence (character string)
#'
#' @return single value of the proportion of G and C bases in the DNA sequence
#' @export
#'
#' @examples
#'gc_content("ACGTTTTGACGCATACCGAGCGCC")
gc_content <- function(seq){

  assertthat::assert_that(base::is.character(seq), msg = "sequence must be a character vector")
  seq <- base::toupper(seq)
  if(base::any(stringr::str_detect(seq, "[^GATC]"))) {
    warning("Non GATC characters found in sequence")
  }
  short_seq <- stringr::str_replace_all(seq,"[^GC]","")
  base::nchar(short_seq)/base::nchar(seq)
}

#' Read fastq file
#'
#' @param filename fastq file to read in
#'
#' @return tibble with columns containing the id, sequence, id2 and quality score.
#' Each read is on a new row.
#' @export
#'
#' @examples
#' fq_file <- read_fastq(system.file("good.fq", package = "fastqR"))
read_fastq <- function(filename){

  assertthat::assert_that(assertthat::see_if(file.exists(filename)))
  assertthat::assert_that(assertthat::has_extension(filename, "fq"))
  file_contents <- base::scan(filename,character())

  id <- file_contents[c(TRUE, FALSE, FALSE, FALSE)]
  assertthat::assert_that(!base::any(duplicated(id)), msg = "duplicated ids found")

  seq <- file_contents[c(FALSE, TRUE, FALSE, FALSE)]
  id2 <- file_contents[c(FALSE, FALSE, TRUE, FALSE)]
  qual <- file_contents[c(FALSE, FALSE, FALSE, TRUE)]

  assertthat::assert_that(!base::any(base::nchar(seq) != nchar(qual)),
                          msg = "found unequal length of sequence and qualities")

  tibble::tibble(id, seq, id2, qual)
}

