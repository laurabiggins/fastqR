test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("decoding", {
  seq <- "???#;ABAAAH"
  expect_length(decode_qualities(seq), base::nchar(seq))
})

test_that("gc_testing", {
  seq <- "ACGTTTTGACGCATACCGAGCGCC"
  expect_gt(gc_content(seq), 0.58)
  expect_error(gc_content(1234))
  expect_error(gc_content(TRUE))
})

test_that("fq_import", {
  file <- system.file("good.fq", package = "fastqR")
  expect_output(str(read_fastq(file)), "tibble")
})
