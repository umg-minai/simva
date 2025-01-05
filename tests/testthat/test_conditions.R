test_that("stp_factor works", {
    expect_equal(stp_factor(), 273.2/310.2)
    expect_equal(stp_factor(tissue_temperature = 273.2), 1)
})
