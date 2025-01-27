test_that("partition_coefficients works", {
    expect_error(partition_coefficients("foo"), "should be one of")
    # ensure return of equal values as shown in Cowles et al. 1973 Table 2
    expect_identical(
        partition_coefficients("nitrous-oxide"),
        c(lung = 0.463, vrg = 0.463, mus = 0.463, fat = 1.03)
    )
    expect_identical(
        partition_coefficients("diethyl-ether"),
        c(lung = 12.1, vrg = 12.1, mus = 12.1, fat = 44.1)
    )
    expect_identical(
        partition_coefficients("halothane"),
        c(lung = 2.3, vrg = 6, mus = 8, fat = 138)
    )
})

test_that("cardiac_output works", {
    expect_error(
        cardiac_output(prop_vrg = 0.8, prop_mus = 0.2, prop_fat = 0.1),
        "should not be larger"
    )
    # ensure return of equal values as shown in Cowles et al. 1973 Table 3
    expect_equal(
        cardiac_output(),
        c(lung = 6.3, vrg = 5.03, mus = 0.99, fat = 0.28),
        tolerance = 5e-2
    )
    expect_equal(
        cardiac_output(total = 6.3 * 1.5),
        c(lung = 6.3, vrg = 5.03, mus = 0.99, fat = 0.28) * 1.5,
        tolerance = 5e-2
    )
})

test_that("partial_pressures works", {
    expect_error(partial_pressures(), "pinsp")
    expect_identical(
        partial_pressures(pinsp = 12),
        c(pinsp = 12, palv = 0, part = 0, pvrg = 0, pmus = 0, pfat = 0, pcv = 0)
    )
})
