test_that("tissue_coefficients works", {
    expect_error(tissue_coefficients("foo"), "should be one of")
    # ensure return of equal values as shown in Cowles et al. 1973 Table 2
    expect_identical(
        tissue_coefficients("nitrous-oxide"),
        c(lung = 0.463, vrg = 0.463, mus = 0.463, fat = 1.03)
    )
    expect_identical(
        tissue_coefficients("diethyl-ether"),
        c(lung = 12.1, vrg = 12.1, mus = 12.1, fat = 44.1)
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
