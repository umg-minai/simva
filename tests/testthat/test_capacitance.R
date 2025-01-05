test_that("capacitance works", {
    expect_equal(
        capacitance(
            tissue_volume = 5, tissue_coefficient = 4,
            blood_volume = 3, blood_coefficient = 2,
            tp_factor = 1
        ),
        5 * 4 + 3 * 2
    )
})

test_that("lung_capacitance works", {
    expect_equal(
        lung_capacitance(
            air_volume = 6,
            tissue_volume = 5, tissue_coefficient = 4,
            blood_volume = 3, blood_coefficient = 2,
            tp_factor = 1
        ),
        6 + 5 * 4 + 3 * 2
    )
})
