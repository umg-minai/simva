test_that("conductance works", {
    expect_equal(
        conductance(flow = 3, partition_coefficient = 2, tp_factor = 1),
        3 * 2
    )
})
