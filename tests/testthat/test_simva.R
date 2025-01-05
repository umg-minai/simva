test_that("stp_factor works", {
    expect_equal(stp_factor(), 273.2/310.2)
    expect_equal(stp_factor(tissue_temperature = 273.2), 1)
})

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

test_that("conductance works", {
    expect_equal(
        conductance(flow = 3, partition_coefficient = 2, tp_factor = 1), 
        3 * 2
    )
})

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

test_that("sim_anaesthetic_uptake works", {

    # Test case with diethyl ether as in Cowles 1973, Table 4
    blood_flow <- cardiac_output()
    part_coefs <- tissue_coefficients("diethyl-ether")
    
    tissue_volume <- c(
        lung_air = 2.68, lung_tissue = 1.0,
        vrg = 8.83, mus = 36.25, fat = 11.5 #, not_perfused = 7.02
    )
    blood_volume <- c(lung = 1.4, vrg = 3.2, mus = 0.63, fat = 0.18)
    
    # Conductances
    conductances <- c(
        ## do we have to use the alveolar ventilation here? 4.0 l/min?
        ## and a gas:gas partition coefficient of 1.0?
        ## from Cowles 1973:
        ## In the case of blood perfusing a tissue, the conductance is equal to
        ## the rate of blood flow multiplied by the blood:gas partition 
        ## coefficient and the factor, T0/P0Ti. In the case of the alveolar gas 
        ## ventilating the lungs, the conductance is the rate of alveolar 
        ## ventilation multiplied by the factor, T0/P0Ti.
        ## The gas:gas partition coefficient is equal to 1.0, by definition.
        lung = conductance(
            flow = 4.0,                     # alveolar minute ventilation
            partition_coefficient = 1.0     # gas:gas partition coefficient
        ),
        vrg = conductance(blood_flow["vrg"], part_coefs["lung"]),
        mus = conductance(blood_flow["mus"], part_coefs["lung"]),
        fat = conductance(blood_flow["fat"], part_coefs["lung"])
    )
    
    # Capacitances
    capacitances <- c(
        lung = lung_capacitance(
            tissue_volume["lung_air"],
            ## blood volume and tissue:gas == blood:gas in that gase part_coefs
            tissue_volume["lung_tissue"], 
            tissue_coefficient = part_coefs["lung"],
            ## blood volume and blood:gas part_coefs
            blood_volume["lung"], part_coefs["lung"]
        ),
        vrg = capacitance(
            tissue_volume["vrg"], part_coefs["vrg"],
            blood_volume["vrg"], part_coefs["lung"]
        ),
        mus = capacitance(
            tissue_volume["mus"], part_coefs["mus"],
            blood_volume["mus"], part_coefs["lung"]
        ),
        fat = capacitance(
            tissue_volume["fat"], part_coefs["fat"],
            blood_volume["fat"], part_coefs["lung"]
        )
    )
    
    # Normal, row 1 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances, 
            capacitances = capacitances
        )[100, ],
        c(time = 10, 
          lung = 1.73, vrg = 1.48, mus = 0.28, fat = 0.08, cv = 1.23),
        tolerance = 5e-2
    )
    # Doubled alveolar ventilation, row 2 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances * c(2, 1, 1, 1),
            capacitances = capacitances
        )[100, ],
        c(time = 10, 
          lung = 3.11, vrg = 2.69, mus = 0.51, fat = 0.14, cv = 2.23),
        tolerance = 5e-2
    )
    # Halved cardiac output, row 3 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances * c(1, 0.5, 0.5, 0.5),
            capacitances = capacitances
        )[100, ],
        c(time = 10, 
          lung = 2.24, vrg = 1.59, mus = 0.20, fat = 0.05, cv = 1.30),
        tolerance = 5e-2
    )
})
