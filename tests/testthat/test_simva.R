test_that("sim_anaesthetic_uptake works", {
    expect_error(
        sim_anaesthetic_uptake(ppart = 1:3), "length"
    )
    expect_error(
        sim_anaesthetic_uptake(
            ppart = 
                c(pinsp = 12, lung = 1, vrg = 1, mus = 1, fat = 1, foo = 1)
        ), "names"
    )
    expect_error(sim_anaesthetic_uptake(pinsp = 12, shunt_frac = 3), "0 and 1")
    expect_error(sim_anaesthetic_uptake(pinsp = 12, shunt_frac = -3), "0 and 1")

    # Test case with diethyl ether as in Cowles 1973, Table 4
    blood_flow <- cardiac_output()
    part_coefs <- partition_coefficients("diethyl-ether")

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
        c(time = 10, pinsp = 12, palv = 1.73, part = 1.73, 
          pvrg = 1.48, pmus = 0.28, pfat = 0.08, pcv = 1.23),
        tolerance = 5e-2
    )
    # Doubled alveolar ventilation, row 2 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances * c(2, 1, 1, 1),
            capacitances = capacitances
        )[100, ],
        c(time = 10, pinsp = 12, palv = 3.11, part = 3.11, 
          pvrg = 2.69, pmus = 0.51, pfat = 0.14, pcv = 2.23),
        tolerance = 5e-2
    )
    # Halved cardiac output, row 3 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances * c(1, 0.5, 0.5, 0.5),
            capacitances = capacitances
        )[100, ],
        c(time = 10, pinsp = 12, palv = 2.24, part = 2.24, 
          pvrg = 1.59, pmus = 0.20, pfat = 0.05, pcv = 1.30),
        tolerance = 5e-2
    )
    # Humified, row 4 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances,
            capacitances = capacitances,
            use_humidification = TRUE
        )[100, ],
        c(time = 10, pinsp = 12, palv = 1.63, part = 1.63, 
          pvrg = 1.39, pmus = 0.26, pfat = 0.08, pcv = 1.16),
        tolerance = 5e-2
    )
    # Concentration effect, row 5 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances,
            capacitances = capacitances,
            use_concentration_effect = TRUE
        )[100, ],
        c(time = 10, pinsp = 12, palv = 1.91, part = 1.91, 
          pvrg = 1.64, pmus = 0.31, pfat = 0.08, pcv = 1.36),
        tolerance = 5e-2
    )
    # Pulmonary shunt, row 7 in Table 4, Cowles 1973
    expect_equal(
        sim_anaesthetic_uptake(
            pinsp = 12, delta_time = 0.1, total_time = 10,
            conductances = conductances,
            capacitances = capacitances,
            shunt_frac = 0.1
        )[100, ],
        c(time = 10, pinsp = 12, palv = 1.78, part = 1.72, 
          pvrg = 1.47, pmus = 0.28, pfat = 0.08, pcv = 1.22),
        tolerance = 5e-2
    )
})
