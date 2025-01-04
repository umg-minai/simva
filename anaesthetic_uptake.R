#' Standard temperature and pressure correction factor
#'
#' @param std_temperature `numeric`, standard temperature value in Kelvin,
#' default: 273.2 K (== 0 °C)
#' @param tissue_temperature `numeric`, tissue temperature value in Kelvin,
#' default: 310.2 (== 37 °C)
#' @param std_pressure `numeric`, standard_pressure in atm, default: 1 atm
#' (== 101.325 kPa)
#' @return `numeric`, correction factor for temperature/pressure condition
#' @examples
#' # BTPS (body temperature, pressure saturated (ignored))
#' stp_factor()
#' # STPD (standard temperature, pressure, dry)
#' stp_factor(tissue_temperature = 273.2)
stp_factor <- function(std_temperature = 273.2, tissue_temperature = 310.2,
                       std_pressure = 1) {
    std_temperature / (std_pressure * tissue_temperature)
}

#' Conductance
#'
#' Conductance is defined as ability of the fluid to deliver anaesthetic agents
#' across a partial pressure gradient.
#'
#' @param flow `numeric`, blood/air flow in l/min.
#' @param partition_coefficient `numeric`, blood:gas or tissue:gas
#' partition coefficient
#' @param tp_factor `numeric`, temperature/pressure factor
#' @return `numeric`, conductance of the tissue in l/(min*atm)
#'
#' @details
#' From Cowles et al.:
#' "In the case of blood perfusing a tissue,
#' the conductance is equal to the rate of blood flow
#' multiplied by the blood:gas partition coefficient and the factor, T0/P0Ti.
#' In the case of the alveolar gas ventilating the lungs,
#' the conductance is the rate of alveolar ventilation
#' multiplied by the factor, T0/P0Ti.
#' The gas:gas partition coefficient is equal to 1.0, by definition."
#
#' @references
#' Eqn. 6 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
conductance <- function(flow, partition_coefficient, tp_factor = stp_factor()) {
    unname(flow * partition_coefficient) * tp_factor
}

#' Capacitance
#'
#' Capacitance is defined as ability of a tissue to hold anaesthetic agents
#' ant any given partial pressure.
#'
#' @param tissue_volume `numeric`, tissue volume in l.
#' @param tissue_coefficient `numeric`, tissue:gas partition coefficient
#' @param blood_volume `numeric`, tissue volume in l.
#' @param blood_coefficient `numeric`, blood:gas partition coefficient
#' @param tp_factor `numeric`, temperature/pressure factor
#' @return `numeric`, conductance of the tissue in l/(min*atm)
#'
#' @details
#' From Cowles et al.:
#' "KETY(14) has shown that most tissues are so finely perfused that they
#' can be considered to be in equilibrium with their venous blood.
#' Therefore the venous blood contained within a tissue compartment must be
#' considered to be a part of that tissue compartment.
#'
#' @references
#' Eqn. 14 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
capacitance <- function(tissue_volume, tissue_coefficient,
                        blood_volume, blood_coefficient,
                        tp_factor = stp_factor()) {
    unname(
        tissue_volume * tissue_coefficient +
        blood_volume * blood_coefficient
    ) * tp_factor
}

#' Lung capacitance
#'
#' Capacitance is defined as ability of a tissue to hold anaesthetic agents
#' ant any given partial pressure.
#'
#' @param tissue_volume `numeric`, tissue volume in l.
#' @param tissue_coefficient `numeric`, tissue:gas partition coefficient
#' @param blood_volume `numeric`, tissue volume in l.
#' @param blood_coefficient `numeric`, blood:gas partition coefficient
#' @param tp_factor `numeric`, temperature/pressure factor
#' @return `numeric`, conductance of the tissue in l/(min*atm)
#'
#' @details
#' From Cowles et al.:
#' Alveolar air, lung tissue and arterial blood are also in equilibrium.
#'
#' @references
#' Eqn. 15 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
lung_capacitance <- function(air_volume,
                             tissue_volume, tissue_coefficient,
                             blood_volume, blood_coefficient,
                             tp_factor = stp_factor()) {
    unname(
        air_volume + tissue_volume * tissue_coefficient +
        blood_volume * blood_coefficient
    ) * tp_factor
}

#' Partition coefficients
#'
#' Partition coefficients for commonly used volatile anaesthetics.
#'
#' @param anaesthetic `character(1)`, anaesthetic
#' @return a named `numeric` with tissue partition coefficients for
#' *lung*, *vrg* (vessel rich group, visceral: brain, heart, kidney, ...),
#' *mus* (muscle, lean tissue: muscle, skin, subcutaneous tissue, ...),
#' and *fat* (fat, fatty tissue: yellow marrow, fat) for the given anaesthetic.
#'
#' @references
#' Table 2 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
#'
#' @examples
#' tissue_coefficients("nitrous oxide")
tissue_coefficients <- function(anaesthetic = c(
                                    "nitrous-oxide",
                                    "diethyl-ether"
                                )) {
    switch(
        match.arg(anaesthetic),
        "nitrous-oxide" = c(lung = 0.463, vrg = 0.463, mus = 0.463, fat = 1.03),
        "diethyl-ether" = c(lung = 12.1, vrg = 12.1, mus = 12.1, fat = 44.1)
    )
}

#' Cardiac output
#'
#' Partial blood flow of common compartments.
#'
#' @param total `numeric`, total cardiac output in l/min.
#' @param prop_lung `numeric`, proportion of cardiac output to the lung, should
#' be 1.00 in general.
#' @param prop_vrg `numeric`, proportion of cardiac output to the vessel rich
#' compartment.
#' @param prop_mus `numeric`, proportion of cardiac output to the musle
#' compartment.
#' @param prop_fat `numeric`, proportion of cardiac output to the fat
#' compartment.
#'
#' @return a named `numeric` with blood flow values (in l/min) for
#' *lung*, *vrg* (vessel rich group, visceral: brain, heart, kidney, ...),
#' *mus* (muscle, lean tissue: muscle, skin, subcutaneous tissue, ...),
#' and *fat* (fat, fatty tissue: yellow marrow, fat) for the given anaesthetic.
#'
#' @references
#' Table 3 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
cardiac_output <- function(total = 6.3,
                           prop_lung = 1.0, prop_vrg = 0.798, prop_mus = 0.157,
                           prop_fat = 0.044) {
    if (prop_vrg + prop_mus + prop_fat > 1)
        stop(
            "Sum of proportions of vrg, mus and fat ",
            "should not be larger than 1.0."
        )

    c(
        lung = prop_lung * total,
        vrg = prop_vrg * total, mus = prop_mus * total, fat = prop_fat * total
    )
}

#' Volumes
#'
#' @references
#' Table 3 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
tissue_volume <- c(
    lung_air = 2.68, lung_tissue = 1.0,
    vrg = 8.83, mus = 36.25, fat = 11.5
)
blood_volume <- c(lung = 1.4, vrg = 3.2, mus = 0.63, fat = 0.18)

#' Simulate anaesthetic uptake
#'
#' @param pinsp `numeric(1)`, inspiratory partial pressure of the anaesthetic.
#' @param delta_time `numeric(1)`, time difference between each calculation step.
#' @param total_time `numeric(1)`, total time to simulate.
#' @param conductances `numeric(4)`, conductances.
#' @param capacitances `numeric(4)`, capacitances.
#' @return `matrix`, with partial pressures for each simulation step.
#'
#' @references
#' Figure 1 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
sim_anaesthetic_uptake <- function(pinsp,
                                   delta_time = 0.1, total_time = 10,
                                   conductances, capacitances) {
    n <- ceiling(total_time / delta_time)

    partial_pressures <- c(lung = 0, vrg = 0, mus = 0, fat = 0)
    results <- matrix(
        NA_real_, nrow = n, ncol = length(partial_pressures) + 2,
        dimnames = list(c(), c("time", names(partial_pressures), "cv"))
    )

    for (i in seq_len(n)) {
        dvdtpt <- (pinsp - partial_pressures["lung"]) * conductances["lung"]
        dvdt <- c(
            dvdt1 = 0,
            dvdt2 = (partial_pressures["lung"] - partial_pressures["vrg"]) *
                conductances["vrg"],
            dvdt3 = (partial_pressures["lung"] - partial_pressures["mus"]) *
                conductances["mus"],
            dvdt4 = (partial_pressures["lung"] - partial_pressures["fat"]) *
                conductances["fat"]
        )
        dvdt["dvdt1"] <- dvdtpt - sum(dvdt)
        partial_pressures <-
            partial_pressures + dvdt * delta_time / capacitances
        pcv <- sum(
            partial_pressures[c("vrg", "mus", "fat")] *
                conductances[c("vrg", "mus", "fat")]
        ) / sum(conductances[c("vrg", "mus", "fat")])
        results[i,] <- c(i * delta_time, partial_pressures, pcv)
    }
    results
}



#' Test case with diethyl ether as in Cowles 1973, Table 4
#' set total to 6.3 * 0.5 to simulate reduced cardiac output (row 3 in Table 4)
blood_flow <- cardiac_output(total = 6.3)
part_coefs <- tissue_coefficients("diethyl-ether")

#' Conductances
conductances <- c(
    ## do we have to use the alveolar ventilation here? 4.0 l/min?
    ## and a gas:gas partition coefficient of 1.0?
    ## from Cowles 1973:
    ## In the case of blood perfusing a tissue, the conductance is equal to
    ## the rate of blood flow multiplied by the blood:gas partition coefficient
    ## and the factor, T0/P0Ti. In the case of the alveolar gas ventilating
    ## the lungs, the conductance is the rate of alveolar ventilation
    ## multiplied by the factor, T0/P0Ti.
    ## The gas:gas partition coefficient is equal to 1.0, by definition.
    lung = conductance(
        flow = 4.0,                     # alveolar minute ventilation
        partition_coefficient = 1.0     # gas:gas partition coefficient
    ),
    vrg = conductance(blood_flow["vrg"], part_coefs["lung"]),
    mus = conductance(blood_flow["mus"], part_coefs["lung"]),
    fat = conductance(blood_flow["fat"], part_coefs["lung"])
)

#' Capacitances
capacitances <- c(
    lung = lung_capacitance(
        tissue_volume["lung_air"],
        ## blood volume and tissue:gas == blood:gas in that case
        tissue_volume["lung_tissue"], tissue_coefficient = part_coefs["lung"],
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

sim <- sim_anaesthetic_uptake(
    pinsp = 12, delta_time = 0.1, total_time = 10,
    conductances = conductances, capacitances = capacitances
)

matplot(sim[, 1], sim[, -1])
