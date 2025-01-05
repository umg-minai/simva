#' Simulate anaesthetic uptake
#'
#' @param pinsp `numeric(1)`, inspiratory partial pressure of the anaesthetic.
#' @param delta_time `numeric(1)`, time difference between each calculation step.
#' @param total_time `numeric(1)`, total time to simulate.
#' @param conductances `numeric(4)`, conductances.
#' @param capacitances `numeric(4)`, capacitances.
#' @param use_humidification `logical(1)`, should humification take into account
#' (default: `FALSE`).
#' @param pambient `numeric(1)`, ambient pressure in kPa.
#' @param pwater `numeric(1)`, water pressure in kPa.
#' @param use_concentration_effect `logical(1)`, should concentration effect
#' take into account (default: `FALSE`).
#' @param tp_factor `numeric`, temperature/pressure factor.
#' @param alveolar_minute_ventilation `numeric(1)`, alveolar minute ventilation
#' in l/min.
#' @param partial_pressures `logical(1)`, initial partial pressures
#' settings to start with.
#' @return `matrix`, with partial pressures for each simulation step.
#' @export
#'
#' @references
#' Figure 1 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
#'
#' @examples
#' ## Test case with diethyl ether as in Cowles 1973, Table 4
#' blood_flow <- cardiac_output(total = 6.3)
#' part_coefs <- tissue_coefficients("diethyl-ether")
#'
#'
#' # Volumes as in Cowles, Table 3
#' tissue_volume <- c(
#'     lung_air = 2.68, lung_tissue = 1.0,
#'     vrg = 8.83, mus = 36.25, fat = 11.5
#' )
#' blood_volume <- c(lung = 1.4, vrg = 3.2, mus = 0.63, fat = 0.18)
#'
#' conductances <- c(
#'     lung = conductance(
#'         flow = 4.0,                     # alveolar minute ventilation
#'         partition_coefficient = 1.0     # gas:gas partition coefficient
#'     ),
#'     vrg = conductance(blood_flow["vrg"], part_coefs["lung"]),
#'     mus = conductance(blood_flow["mus"], part_coefs["lung"]),
#'     fat = conductance(blood_flow["fat"], part_coefs["lung"])
#' )
#' capacitances <- c(
#'     lung = lung_capacitance(
#'         tissue_volume["lung_air"],
#'         ## blood volume and tissue:gas == blood:gas in that case
#'         tissue_volume["lung_tissue"], tissue_coefficient = part_coefs["lung"],
#'         ## blood volume and blood:gas part_coefs
#'         blood_volume["lung"], part_coefs["lung"]
#'     ),
#'     vrg = capacitance(
#'         tissue_volume["vrg"], part_coefs["vrg"],
#'         blood_volume["vrg"], part_coefs["lung"]
#'     ),
#'     mus = capacitance(
#'         tissue_volume["mus"], part_coefs["mus"],
#'         blood_volume["mus"], part_coefs["lung"]
#'     ),
#'     fat = capacitance(
#'         tissue_volume["fat"], part_coefs["fat"],
#'         blood_volume["fat"], part_coefs["lung"]
#'     )
#' )
#'
#' sim <- sim_anaesthetic_uptake(
#'     pinsp = 12, delta_time = 10/60, total_time = 10,
#'     conductances = conductances, capacitances = capacitances
#' )
#'
#' matplot(sim[, 1], sim[, -1])
sim_anaesthetic_uptake <- function(pinsp,
                                   delta_time = 0.1, total_time = 10,
                                   conductances, capacitances,
                                   use_humidification = FALSE,
                                   pambient = 101.325,
                                   pwater = 6.26,
                                   use_concentration_effect = FALSE,
                                   tp_factor = stp_factor(),
                                   alveolar_minute_ventilation =
                                       conductances["lung"] / tp_factor,
                                   partial_pressures =
                                       c(pinsp = pinsp, lung = 0, vrg = 0,
                                         mus = 0, fat = 0, cv = 0)
                                   ) {
    nms <- c("pinsp", "lung", "vrg", "mus", "fat", "cv")

    n <- ceiling(total_time / delta_time)

    results <- matrix(
        NA_real_, nrow = n, ncol = 7L, dimnames = list(c(), c("time", nms))
    )

    if (length(partial_pressures) != length(nms) ||
            any(names(partial_pressures) != nms))
        stop(
            "'partial_pressures' has to be of length ", length(nms),
            " and has to have the following names: ",
            paste0(nms, collapse = ", ")
        )

    if (isTRUE(use_humidification))
        partial_pressures["pinsp"] <-
            partial_pressures["pinsp"] * (pambient / (pambient + pwater))

    for (i in seq_len(n)) {
        dvdtpt <- (partial_pressures["pinsp"] - partial_pressures["lung"]) *
            conductances["lung"]

        if (isTRUE(use_concentration_effect))
            conductances["lung"] <-
                alveolar_minute_ventilation * tp_factor + dvdtpt / 100

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
        partial_pressures[c("lung", "vrg", "mus", "fat")] <-
            partial_pressures[c("lung", "vrg", "mus", "fat")] +
            dvdt * delta_time / capacitances
        partial_pressures["cv"] <- sum(
            partial_pressures[c("vrg", "mus", "fat")] *
                conductances[c("vrg", "mus", "fat")]
        ) / sum(conductances[c("vrg", "mus", "fat")])
        results[i, ] <- c(i * delta_time, partial_pressures)
    }
    results
}
