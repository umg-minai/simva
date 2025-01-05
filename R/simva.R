#' Simulate anaesthetic uptake
#'
#' @param pinsp `numeric(1)`, inspiratory partial pressure of the anaesthetic.
#' @param delta_time `numeric(1)`, time difference between each calculation step
#' in min.
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
#' @param shunt_frac `double(1)`, fraction of pulmonary shunt.
#' @param metabolism_frac `double(1)`, fraction of metabolism of the volatile
#' anaesthetic per hour (e.g. `metabolism_frac = 0.05` for 5 % per hour).
#' @param ppart `double(6)`, initial partial pressures settings to start with.
#' Useful to (re)start a simulation at a given anaesthetic state/time point.
#' @return `matrix`, with partial pressures for each simulation step.
#' @export
#'
#' @note
#' `metabolism_frac`: applying the values given in Cowles 1973 the output
#' slightly differs (at the second decimal place).
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
#' part_coefs <- partition_coefficients("diethyl-ether")
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
#' matplot(sim[, 1], sim[, -1], type = "l")
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
                                   shunt_frac = 0,
                                   metabolism_frac = 0,
                                   ppart =
                                       partial_pressures(pinsp = pinsp)
                                   ) {
    nms <- names(partial_pressures(0))

    n <- ceiling(total_time / delta_time)

    results <- matrix(
        NA_real_, nrow = n, ncol = length(nms) + 1L,
        dimnames = list(c(), c("time", nms))
    )

    if (length(ppart) != length(nms) ||
            any(names(ppart) != nms))
        stop(
            "'ppart' has to be of length ", length(nms),
            " and has to have the following names: ",
            paste0(nms, collapse = ", ")
        )

    if (isTRUE(use_humidification))
        ppart["pinsp"] <-
            ppart["pinsp"] * (pambient / (pambient + pwater))

    if (shunt_frac < 0 || shunt_frac > 1)
        stop("'shunt_frac' has to be between 0 and 1.")

    if (metabolism_frac < 0 || metabolism_frac > 1)
        stop("'metabolism_frac' has to be between 0 and 1.")

    ## determine metabolism_frac per time interval / metabolism_frac is give per
    ## hour and delta_time is in min
    metabolism_frac <- metabolism_frac / (60 / delta_time)

    for (i in seq_len(n)) {
        dvdtpt <- (ppart["pinsp"] - ppart["palv"]) *
            conductances["lung"]

        if (isTRUE(use_concentration_effect))
            conductances["lung"] <-
                alveolar_minute_ventilation * tp_factor + dvdtpt / 100

        ppart["part"] <-
            ppart["palv"] * (1 - shunt_frac) + ppart["pcv"] * shunt_frac

        dvdt <- c(
            dvdt1 = 0,
            dvdt2 = (ppart["part"] - ppart["pvrg"]) *
                conductances["vrg"],
            dvdt3 = (ppart["part"] - ppart["pmus"]) *
                conductances["mus"],
            dvdt4 = (ppart["part"] - ppart["pfat"]) *
                conductances["fat"]
        )

        dvdt["dvdt1"] <- dvdtpt - sum(dvdt)
        ppart[c("palv", "pvrg", "pmus", "pfat")] <-
            ppart[c("palv", "pvrg", "pmus", "pfat")] +
            dvdt * delta_time / capacitances

        ppart[c("pvrg", "pmus", "pfat")] <-
            ppart[c("pvrg", "pmus", "pfat")] *
            (1 - metabolism_frac)

        ppart["pcv"] <- sum(
            ppart[c("pvrg", "pmus", "pfat")] *
                conductances[c("vrg", "mus", "fat")]
        ) / sum(conductances[c("vrg", "mus", "fat")])

        results[i, ] <- c(i * delta_time, ppart)
    }
    results
}
