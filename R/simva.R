#' Simulate anaesthetic uptake
#'
#' @param pinsp `numeric(1)`, inspiratory partial pressure of the anaesthetic.
#' @param delta_time `numeric(1)`, time difference between each calculation step.
#' @param total_time `numeric(1)`, total time to simulate.
#' @param conductances `numeric(4)`, conductances.
#' @param capacitances `numeric(4)`, capacitances.
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
                                   conductances, capacitances) {
    n <- ceiling(total_time / delta_time)

    partial_pressures <- c(lung = 0, vrg = 0, mus = 0, fat = 0)
    results <- matrix(
        NA_real_, nrow = n, ncol = length(partial_pressures) + 2,
        dimnames = list(c(), c("time", names(partial_pressures), "cv"))
    )

    # humified
    # pamb <- 760 # mmHg
    # dph2o <- 47 # mmHg
    # pinsp <- pinsp * (pamb/ (pamb + dph2o))

    for (i in seq_len(n)) {
        dvdtpt <- (pinsp - partial_pressures["lung"]) * conductances["lung"]

        # if concentration effect
        # AMV = 4
        # conductances["lung"] <- AMV * stp_factor() + dvdtpt / 100
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
