#' Partition coefficients
#'
#' Partition coefficients for commonly used volatile anaesthetics.
#'
#' @param anaesthetic `character(1)`, anaesthetic
#' @return a named `numeric` with partition coefficients for
#' *lung*, *vrg* (vessel rich group, visceral: brain, heart, kidney, ...),
#' *mus* (muscle, lean tissue: muscle, skin, subcutaneous tissue, ...),
#' and *fat* (fat, fatty tissue: yellow marrow, fat) for the given anaesthetic.
#' @export
#'
#' @references
#' Table 2 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
#'
#' @examples
#' partition_coefficients("nitrous-oxide")
partition_coefficients <- function(anaesthetic = c(
                                        "nitrous-oxide",
                                        "diethyl-ether",
                                        "halothane")) {
    switch(
        match.arg(anaesthetic),
        "nitrous-oxide" = c(lung = 0.463, vrg = 0.463, mus = 0.463, fat = 1.03),
        "diethyl-ether" = c(lung = 12.1, vrg = 12.1, mus = 12.1, fat = 44.1),
        "halothane" = c(lung = 2.3, vrg = 6, mus = 8, fat = 138)
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
#' @export
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

#' Partial Pressures
#'
#' Create initial vector of partial pressures for
#' [`simva::sim_anaesthetic_uptake()`].
#'
#' @param pinsp `double(1)`, inspired anaesthetic partial pressure in atm.
#' @param palv `double(1)`, alveolar pressure, partial pressure in the lung.
#' @param part `double(1)`, arterial partial pressure.
#' @param pvrg `double(1)`, partial pressure in the vessel rich group
#' compartment.
#' @param pmus `double(1)`, partial pressure in the lean/muscular
#' compartment.
#' @param pfat `double(1)`, partial pressure in the fat
#' compartment.
#' @param pcv `double(1)`, partial pressure in the central venous
#' compartment.
#' @return a named `numeric` with partial pressures suiteable as input for the
#' [`simva::sim_anaesthetic_uptake()`] function.
#' @export
#'
#' @examples
#' partial_pressures(pinsp = 12)
partial_pressures <- function(pinsp,
                              palv = 0, part = 0,
                              pvrg = 0, pmus = 0, pfat = 0, pcv = 0) {
    c(pinsp = pinsp, palv = palv, part = part, pvrg = pvrg, pmus = pmus,
      pfat = pfat, pcv = pcv)
}
