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
#' @export
#' @rdname capacitance
#'
#' @details
#' From Cowles et al.:
#' "KETY(14) has shown that most tissues are so finely perfused that they
#' can be considered to be in equilibrium with their venous blood.
#' Therefore the venous blood contained within a tissue compartment must be
#' considered to be a part of that tissue compartment.
#'
#' @references
#' `capacitance`: Eqn. 14 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
capacitance <- function(tissue_volume, tissue_coefficient,
                        blood_volume, blood_coefficient,
                        tp_factor = stp_factor()) {
    (tissue_volume * tissue_coefficient +
        blood_volume * blood_coefficient) * tp_factor
}

#' @rdname capacitance
#' @param air_volume `numeric`, air volume in l.
#' @export
#'
#' @references
#'
#' `lung_capacitance`: Eqn. 15 in
#' Cowles, A. L., Borgstedt, H. H., & Gillies, A. J. (1973).
#' A simplified digital method for predicting anesthetic uptake and distribution.
#' Computers in Biology and Medicine, 3(4), 385-395.
#' \doi{10.1016/0010-4825(73)90004-8}
lung_capacitance <- function(air_volume,
                             tissue_volume, tissue_coefficient,
                             blood_volume, blood_coefficient,
                             tp_factor = stp_factor()) {
    (air_volume +
        tissue_volume * tissue_coefficient +
        blood_volume * blood_coefficient) * tp_factor
}
