#' Standard temperature and pressure correction factor
#'
#' @param std_temperature `numeric`, standard temperature value in Kelvin,
#' default: 273.2 K (== 0 °C)
#' @param tissue_temperature `numeric`, tissue temperature value in Kelvin,
#' default: 310.2 (== 37 °C)
#' @param std_pressure `numeric`, standard_pressure in atm, default: 1 atm
#' (== 101.325 kPa)
#' @return `numeric`, correction factor for temperature/pressure condition
#' @export
#' @examples
#' # BTPS (body temperature, pressure saturated (ignored))
#' stp_factor()
#' # STPD (standard temperature, pressure, dry)
#' stp_factor(tissue_temperature = 273.2)
stp_factor <- function(std_temperature = 273.2, tissue_temperature = 310.2,
                       std_pressure = 1) {
    std_temperature / (std_pressure * tissue_temperature)
}
