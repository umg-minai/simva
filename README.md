# simva

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/umg-minai/simva/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/umg-minai/simva/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/umg-minai/simva/branch/main/graph/badge.svg)](https://app.codecov.io/gh/umg-minai/simva?branch=main)
<!-- badges: end -->

R Implementation of the simple three compartment model for volatile
anaesthetics described in
[Cowles et al. 1973](https://doi.org/10.1016/0010-4825(73)90004-8).

## Bootstrap

We assume you have `guix` installed to generate an isolated development
environment in the following way:

```bash
git clone https://github.com/umg-minai/simva.git
cd simva
echo "$(pwd)" >> ~/.config/guix/shell-authorized-directories
guix shell
```

If network access (e.g. checking CRAN etc.) is needed:

```bash
CURL_CA_BUNDLE="/etc/ssl/certs/ca-certificates.crt" guix shell --network
```
