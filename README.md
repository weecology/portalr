
<!-- README.md is generated from README.Rmd. Please edit that file -->

# portalr

<!-- badges: start -->

[![R-CMD-check](https://github.com/weecology/portalr/workflows/R-CMD-check/badge.svg)](https://github.com/weecology/portalr/actions)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalr/main/LICENSE)
[![Coverage
status](https://codecov.io/gh/weecology/portalr/branch/main/graph/badge.svg)](https://codecov.io/github/weecology/portalr?branch=main)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/portalr)](https://CRAN.R-project.org/package=portalr)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1429290.svg)](https://doi.org/10.5281/zenodo.1429290)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.01098/status.svg)](https://doi.org/10.21105/joss.01098)
[![NSF-1929730](https://img.shields.io/badge/NSF-1929730-blue.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1929730)
<!-- badges: end -->

<img src="man/figures/portalr.png" alt="hexagon software logo, designed to look like a 3D cube, basic lettering in the top right blue-and-grey-striped side of the cube says portalr, a drawn all black rodent jumps from a blue portal on a grey background on the left of the cube, to an orange portal on a checkerboard background on the bottom." width="200px" align="right">

## Overview

The **portalr** package provides collection of basic functions to
summarize the Portal project data on rodents, plants, ants, and weather
at our long-term field site in the Chihuahuan Desert. The data begin in
1977 and are continuously updated today. There are functions to
summarize rodent abundance, biomass, or energy and by site, plot, or
treatment type. There are functions to summarize the weather data
collected from our automated weather stations and plant data that is
collected each summer and fall.

## Installation

You can install portalr from CRAN with:

    install.packages("portalr")

OR from github with:

    # install.packages("remotes")
    remotes::install_github("weecology/portalr")

## Examples

1.  Load all data tables from the [PortalData GitHub
    repo](https://github.com/weecology/portalData):

<!-- -->

    data_tables <- load_rodent_data("repo")

1.  Download and generate summaries of rodent abundance and biomass:

<!-- -->

    download_observations(".")

    rodent_data <- abundance(".") # default grouping is by sampling period

    rodent_biomass_by_plot <- biomass(".", level = "plot", type = "granivores", 
    shape = "flat", time = "date")

1.  Retrieve weather data:

<!-- -->

    weatherdata <- weather("Monthly", ".")

For more detailed info, checkout the vignettes associated with the
package:

    browseVignettes("portalr")

## More Information

#### [Portal Data Repo](https://github.com/weecology/PortalData)

The data repo contains useful details for issues with data collection,
and background on why we handle them the way we do. Of course, it also
contains the raw data, if you would like to create more complex data
summaries than what is provided here.

#### [The Portal Project](https://portal.weecology.org/)

Find a list of previous publications using the Portal data at our
website.

#### [The Portal Blog](https://portalproject.wordpress.com/)

Follow our blog to get the latest news on what is happening with our
project and at the site.

## Citation

To cite `portalr`, please refer to either:

-   [JOSS publication](https://doi.org/10.21105/joss.01098):

    Erica M. Christensen, Glenda M. Yenni, Hao Ye, Juniper L. Simonis,
    Ellen K. Bledsoe, Renata M. Diaz, Shawn D. Taylor, Ethan P. White,
    and S. K. Morgan Ernest. (2019). portalr: an R package for
    summarizing and using the Portal Project Data. Journal of Open
    Source Software, 4(33), 1098,
    <a href="https://doi.org/10.21105/joss.01098" class="uri">https://doi.org/10.21105/joss.01098</a>

-   or use the most recent release on
    [Zenodo](https://doi.org/10.5281/zenodo.1429290).

### Dataset Citation

To cite the Portal dataset, use:

    get_dataset_citation()
    #> 
    #> To cite the Portal Data in publications, use:
    #> 
    #>   S. K. Morgan Ernest, Glenda M. Yenni, Ginger Allington, Ellen K.
    #>   Bledsoe, Erica M. Christensen, Renata M. Diaz, Keith Geluso, Jacob R.
    #>   Goheen, Qinfeng Guo, Edward Heske, Douglas Kelt, Joan M. Meiners, Jim
    #>   Munger, Carla Restrepo, Douglas A. Samson, Michele R. Schutzenhofer,
    #>   Marian Skupski, Sarah R. Supp, Kate Thibault, Shawn Taylor, Ethan
    #>   White, Diane W. Davidson, James H. Brown, and Thomas J. Valone.
    #>   (2018). The Portal Project: a long-term study of a Chihuahuan desert
    #>   ecosystem. bioRxiv, https://doi.org/10.1101/332783
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Article{ernest2018portal,
    #>     title = {The Portal Project: a long-term study of a Chihuahuan desert ecosystem},
    #>     author = {S. K. Morgan Ernest and Glenda M. Yenni and Ginger Allington and Ellen K. Bledsoe and Erica M. Christensen and Renata M. Diaz and Keith Geluso and Jacob R. Goheen and Qinfeng Guo and Edward Heske and Douglas Kelt and Joan M. Meiners and Jim Munger and Carla Restrepo and Douglas A. Samson and Michele R. Schutzenhofer and Marian Skupski and Sarah R. Supp and Kate Thibault and Shawn Taylor and Ethan White and Diane W. Davidson and James H. Brown and Thomas J. Valone},
    #>     year = {2018},
    #>     journal = {bioRxiv},
    #>     doi = {10.1101/332783},
    #>   }
