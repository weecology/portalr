## A collection of basic functions to summarize the Portal data

[![Build Status](https://travis-ci.org/weecology/portalr.svg?branch=master)](https://travis-ci.org/weecology/portalr)
[![License](http://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/portalr/master/LICENSE)

The portalr package creates useful summaries of the Portal project data on rodents, plants, ants, and weather at our long-term field site in the Chihuahuan Desert. The data begin in 1977 and are continuously updated today. There are functions to summarize rodent abundance, biomass, or energy and by site, plot, or treatment type. There are functions to summarize the weather data collected from our automated weather stations and plant data that is collected each summer and fall.

## Installation

Install the `devtools` package and then run:

```
devtools::install_github("weecology/portalr")
```

## Usage

```
download_observations('.')

data = loadData("repo")

rodentdata = abundance()

rodentplotdata = abundance(".", level='plot', type="granivores", shape="flat", time="newmoon")

weatherdata = weather("Monthly",".")
```

## More Information

#### [Portal Data Repo](github.com/weecology/PortalData) 
The data repo contains useful details for issues with data collection, and background on why we handle them the way we do. Of course, it also contains the raw data, if you would like to create more complex data summaries than what is provided here.

#### [The Portal Project](portal.weecology.org/)
Find a list of previous publications using the Portal data at our website.

#### [The Portal Blog](portalproject.wordpress.com/)
Follow our blog to get the latest news on what is happening with our project and at the site.
