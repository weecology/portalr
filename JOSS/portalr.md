---
title: 'portalr: an R package for summarizing and using the Portal Project Data'
tags:
  - Portal Project
  - 
authors:
  - name: Erica Christensen
    orcid: 0000-0002-5635-2502
    affiliation: 1
affiliations:
  - name: University of Florida, Wildlife Ecology and Conservation Department
    index: 1
date: 22 October 2018
bibliography: paper.bib
---

# Summary

Data sharing, the practice of making data widely available for use by others, benefits the ecological research community in ensuring study reproducibility and enabling many data sets to be combined to address broad-scale questions (e.g. meta-analyses). However, there are few standards or rules about how ecological data are collected and stored, and so it can be difficult and/or time-consuming to understand how to use unfamiliar data appropriately. ``portalr`` is an R package [@RCoreTeam2016] to facilitate working with the data collected from a long-term ecological study called The Portal Project. All data is freely available in a GitHub repository (https://github.com/weecology/PortalData) and archived on Zenodo. The purpose of ``portalr`` is to allow new users to quickly and easily gain access to the data and start to use it. The functions contained in this package were developed by the researchers most familiar with the data and are designed to reflect best use practices.

The Portal Project is a long-term ecological study site located in the Chihuahuan Desert, near the town of Portal, Arizona, USA. Data collection spans 1977 to the present, and includes data on small mammals, ants, plants, as well as weather data from an automated weather station. For a complete description of the data and data collection process see [@Ernest:2018]. These data have been used in a wide variety of ecological research areas, from competition studies [@Heske:1994] to ecological forecasting [@White:2018]. 

The ``portalr`` package contains functions to download the most up-to-date version of the data from GitHub and Zenodo, and to summarize or manipulate the data in several common ways. For example, plant abundance data can be obtained at the plot level or aggregated to site level; small mammal data can be obtained as time series of abundances, biomass, or metabolic energy; and records for "unknown" species or incomplete records (containing "NAs") can be stripped from the data if desired. See [CRAN address] for documentation. 

# Acknowledgements

# References