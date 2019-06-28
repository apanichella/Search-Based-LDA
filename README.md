# Search-Based-LDA
This repository contains a few script (written in R) to automatically configure Latent Dirichlet Allocation (LDA) using meta-heuristics in a unsupervised fashion. In the current version, the scripts are customized for the problem of identify duplicated bug reports.  

# Datasets
To run the tool you need to download the projects from the Bench4BL dataset and publicly available in GitHub: https://github.com/exatoa/Bench4BL

Download an Archive and unpack it like described in their readme. Then move the contained folder `data/<group>/<archive_name>/bugrepo` into the folder `datasets`.
You have to take an Archive which contains duplicate bug reports (array in `duplicates.json` is not empty), for example `SHL.tar`.

# Publication
A. Panichella, "A Systematic Comparison of Search Algorithms for Topic Modelling - A Study on Duplicate Bug Report Identification", The 11th Symposium on Search-Based Software Engineering (SSBSE), Tallinn Estonia, 31 Aug - 1 Sep, 2019.
