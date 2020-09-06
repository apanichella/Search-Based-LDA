# Search-Based-LDA
This repository contains a few scripts (written in R) to automatically configure Latent Dirichlet Allocation (LDA) using meta-heuristics in an unsupervised fashion. In the current version, the hands are customized for the problem of identify duplicated bug reports.  

# Datasets
The projects used in the benchmark are available in the folder `datasets`. These projects come from the **Bench4BL** dataset and are publicly available in GitHub: `https://github.com/exatoa/Bench4BL`. 

Notice that projects available in the `datasets` folder are smaller than their original counterparts in **Bench4BL**. This is because we provide only the bug reports, the bugs, and the duplicate pairs (the oracle). If you also need to access the source code, please download the original projects from **Bench4BL**.

# Run with Docker
I warmly recommend using **Docker** as the scripts have been tested on a specific `R` version (v3.6.2). Besides, the `R` packages that I use require to fix some conflicts between dependencies and versions. 

To facilitate installing and running the script, please use the **Docker File** available in the `docker` folder. Follow these instructions:

1. Open the terminal

2. Build the docker image using the command:
	`docker build -t sbse-lda -f docker/Dockerfile .`
	
3. Run the docker container using the command:
`docker run -it -v <local-folder>:/home/SSBSE-LDA/Results sbse-lda:latest bash`.
In the command above, replace `<local-folder>` with the folder in your local machine to save the results. The option `-v` maps a host directory to a directory in docker container.

4. Once steps 1-2 are completed, you can run the `R` script inside the container using the commands:<br/>
4.1 Start `R` with the command: `R`<br/>
4.2 Run the main script inside `R`using the command: `source('MainScript.R')`<br/>
4.3 Follow the instructions on the computer screen <br/>
 
Notice that building the docker image will require some time (around 20+ minutes).

# Publication
A. Panichella, "A Systematic Comparison of Search Algorithms for Topic Modelling - A Study on Duplicate Bug Report Identification", The 11th Symposium on Search-Based Software Engineering (SSBSE), Tallinn Estonia, 31 Aug - 1 Sep, 2019.

A. Panichella, "A Systematic Comparison of Search-Based Approaches for LDA Hyperparameter Tuning", Information and Software Technology, Under Review 
