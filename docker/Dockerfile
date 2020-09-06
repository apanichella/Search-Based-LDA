FROM r-base:3.6.2

## gnupg is needed to add new key
RUN apt-get update && apt-get install -y gnupg2

## install JDK
RUN apt-get -yqq update
RUN apt-get -yqq install openjdk-8-jdk

## add JAVA_HOME
RUN echo 'JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64' >> /etc/environment
RUN /bin/bash -c 'source /etc/environment'

## configure JAVA and install rJava package
RUN R CMD javareconf
RUN R -e "install.packages('rJava', dependencies = TRUE, repos='https://cran.rstudio.com/')"

## install Git
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git

## Clone the git repository
WORKDIR /home/
#RUN git clone https://github.com/apanichella/Search-Based-LDA.git
#WORKDIR /home/Search-Based-LDA
ADD . /home/SSBSE-LDA
RUN mkdir -p SSBSE-LDA
WORKDIR /home/SSBSE-LDA/

## install required R packages
RUN apt-get update && \
    apt-get install -y build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
RUN apt-get install -y gsl-bin libgsl0-dev
RUN apt-get install -y libglpk-dev
RUN apt-get install -y coinor-libsymphony-dev

#RUN apt-get update

RUN Rscript install.R



#RUN R -e "if (!require(devtools)){ install.packages(c('devtools','curl'), dependencies=F)  }"
#if (!require(XML)){ install.packages("XML", repos = "http://www.omegahat.net/R") }
#library(XML)
#RUN R -e "install.packages(c('devtools','curl'), dependencies=T)"
#RUN R -e "library('devtools'); devtools::install_github('matthewjdenny/SpeedReader', dependencies=T)"
#RUN R -e "if (!require(tm)){ install.packages('tm') }"
#RUN R -e "if (!require(jsonlite)){ install.packages('jsonlite')}"
#RUN R -e "if (!require(slam)){ install.packages('slam') }"
#RUN R -e "if (!require(igraph)){ install.packages('igraph') }"
#RUN R -e "if (!require(stringr)){ install.packages('stringr') }"
#RUN R -e "if (!require(cluster)){ install.packages('cluster') }"
#RUN R -e "if (!require(cmaes)){ install.packages('cmaes') }"
#RUN R -e "if (!require(snakecase)){ install.packages('snakecase') }"
#RUN R -e "if (!require(stopwords)){ install.packages('stopwords') }"
#RUN R -e "if (!require(NMOF)){ install.packages('NMOF') }"
#RUN R -e "if (!require(xtable)){ install.packages('xtable') }"
#RUN R -e "if (!require(Matrix)){ install.packages('Matrix') }"
#RUN R -e "if (!require(SpeedReader)){ devtools::install_github('matthewjdenny/SpeedReader') }"
#RUN R -e "if (!require(GA)){ install.packages('GA')  }"
#RUN R -e "if (!require(slam)){ install.packages('slam')  }"
#RUN R -e "if (!require(DEoptim)){ install.packages('DEoptim')  }"
#RUN R -e "if (!require(GenSA)){ install.packages('GenSA')  }"
#RUN R -e "if (!require(metaheuristicOpt)){ install.packages('metaheuristicOpt')  }"
#RUN R -e "if (!require(randomsearch)){ install.packages('randomsearch')  }"
#RUN R -e "if (!require(randomsearch)){ install.packages('randomsearch')  }"
#RUN R -e "if (!require(parma)){ install.packages('parma')  }"
#RUN R -e "if (!require(pracma)){ install.packages('pracma')  }"
#RUN R -e "if (!require(philentropy)){ install.packages('philentropy')  }"
#RUN R -e "if (!require(topicmodels)){ install.packages('topicmodels', dependencies=T)  }"
#RUN R -e "install.packages('textmineR')"
#RUN R -e "install.packages('parma', dependencies = T)"
#RUN R -e "install.packages('corpus', dependencies = T)"


## install gzip
RUN apt-get install -y gzip

## Let's unzip the datasets
WORKDIR /home/SSBSE-LDA/datasets
RUN unzip COLLECTIONS.zip
RUN unzip DATACMNS.zip
RUN unzip IO.zip
RUN unzip LANG.zip
RUN unzip MATH.zip
RUN unzip ROO.zip
RUN unzip SEC.zip
RUN unzip WFLY.zip
RUN unzip HIVE.zip
RUN unzip SPR.zip

WORKDIR /home/SSBSE-LDA
RUN mkdir -p Results
#ENTRYPOINT["/home/SSBSE-LDA/entrypoint.sh"]

CMD ["bin", "bash"]