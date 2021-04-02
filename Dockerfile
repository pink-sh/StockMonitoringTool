FROM rocker/r-ver:3.6.3

MAINTAINER Enrico Anello "enrico.anello@fao.org"


# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    #libssl1.0.0 \
    libjpeg-dev \
    default-jre \
    default-jdk \
    libxml2 \
    libxml2-dev \
    git \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-formats-extra \
    #libv8-3.14.5 \
    libv8-dev


RUN apt-get update && apt-get upgrade -y

#Install XML package from archive
#Issue is that XML package from 2020-07 is referenced as depending on R >= 4.0
#To temporarily solve that we use the previous XML package version from archive
#RUN wget https://cran.r-project.org/src/contrib/Archive/XML/XML_3.99-0.3.tar.gz

#RUN R -e "install.packages('XML_3.99-0.3.tar.gz', repos = NULL, type = 'source')"
# install dependencies of the Stock monitoring tool app
#RUN R -e "install.packages(c('shiny', 'rmarkdown','shinyjs', 'shinythemes', 'shinydashboard', 'RCurl', 'devtools', 'ggplot2', 'rfishbase', 'shinyBS', 'lubridate', #'waiter', 'pracma', 'googleVis', 'stringr','R.utils'), repos='https://cloud.r-project.org/')"

RUN R -e "install.packages(c('devtools'), repos='https://cran.r-project.org/')"
RUN R -e "devtools::install_version('XML', version='3.99-0.3', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shiny', version='1.5.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('rmarkdown', version='2.3', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinyjs', version='1.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinythemes', version='1.1.2', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinydashboard', version='0.7.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinyWidgets', version='0.5.3', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('RCurl', version='1.98.1.2', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('ggplot2', version='3.3.2', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('rfishbase', version='3.0.4', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('shinyBS', version='0.61', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('lubridate', version='1.7.9', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('waiter', version='0.1.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('pracma', version='2.2.9', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('googleVis', version='0.6.5', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('stringr', version='1.4.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('R.utils', version='2.9.2', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('fishmethods', version='1.11.1', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('V8', version='3.2.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('DT', version='0.14', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('futile.logger', version='1.4.3', repos = 'http://cran.r-project.org')"
#RUN R -e "devtools::install_version('TropFishR', version='1.6.2', repos = 'http://cran.r-project.org')"
RUN R -e "install.packages(c('TropFishR'), repos='https://cran.r-project.org/')"
RUN R -e "install.packages(c('nloptr'), repos='https://cran.r-project.org/')"
RUN R -e "devtools::install_github('AnalytixWare/ShinySky')"
#RUN R -e "devtools::install_github('jyypma/nloptr')"

#RUN R -e "install.packages(c('fishmethods'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages(c('V8'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages(c('DT'), repos='https://cloud.r-project.org/')"
#RUN R -e "install.packages('futile.logger', repos='https://cloud.r-project.org/')"

#Development
RUN git -C /root/ clone https://github.com/abennici/StockMonitoringTool.git && echo "OK!"
#Deployment
#RUN git -C /root/ clone https://github.com/pink-sh/StockMonitoringTool.git && echo "OK!"
RUN mkdir -p /srv/shiny/
RUN ln -s /root/StockMonitoringTool /srv/shiny/stockMonitoringTools
 
EXPOSE 3838

ENV SMT_LOG=session.log

RUN apt-get install -y curl
#Development
CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools',port=3838,host='0.0.0.0')"]
#Deployment
#CMD ["R", "-e shiny::runApp('/srv/shiny/stockMonitoringTools')"]
