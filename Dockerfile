FROM rocker/shiny-verse:latest

# copy the app to the image
# COPY *.Rproj /srv/shiny-server/
COPY *.R /srv/shiny-server/
# COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# install packages
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('padr', repos='http://cran.rstudio.com/')"



# run app
CMD ["/usr/bin/shiny-server.sh"]
