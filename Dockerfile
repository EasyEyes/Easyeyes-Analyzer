# Use the official R Shiny image from the Rocker project
FROM rocker/shiny:latest

# Install system libraries required by your app
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

# Copy renv.lock and .Rprofile first for better Docker layer caching
COPY renv.lock renv.lock
COPY renv renv

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore R packages from renv.lock
RUN R -e "renv::consent(provided = TRUE); renv::restore()"

# Copy the rest of the application
COPY . .

# Create necessary directories and set permissions
RUN mkdir -p /var/log/shiny-server && \
    chown -R shiny:shiny /srv/shiny-server && \
    chown -R shiny:shiny /var/log/shiny-server

# Expose port 3838
EXPOSE 3838

# Switch to shiny user
USER shiny

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
