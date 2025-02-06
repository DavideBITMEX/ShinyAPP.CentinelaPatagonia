# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# attachment::att_amend_desc()

# sometimes re launch this, just to get the latest version
# Install the remotes package if you haven't already
# install.packages("remotes")
# Install tablerDash from GitHub
# remotes::install_github("RinteRface/tablerDash")

# Run the application
run_app()

