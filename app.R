# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# this causes problems during development (it sees CentinelaPatagonia as a package)
# CentinelaPatagonia::run_app() # add parameters here (if any)

run_app()  # call directly without CentinelaPatagonia::

