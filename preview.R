library(here)

i_am('preview.R')

# Preview in web browser
blogdown::serve_site()

# Shut down
blogdown::stop_server()
