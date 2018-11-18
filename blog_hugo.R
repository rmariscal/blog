# ******************************************************************************
# Setup a new blog using blogdown and hugo
# ------------------------------------------------------------------------------
# Last edited: Nov-2018
# Rodrigo Mariscal (rmariscalparedes@gmail.com)
# ******************************************************************************

## References ##
# https://tclavelle.github.io/blog/blogdown_github/
# https://themes.gohugo.io/
# https://amber.rbind.io/blog/2016/12/19/creatingsite/
# https://bookdown.org/yihui/blogdown/rstudio-ide.html
# https://dev.to/toyotarone/how-to-host-your-hugo-website-on-github-pages-4aa2
# https://rmarkdown.rstudio.com/rmarkdown_websites.htm
# https://pawelgrzybek.com/from-jekyll-to-hugo-from-github-pages-to-netlify/


## Load blogdown package and install Hugo ##
library(blogdown)
#install_hugo()


# new_site(dir = "~/LocalDocs/blog_beta",
#          theme = 'mtn/cocoa-eh-hugo-theme',
#          format = 'toml')


# 'yoshiharuyamashita/blackburn'

# Buld the site
blogdown::build_site()

# Use server to view the site
blogdown::serve_site()

# Add a new post
blogdown::new_post()

file.create('.nojekyll')
