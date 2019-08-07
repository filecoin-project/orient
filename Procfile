# Uses buildpacks for sbcl and graphviz.
#
# > heroku buildpacks:add --index 1 https://github.com/yangby/heroku-buildpack-sbcl.git
# > heroku buildpacks:add --index 2 https://github.com/weibeld/heroku-buildpack-graphviz.git
#
# Buildpack added. Next release on ubercalc will use:
#  1. https://github.com/yangby/heroku-buildpack-sbcl.git
#  2. https://github.com/weibeld/heroku-buildpack-graphviz.git

web: $HOME/ubercalc ubercalc web --port $PORT

