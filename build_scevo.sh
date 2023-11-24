#!/bin/sh

## The build assumes docker and R are already installed and configured.
##
## Some stuff that might help set yourself up :
##
##
# sudo apt install r-base r-base-dev r-cran-littler
# 
# sudo R -e "install.packages('shiny')"
# 
# sudo apt install libxml2-dev libfreetype-dev libfreetype6-dev libfontconfig-dev 
# sudo apt install libharfbuzz-dev libfribidi-dev 
# sudo apt install libpng-dev libtiff5-dev libjpeg-dev 
# sudo apt install libgdal-dev gdal-bin libgit2-dev pandoc
# sudo apt install libcairo2-dev
# sudo apt install libudunits2-dev

export DEBUG=flase
export ALL=false
export RPAK=false
export SAVE=false
export BASE=true
export SVONLY=false
export DASHDIR=dashboard

ARGS=""
while [ $# -gt 0 ] ; do
  ARGS="$ARGS $1"
  case $1 in
    --debug)
      export DEBUG=true
      ;;
    --all)
      export ALL=true
      export SAVE=true
      export BASE=true
      export RPAK=true
      ;;
    --just-save)
      export SVONLY=true
      export SAVE=true
      ;;
    --package)
      export RPAK=true
      ;;
    --no-base)
      export BASE=false
      ;;
    --update)
      export DASHDIR=dashboard_update
      ;;
    *)
      ;;
  esac
  shift
done

START=`date`

if [ -f renv.lock ] ; then
  /bin/rm renv.lock
fi
ln -s ${DASHDIR}/www .
if [ -f www ] ; then
  /bin/rm www
fi
ln -s ${DASHDIR}/renv.lock .
# if we're not just saving the docker
if [ "$SVONLY" != "true" ] ; then
  if [ ! -f scevo_*.tar.gz ] || [ "$RPAK" = "true" ] ; then
    echo Building SCEVO R Package
    cd ${DASHDIR}

    R  -e "renv::restore()"
    if [ $? -ne 0 ] ; then
      echo "Need to fix renv::restore()"
      exit 1
    fi

    R  -e "devtools::build()"
    # options(golem.app.prod = FALSE)  # TRUE for production, FALSE for development
    R  -e "options(golem.app.prod = FALSE);golem::detach_all_attached();golem::document_and_reload()"

    cd ..
  fi

  if [ "$BASE" = "true" ] ; then
    echo Building SCEVO base docker image
    docker build -f Dockerfile_base --progress=plain -t scevo_base .
  fi
  echo Building SCEVO main docker image
  docker build -f Dockerfile --progress=plain -t scevo:latest .
fi

if [ "$SAVE" = "true" ] ; then
  echo Saving SCEVO docker images
  docker save scevo_base > scevo_base.tar
  docker save scevo > scevo.tar
fi

echo " Started at" $START
echo "Finished at" `date`

exit 0
