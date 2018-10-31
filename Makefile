# Based on https://github.com/robjhyndman/forecast/blob/master/Makefile
# Makefile for generating R packages, modified.
# Assumes Makefile is in top folder of package

PKG_NAME="qcoder" # Bash code removed due to R CMD CHECK failure

all: install

check:
	Rscript -e "devtools::check(document=TRUE)"

build:
	Rscript -e "devtools::build()"

test:
	Rscript -e "devtools::test()"

install:
	Rscript -e "devtools::install()"

docs:
	Rscript -e "devtools::document()"

clean:
	-rm -f ../$(PKG_NAME)_*.tar.gz
	#-rm -r -f man/*.Rd
	#-rm -r -f NAMESPACE

