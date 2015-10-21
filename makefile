VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md

.PHONY: all install

install:
	R -e 'devtools::install_github("sherrillmix/$(NAME)")'

localInstall:
	R -e 'devtools::install()'

man: R/*.R 
	R -e 'devtools::document()'
	touch man


#inst/doc: vignettes/*.Rnw
	#R -e 'devtools::build_vignettes()'
	#touch inst/doc

README.md: README.Rmd R/*.R
	make localInstall
	R -e 'knitr::opts_chunk$$set(fig.path="README_files/");knitr::knit("README.Rmd")'
	
#inst/doc tests/testthat/tests.R
$(PACKAGEFILE): man R/*.R DESCRIPTION 
	R -e 'devtools::check();devtools::build()'
