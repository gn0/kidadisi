# 9/19/2023

document:
	Rscript -e "devtools::document()"

check: check-devtools check-as-cran

check-devtools:
	Rscript -e "devtools::check()"

check-as-cran: build
	$(eval pkg_version := \
		$(shell cat DESCRIPTION | grep '^Version:' | cut -d' ' -f2))
	R CMD check --as-cran "kidadisi_$(pkg_version).tar.gz"

build:
	R CMD build .

install:
	Rscript -e "devtools::install()"

deploy:
	Rscript -e "pkgdown::deploy_to_branch()"
