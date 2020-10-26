all : pres site

site : index.Rmd
		Rscript -e "library(rmarkdown); render_site()"
		cp -r Pres docs

pres : Makefile
	    cd Pres && make all

