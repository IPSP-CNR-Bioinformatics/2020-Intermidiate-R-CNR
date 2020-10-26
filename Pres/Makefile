all : P0

P0 : day-1-tidy.Rmd
	Rscript -e "library(rmarkdown); render('day-1-tidy.Rmd', 'xaringan::moon_reader')"

clean :
	rm Lesson*html; rm -r *_cache
