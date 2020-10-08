# SRCR, 2020 - Prolog database makefile

# ----------------------------------------------------------------------------
# Configuration section (change this variables to load different prolog files)

MAIN = main.pl

# Runs sicstus software
run:
	~/prolog/bin/sicstus -l $(MAIN)

# Copies processed data from the parser
update:
	cp ../../datasets/processed/paragensLigacoes.pl include/paragensLigacoes.pl
