# SRCR, 2020 - CSV to Prolog parser makefile

# ------------------------------------------------------------------------
# Configuration section (change this variables to load different datasets)

# Dataset sources
IN_DATASETS_FOLDER = ../../datasets/raw/encoded/
# Dataset processed into a prolog file
OUT_PROLOG_FILE = ../../datasets/processed/paragensLigacoes.pl

# ------------------------------------------------------------------------

# Compiling variables
OUT_PRODUCTION = out/production/parser
SRC = src

all: clean compile
	@echo "\nInput datasets = "$(IN_DATASETS_FOLDER)
	@echo "Ouput prolog   = "$(OUT_PROLOG_FILE)
	@echo "\nProgram compiled, use rule 'make run' to run the parser.\n"

compile:
	@echo "[Makefile] (Rule: $@) \n\t Compiling java packages..."
	@find $(SRC) -name "*.java" -print0 | xargs -0 javac -d $(OUT_PRODUCTION)

run:
	@clear
	@java -classpath $(OUT_PRODUCTION) parser.Main $(IN_DATASETS_FOLDER) $(OUT_PROLOG_FILE)

clean:
	@clear
	@echo "[Makefile] (Rule: $@) \n\t Cleaning production files (if any)..."
	@rm -rf $(OUT_PRODUCTION)/*