# SRCR, 2020 - CSV to Prolog parser makefile

# ------------------------------------------------------------------------
# Configuration section (change this variables to load different datasets)

# Dataset sources
IN_DATASET_FILE = ../../datasets/raw/encoded/paragem_autocarros_oeiras_encoded.csv
# Dataset processed into a prolog file
OUT_DATASET_FILE = ../../datasets/processed/paragens.pl

# ------------------------------------------------------------------------

# Compiling variables
OUT_PRODUCTION = out/production/parser
SRC = src

all: clean compile
	@echo "\nInput dataset : "$(IN_DATASET_FILE)
	@echo "Ouput prolog  : "$(OUT_DATASET_FILE)
	@echo "\nProgram compiled, use rule 'make run' to run the parser.\n"

compile:
	@echo "[Makefile] (Rule: $@) \n\t Compiling java packages..."
	@find $(SRC) -name "*.java" -print0 | xargs -0 javac -d $(OUT_PRODUCTION)

run:
	@clear
	@java -classpath $(OUT_PRODUCTION) parser.Main $(IN_DATASET_FILE) $(OUT_DATASET_FILE)

clean:
	@clear
	@echo "[Makefile] (Rule: $@) \n\t Cleaning production files (if any)..."
	@rm -rf $(OUT_PRODUCTION)/*