
# This makefile helps compiling and running the project
# without having to use an IDE to do so...

OUT_PRODUCTION = out/production/parser
SRC = src

all: clean compile
	@echo "\nProgram compiled, use rule 'make run' to run the parser."

compile:
	@echo "[Makefile] (Rule: $@) \n\t Compiling java packages..."
	@find $(SRC) -name "*.java" -print0 | xargs -0 javac -d $(OUT_PRODUCTION)

run:
	@clear
#@echo "[Makefile] (Rule: $@) \n\t Running parser...\n"
	@java -classpath $(OUT_PRODUCTION) parser.Main

clean:
	@clear
	@echo "[Makefile] (Rule: $@) \n\t Cleaning production files (if any)..."
	@rm -rf $(OUT_PRODUCTION)/*