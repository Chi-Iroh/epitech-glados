BIN	=	glados

all:
	stack build
	@cp $(shell stack path --local-install-root)/bin/glados-exe $(BIN)

re: fclean all


clean:
	rm -rf .stack-work

fclean: clean
	rm -f $(BIN)
	rm -f $(BIN).cabal

style:
	./lambdananas .

tests: unit_test func_test

func_test:
	./test.py

unit_test:
	stack test --coverage
	@rm coverage.html
	@ln -s $(shell stack path --local-install-root)/hpc/glados/glados-test/hpc_index.html coverage.html

.PHONY: all re clean fclean style tests func_test unit_test