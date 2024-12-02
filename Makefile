BIN	=	glados

all:
	stack build
	@cp $(shell stack path --local-install-root)/bin/glados-exe $(BIN)

re: fclean all


clean:
	rm -rf .stack-work

fclean: clean
	rm -f $(BIN)

style:
	./lambdananas .

tests: unit_test func_test

func_test:
#	${shell python3 test.py}

unit_test:
	${shell stack test}

.PHONY: all re clean fclean style tests func_test unit_test