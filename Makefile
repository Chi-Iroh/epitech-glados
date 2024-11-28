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

unit_test:
	${shell stack test}