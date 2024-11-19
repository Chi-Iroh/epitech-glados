BIN	=	glados

all:
	stack build
	mv $(shell stack path --local-install-root)/bin/glados-exe $(BIN)

re: fclean all

clean:
	rm -rf .stack-work

fclean: clean
	rm -f $(BIN)

style:
	./lambdananas .
