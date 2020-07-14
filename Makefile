##
## EPITECH PROJECT, 2019
## name
## File description:
## Makefile
##

NAME    =	imageCompressor

all:    $(NAME)

$(NAME):
		stack build --allow-different-user && mv $(shell stack path --local-install-root)/bin/imageCompressor-exe ./
		mv imageCompressor-exe $(NAME)

clean:
		stack clean

fclean: clean
		rm $(NAME)

re:	fclean all

.PHONY: all clean fclean re


