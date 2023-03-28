# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2023/03/28 23:37:22 by znichola          #+#    #+#              #
#    Updated: 2023/03/29 00:04:19 by znichola         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #



NAME	= life

CC		= gcc
CFLAGS	= -Wall -Wextra
CFLAGS	+= -Werror

ifdef DEBUG
CFLAGS	+= -g3 -fsanitize=address
else
ifdef DEBUGL
CFLAGS += -g3
endif
endif

FILES	= main getters

OBJS_PATH = objs/
SRCS_PATH = srcs/
INCS_PATH = includes/

INCLUDES = -I$(INCS_PATH).
# INCLUDES = -I$(INCS_PATH). -Imlx -Ilibft/includes -Ignl
# FRAMEWORK = -framework OpenGL -framework AppKit

SRCS	= $(addprefix $(SRCS_PATH), $(addsuffix .c, $(FILES)))
OBJS	= $(addprefix $(OBJS_PATH), $(addsuffix .o, $(FILES)))

all	: $(NAME)

$(OBJS_PATH)%.o: $(SRCS_PATH)%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $(INCLUDES) -o $@ $<

$(NAME)	: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

clean	:
	$(RM) $(OBJS)

fclean	: clean
	$(RM) $(NAME)

re	: fclean all

leaks : re
	leaks -atExit -- ./$(NAME)

.PHONY: all clean fclean re
