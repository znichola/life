/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   life.h                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:36:54 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 13:07:16 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#ifndef LIFE_H
# define LIFE_H

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>
# include <ctype.h>
# include <unistd.h>

# define WIDTH 60
# define HEIGHT 60


# define X WIDTH - 1
# define Y HEIGHT - 1

# define L '#'
# define E '.'

char	get_e(char *world, int x, int y);
char	get_w(char *world, int x, int y);
char	get_n(char *world, int x, int y);
char	get_s(char *world, int x, int y);
char	get_nw(char *world, int x, int y);
char	get_ne(char *world, int x, int y);
char	get_sw(char *world, int x, int y);
char	get_se(char *world, int x, int y);

#endif /* LIFE_H */
