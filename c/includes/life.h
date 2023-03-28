/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   life.h                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:36:54 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 00:59:01 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#ifndef LIFE_H
# define LIFE_H

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>


# define WIDTH 10
# define HEIGHT 10


# define X WIDTH - 1
# define Y HEIGHT - 1
# define L '#'

char	get_e(char world[WIDTH][HEIGHT], int x, int y);
char	get_w(char world[WIDTH][HEIGHT], int x, int y);
char	get_n(char world[WIDTH][HEIGHT], int x, int y);
char	get_s(char world[WIDTH][HEIGHT], int x, int y);
char	get_nw(char world[WIDTH][HEIGHT], int x, int y);
char	get_ne(char world[WIDTH][HEIGHT], int x, int y);
char	get_sw(char world[WIDTH][HEIGHT], int x, int y);
char	get_se(char world[WIDTH][HEIGHT], int x, int y);

#endif /* LIFE_H */
