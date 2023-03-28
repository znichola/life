/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   getters.c                                          :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:58:23 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 00:49:40 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "life.h"

char	get_e(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (-1);
	if (x == X)
		return (world[0][y]);
	return (world[x + 1][y]);
}

char	get_w(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (-1);
	if (x == 0)
		return (world[X][y]);
	return (world[x - 1][y]);
}


char	get_n(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (-1);
	if (y == 0)
		return (world[x][Y]);
	return (world[x][y - 1]);
}

char	get_s(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (-1);
	if (y == Y)
		return (world[x][0]);
	return (world[x][y + 1]);
}

char	get_nw(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (-1);
	if (y == 0)
		y = Y;
	else
		y -= 1;
	if (x == 0)
		x = X;
	else
		x -= 1;
	return (world[x][y]);
}

char	get_ne(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (-1);
	if (y == 0)
		y = Y;
	else
		y -= 1;
	if (x == X)
		x = 0;
	else
		x += 1;
	return (world[x][y]);
}

char	get_sw(char world[WIDTH][HEIGHT], int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (-1);
	if (y == Y)
		y = 0;
	else
		y += 1;
	if (x == 0)
		x = X;
	else
		x -= 1;
	return (world[x][y]);
}

char	get_se(char world[WIDTH][HEIGHT], int x, int y)
{
	printf("got %d : %d, %d\n", world[x][y], x, y);

	if (x > X || y > Y || x < 0 || y < 0)
		return (-1);
	if (y == Y)
		y = 0;
	else
		y += 1;
	if (x == X)
		x = 0;
	else
		x += 1;
	return (world[x][y]);
}
