/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   getters.c                                          :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:58:23 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 03:15:00 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "life.h"

char	get_e(char *world, int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (E);
	if (x == X)
		return (world[0 + y * WIDTH]);
	return (world[x + 1 + y * WIDTH]);
}

char	get_w(char *world, int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (E);
	if (x == 0)
		return (world[X + y * WIDTH]);
	return (world[x - 1 + y * WIDTH]);
}


char	get_n(char *world, int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (E);
	if (y == 0)
		return (world[x + Y * WIDTH]);
	return (world[x+ (y - 1) * WIDTH]);
}

char	get_s(char *world, int x, int y)
{
	if (x >= X || y >= Y || x < 0 || y < 0)
		return (E);
	if (y == Y)
		return (world[x + 0 * WIDTH]);
	return (world[x + (y + 1) * WIDTH]);
}

char	get_nw(char *world, int x, int y)
{
	// printf("bef %d, %d\n", x, y);
	if (x > X || y > Y || x < 0 || y < 0)
		return (E);
	if (y == 0)
		y = Y;
	else
		y -= 1;
	if (x == 0)
		x = X;
	else
		x -= 1;
	// printf("aft %d, %d : %d\n", x, y, x + y * WIDTH);
	return (world[x + y * WIDTH]);
}

char	get_ne(char *world, int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (E);
	if (y == 0)
		y = Y;
	else
		y -= 1;
	if (x == X)
		x = 0;
	else
		x += 1;
	return (world[x + y * WIDTH]);
}

char	get_sw(char *world, int x, int y)
{
	if (x > X || y > Y || x < 0 || y < 0)
		return (E);
	if (y == Y)
		y = 0;
	else
		y += 1;
	if (x == 0)
		x = X;
	else
		x -= 1;
	return (world[x + y * WIDTH]);
}

char	get_se(char *world, int x, int y)
{
	// printf("got %c : %d, %d\n", world[x + y * WIDTH], x, y);

	if (x > X || y > Y || x < 0 || y < 0)
		return (E);
	if (y == Y)
		y = 0;
	else
		y += 1;
	if (x == X)
		x = 0;
	else
		x += 1;
	return (world[x + y * WIDTH]);
}
