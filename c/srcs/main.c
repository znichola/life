/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   main.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:19:31 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 00:59:51 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "life.h"

static void	get_neighbours(char world[WIDTH][HEIGHT], char *neighbours, int x, int y);
static void	print_world(char world[WIDTH][HEIGHT]);

int	main(int ac, char **av)
{
	char	world[WIDTH][HEIGHT] = {0};

	memset(&world, 0, sizeof(world));

	if (ac != 1)
		return (1);
	(void)av;
	printf("hello world\n");

	world[0][0] = 0;
	world[1][0] = 0;
	world[2][0] = 0;
	world[0][1] = 0;
	world[1][1] = 0;
	world[2][1] = 0;
	world[0][2] = 0;
	world[1][2] = 0;
	world[2][2] = 0;


	char nei[8] = {0};

	get_neighbours(world, nei, 1, 1);

	for (int i = 0; i < 8; i++)
		printf("%d ", nei[i]);

	print_world(world);
	return (1);
}

static void	get_neighbours(char world[WIDTH][HEIGHT], char *neighbours, int x, int y)
{
	neighbours[0] = get_nw(world, x, y);
	neighbours[1] =  get_n(world, x, y);
	neighbours[2] = get_ne(world, x, y);
	neighbours[3] =  get_w(world, x, y);
	neighbours[4] =  get_e(world, x, y);
	neighbours[5] = get_sw(world, x, y);
	neighbours[6] =  get_s(world, x, y);
	neighbours[7] = get_se(world, x, y);
}

static void	print_world(char world[WIDTH][HEIGHT])
{
	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			printf("%c", world[x][y]);
		}
		printf("\n");
	}
}
