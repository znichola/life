/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   main.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:19:31 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 02:40:33 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "life.h"

static void	get_neighbours(char world[WIDTH][HEIGHT], char *neighbours, int x, int y);
static void	print_world(char world[WIDTH][HEIGHT]);
static int	count_neighbours(char world[WIDTH][HEIGHT], int x, int y);
static void	set_world(char world[WIDTH][HEIGHT], char *input);
static int	get_square(char *input);
static void	remove_newlines(char *input);

int	main(int ac, char **av)
{
	char	world[WIDTH][HEIGHT] = {0};

	memset(&world, '.', sizeof(world));

	if (ac != 1)
		return (1);
	(void)av;
	printf("hello live world\n");

	(void)count_neighbours;
	(void)print_world;
	(void)get_square;

	char str[] = "\
	.#.\
	.#.\
	.#.\
	";

	set_world(world, str);
	print_world(world);
	// printf("\033[%dF", HEIGHT);
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
			printf("%c", world[x][y]);
		printf("\n");
	}
}

static int	count_neighbours(char world[WIDTH][HEIGHT], int x, int y)
{
	char nei[8] = {0};
	int	count = 0;

	get_neighbours(world, nei, x, y);
	for (int i = 0; i < 8; i++)
		if (nei[i] == L)
			count += 1;
	return (count);
}

static void	set_world(char world[WIDTH][HEIGHT], char *input)
{
	remove_newlines(input);

	int	d = get_square(input);

	int	sx =  WIDTH / 2 - d / 2 - 1;
	int	sy = HEIGHT / 2 - d / 2 - 1;

	for (int y = 0; y < d; y++)
		for (int x = 0; x < d; x++)
			world[sx + x][sy + y] = input[x + d * y];
}


// 0 1 2 3 4
// 5 6 7 8 9


static int	get_square(char *input)
{
	int	l = strlen(input);

	// printf("l:%d\n", (int)l);
	for (int i = 2; i < l; i ++)
		if (i * i == l)
			return (i);

	printf("not a square starting seed\n");
	exit(0);
	return (0);
}

static void	remove_newlines(char *input)
{
	char *ptr = input;

	while (*ptr)
	{
		if (isspace(*ptr))
		{
			size_t	len = strlen(ptr + 1);
			memmove(ptr, ptr + 1, len);
			ptr[len] = '\0';
		}
		ptr++;
	}
}
