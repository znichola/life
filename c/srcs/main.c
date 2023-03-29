/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   main.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: znichola <znichola@student.42lausanne.ch>  +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2023/03/28 23:19:31 by znichola          #+#    #+#             */
/*   Updated: 2023/03/29 14:34:44 by znichola         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "life.h"

static void	get_neighbours(char *world, char *neighbours, int x, int y);
static void	print_world(char *world);
static int	count_neighbours(char *world, int x, int y);
static void	set_world(char *world, char *input);
static int	get_square(char *input);
static void	remove_newlines(char *input);
static void	itter_cells(char *world);

int	main(int ac, char **av)
{
	char	world[WIDTH * HEIGHT] = {0};

	memset(&world, E, sizeof(world));

	if (ac != 1)
		return (1);
	(void)av;
	printf("hello live world\n");

	char str[] = "\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.......##.......####.....\
......##......##....##...\
.....##............##....\
....##..##.......##......\
...#########...##........\
.......##....###.........\
......##... ########.....\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
.........................\
	";

	// char	str[WIDTH * HEIGHT];
	// int len	= WIDTH * HEIGHT - 10;
	// srand(1337);
	// for (int i = 0; i < len; i++)
	// 	str[i] = rand() %2 ? L : E;

	set_world(world, str);

	while (1)
	{
		print_world(world);
		itter_cells(world);
		// printf("~~\n");
		printf("\033[%dF", HEIGHT);
		usleep(500000);
	}
	// printf("c:%d\n",count_neighbours(world, 0, 0));
	return (1);
}

static void	itter_cells(char *world)
{
	char new_world[WIDTH * HEIGHT] = {0};
	memset(&new_world, E, sizeof(new_world));

	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
		{
			int	n = count_neighbours(world, x, y);
			if (world[x + y * WIDTH] == L && (n == 2 || n == 3))
				new_world[x + y * WIDTH] = L;
			else if (world[x + y * WIDTH] == E && n == 3)
				new_world[x + y * WIDTH] = L;
		}
	}

	// int x = 4;
	// int y = 4;
	// printf("old world (%d, %d) %d %c\n", x, y, count_neighbours(world, x, y), world[x + y * WIDTH]);
	// printf("new world (%d, %d) %d %c\n", x, y, count_neighbours(new_world, x, y), new_world[x + y * WIDTH]);

	// printf("~\n");
	// y = 3;
	// printf("old world (%d, %d) %d %c\n", x, y, count_neighbours(world, x, y), world[x + y * WIDTH]);
	// printf("new world (%d, %d) %d %c\n", x, y, count_neighbours(new_world, x, y), new_world[x + y * WIDTH]);

	memmove(world, new_world, sizeof(new_world));
}

static void	get_neighbours(char *world, char *neighbours, int x, int y)
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

static void	print_world(char *world)
{
	for (int y = 0; y < HEIGHT; y++)
	{
		for (int x = 0; x < WIDTH; x++)
			printf("%c", world[x + y * WIDTH]);
		printf("\n");
	}
}

static int	count_neighbours(char *world, int x, int y)
{
	char nei[8] = {0};
	int	count = 0;

	get_neighbours(world, nei, x, y);
	for (int i = 0; i < 8; i++)
		if (nei[i] == L)
			count += 1;
	return (count);
}

static void	set_world(char *world, char *input)
{
	remove_newlines(input);

	int	d = get_square(input);

	int	sx =  WIDTH / 2 - d / 2 - 1;
	int	sy = HEIGHT / 2 - d / 2 - 1;

	for (int y = 0; y < d; y++)
		for (int x = 0; x < d; x++)
			world[sx + x + (sy + y) * WIDTH] = input[x + d * y];
}


// 0 1 2 3 4
// 5 6 7 8 9


static int	get_square(char *input)
{
	int	l = strlen(input);

	int i = 2;
	// printf("l:%d\n", (int)l);
	for (; i < l; i ++)
		if (i * i == l)
			return (i);
		else if (i * i > l)
			break;

	printf("not a square starting seed l:%d i:%d\n", l, i);
	return (i - 1);
	// exit(0);
	// return (0);
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
