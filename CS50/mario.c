#include <cs50.h>
#include <stdio.h>

int get_height(void);
void print_pyramids(int);
void print_spaces(int, int);
void print_hashes(int);

int main(void)
{
    int height = get_height();
    print_pyramids(height);
}

// Our method to get the height using a do-while loop
int get_height(void)
{
    int y = get_int("Height: ");
    while (y < 1 || y > 8)
    {
        y = get_height();
    }
    return y;
}

// Print out the hash pyramid
void print_pyramids(int y)
{
    for(int i = 1; i <= y; i++)
    {
        print_spaces(i, y);
        print_hashes(i);
        printf("  ");
        print_hashes(i);
        printf("\n");
    }
}

// Print the amount of spaces we need by first printing the max amount allowed by the inputted height, and decrementing the rest
void print_spaces(int n, int height)
{
    int m = height - n;
    while(m > 0)
    {
        printf(" ");
        m--;
    }
}

// Print the hashes based on height given
void print_hashes(int x)
{
    while(x)
    {
        printf("#");
        x--;
    }
}