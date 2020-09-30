#include <stdio.h>
#include <cs50.h>
#include <string.h>
#include <math.h>

int count_letters(string input);
int count_words(string input);
int count_sentences(string input);
int calc_index(int letter_count, int word_count, int sentence_count);


int main(void)
{
    string input = get_string("Text: ");

    int letter_count = count_letters(input);
    int word_count = count_words(input);
    int sentence_count = count_sentences(input);

    printf("%i letter(s)\n", letter_count);
    printf("%i word(s)\n", word_count);
    printf("%i sentence(s)\n", sentence_count);

    int index = calc_index(letter_count, word_count, sentence_count);

    if (index >= 16)
    {
        printf("Grade 16+\n");
    }
    else if (index < 1)
    {
        printf("Before Grade 1\n");
    }
    else
    {
        printf("Grade %i\n", index);
    }

    return (0);
}

int count_letters(string input)
{
    int ctr = 0;
    for (int i = 0; i < strlen(input); i++)
    {
        if ((input[i] > 64 && input[i] < 91) || (input[i] > 96 && input[i] < 123))
        {
            ctr++;
        }
    }

    return ctr;
}

int count_words(string input)
{
    int ctr = 0;
    for (int i = 0; i < strlen(input); i++)
    {
        if (input[i] == 32)
        {
            ctr++;
        }
    }

    // We return + 1 here because by nature a space will involve two words.
    // Therefore, by counting all spaces once, we will miss one word that depends on a space.
    return ctr + 1;
}

int count_sentences(string input)
{
    int ctr = 0;
    for (int i = 0; i < strlen(input); i++)
    {
        if (input[i] == 46 || input[i] == 33 || input[i] == 63)
        {
            ctr++;
        }
    }

    return ctr;
}

int calc_index(int letter_count, int word_count, int sentence_count)
{
    float hundred_word_blocks = (float)word_count / 100;
    float L = (float)letter_count / hundred_word_blocks;
    float S = (float)sentence_count / hundred_word_blocks;
    int index = round(0.0588 * L - 0.296 * S - 15.8);
    return index;
}