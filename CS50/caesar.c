#include <cs50.h>
#include <stdio.h>
#include <string.h>

void cipher(string pt, int key);
char rotate(char base, int key);

int main(int argc, string argv[])
{
    int key;
    // Input int for key
    if (argc == 2)
    {
        for(int i = 0, n = strlen(argv[1]); i < n; i++)
        {
            if (48 <= argv[1][i] && argv[1][i] <= 57)
            {
                continue;
            }
            else
            {
                printf("Usage: ./caesar key\n");
                return(1);
            }
        }
        key = atoi(argv[1]);
    }
    else
    {
        printf("Usage: ./caesar key\n");
        return(1);
    }
    
    // Getstring for plaintext  
    string pt = get_string("plaintext: ");
    
    // Output cipher text
    cipher(pt, key);
}

void cipher(string pt, int key)
{
    printf("ciphertext: ");
    for(int i = 0, n = strlen(pt); i < n; i++)
    {
        // If upper, rotate and preserve case
        if (pt[i] >= 'A' && pt[i] <= 'Z')
        {
            printf("%c", rotate(pt[i], key));
        }
        // If lower, rotate and preserve case
        else if (pt[i] >= 'a' && pt[i] <= 'z')
        {
            printf("%c", rotate(pt[i], key));
        }
        // If neither, print
        else printf("%c", pt[i]);
    }
    printf("\n");
}

// Utility function to rotate our chars handling going past z by resetting the rotation at A
char rotate(char base, int key)
{
    while(key > 0)
    {
        if(base == 'z')
        {
            base = 'a';
            key--;
            continue;
        }
        if(base == 'Z')
        {
            base = 'A';
            key--;
            continue;
        }
        else
        {
            base++;
        }
        key--;
    }
    return base;
    
}
