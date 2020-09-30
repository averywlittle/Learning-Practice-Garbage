#include <stdio.h>
#include <cs50.h>
#include <string.h>
#include <assert.h>

long get_number(void);
string stringify(long cc);
bool check_sum(long cc);
void find_brand(long cc);

int main(void)
{

    long cc = get_number();
    string credit = stringify(cc);
    if (check_sum(cc) == false && strlen(credit) != 13 && strlen(credit) != 15 && strlen(credit) != 16)
    {
        printf("INVALID\n");
    }
    else
    {
        find_brand(cc);
    }
}

// Gets the long to be processed
long get_number(void)
{
    long cc = get_long("Number: ");
    return cc;
}

// Utility function to execute Lahn's formula
bool check_sum(long cc)
{
    string credit = stringify(cc);
    int length = strlen(credit);

    /*
    // Test length
    printf("Length: %d\n", length);
    */

    int digits[length];

    for (int i = length - 1; i >= 0; i--)
    {
        digits[i] = cc % 10;
        cc = cc / 10;
    }

    /*
    // Test the array
    printf("Digits: ");
    for (int i = 0; i < length; i++)
    {
        printf("%d, ", digits[i]);
    }
    printf("\n");
    */

    int sum1 = 0, sum2 = 0;
    for (int i = length - 2; i >= 0; i -= 2)
    {
        int digit = digits[i];
        digit *= 2;
        if (digit > 9)
        {
            digit -= 9;
        }
        sum1 += digit;
    }

    for (int i = length - 1; i >= 0; i -= 2)
    {
        sum2 += digits[i];
    }

    int sum3 = sum1 + sum2;

    if (sum3 % 10 == 0)
    {
        return true;
    }
    else
    {
        return false;
    }
}

// Function to find the brand based on parameters
void find_brand(long cc)
{
    string credit = stringify(cc);
    int length = strlen(credit);
    // printf("CC length2: %d\n", length);

    int a = credit[0];
    int b = credit[1];

    if ((length == 13 || length == 16) && a == 52)
    {
        printf("VISA\n");
    }

    else if (length == 15 && a == 51 && (b == 52 || b == 55))
    {
        printf("AMEX\n");
    }

    else if (length == 16 && a == 53 && (b == 49 || b == 50 || b == 51 || b == 52 || b == 53))
    {
        printf("MASTERCARD\n");
    }

    else
    {
        printf("INVALID\n");
    }
}

// Utility function to turn long into string
string stringify(long cc)
{

    // I was having issues with my stringify method using sprintf overflowing the given buffer, so I use this from stack overflow
    const int n = snprintf(NULL, 0, "%lu", cc);
    assert(n > 0);
    char buf[n+1];
    int c = snprintf(buf, n + 1, "%lu", cc);
    assert(buf[n] == '\0');
    assert(c == n);

    /*
    char length[100];
    sprintf(length, "%ld", cc);
    */

    string credit = buf;
    return credit;
}
