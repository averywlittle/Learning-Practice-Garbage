#include <cs50.h>
#include <stdio.h>
#include <string.h>

// Max number of candidates
#define MAX 9

// preferences[i][j] is number of voters who prefer i over j
int preferences[MAX][MAX];

// locked[i][j] means i is locked in over j
bool locked[MAX][MAX];

// Each pair has a winner, loser
typedef struct
{
    int winner;
    int loser;
}
pair;

// Array of candidates
string candidates[MAX];
pair pairs[MAX * (MAX - 1) / 2];

int pair_count;
int candidate_count;

// Function prototypes
bool vote(int rank, string name, int ranks[]);
void record_preferences(int ranks[]);
void add_pairs(void);
void sort_pairs(void);
void lock_pairs(void);
void print_winner(void);
bool check_cycle(int x, int y);

// Sort functions
void merge_sort(pair all[], int length);
void merge(pair left[], pair right[], pair all[], int n_left, int n_right);
double calc_pref(pair x);

int main(int argc, string argv[])
{
    // Check for invalid usage
    if (argc < 2)
    {
        printf("Usage: tideman [candidate ...]\n");
        return 1;
    }

    // Populate array of candidates
    candidate_count = argc - 1;
    if (candidate_count > MAX)
    {
        printf("Maximum number of candidates is %i\n", MAX);
        return 2;
    }
    for (int i = 0; i < candidate_count; i++)
    {
        candidates[i] = argv[i + 1];
    }

    // Clear graph of locked in pairs
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            locked[i][j] = false;
        }
    }

    pair_count = 0;
    int voter_count = get_int("Number of voters: ");

    // Query for votes
    for (int i = 0; i < voter_count; i++)
    {
        // ranks[i] is voter's ith preference
        int ranks[candidate_count];

        // Query for each rank
        for (int j = 0; j < candidate_count; j++)
        {
            string name = get_string("Rank %i: ", j + 1);

            if (!vote(j, name, ranks))
            {
                printf("Invalid vote.\n");
                return 3;
            }
        }

        record_preferences(ranks);

        printf("\n");
    }

    add_pairs();
    sort_pairs();
    lock_pairs();
    print_winner();
    return 0;
}

// Update ranks given a new vote
bool vote(int rank, string name, int ranks[])
{
    // If name is a match for the name of a candidate, update ranks array to indicate to show this voter has the candidate as their rank preference
    for (int i = 0; i < candidate_count; i++)
    {
        // Check for name match
        if (strcmp(candidates[i], name) == 0)
        {
            // If found, add rank where the rank variable is the rank and i indicates the index of the candidate
            ranks[rank] = i;
            return true;
        }
    }
    // If the candidate name fails to find a match, return false to indicate invalid vote
    return false;
}

// Update preferences given one voter's ranks
void record_preferences(int ranks[])
{
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = i + 1; j < candidate_count; j++)
        {
            // Increment preferences for i
            preferences[ranks[i]][ranks[j]]++;
        }
    }
}

// Record pairs of candidates where one is preferred over the other
void add_pairs(void)
{
    // Go through the preferences matrix and find the winning preferences
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            // Skip comparison of a candidate to itself
            if (i == j)
            {
                continue;
            }
            else
            {
                // Test preference
                if (preferences[i][j] > preferences[j][i])
                {
                    // Create pair
                    pair newPair = {i, j};
                    // Add pair
                    pairs[pair_count] = newPair;
                    pair_count++;
                }
            }
        }
    }
}

// Sort pairs in decreasing order by strength of victory
void sort_pairs(void)
{
    // Pairs is just a reference to the indices in preferences[][]. We call into pref[][] to compare strength of preferences
    merge_sort(pairs, pair_count);
}

// Lock pairs into the candidate graph in order, without creating cycles
void lock_pairs(void)
{
    // Move locked ("strongest") pairs to new array
    for (int i = 0; i < pair_count; i++)
    {
        pair new_pair = pairs[i];

        if (i < 2) // Impossible to be a cycle with less than 3 edges
        {
            locked[new_pair.winner][new_pair.loser] = true;
        }
        else if (check_cycle(new_pair.winner, new_pair.loser))
        {
            locked[new_pair.winner][new_pair.loser] = false;
        }
        else
        {
            locked[new_pair.winner][new_pair.loser] = true;
        }
    }
}

// Print the winner of the election
void print_winner(void)
{
    // Find the source of the graph by testing the rows for edges
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            // Continue and possibly print winner if false
            if (locked[j][i] == false)
            {
                // If the whole row contained 0s then col will be # of candidates - 1. Print this winner.
                if (j == candidate_count - 1)
                {
                    // Print the source of the locked graph
                    printf("%s\n", candidates[i]);
                }

                // Continue exploring row
                continue;
            }

            // Go to next row
            break;
        }
    }
}

void merge_sort(pair all[], int length)
{
    if (length < 2)
    {
        return;
    }

    int mid = length / 2;
    pair left[mid];
    pair right[length - mid];

    for (int i = 0; i < mid; i++)
    {
        left[i] = all[i];
    }
    for (int i = mid; i < length; i++)
    {
        right[i - mid] = all[i];
    }

    merge_sort(left, mid);
    merge_sort(right, length - mid);
    merge(left, right, all, mid, length - mid);
}

void merge(pair left[], pair right[], pair all[], int n_left, int n_right)
{
    int i = 0, j = 0, k = 0;

    while (i < n_left && j < n_right)
    {
        // Get preference strengths here, then use for comparison
        double pref_left = calc_pref(left[i]);
        double pref_right = calc_pref(right[j]);

        if (pref_left >= pref_right)
        {
            all[k] = left[i];
            i++;
        }
        else
        {
            all[k] = right[j];
            j++;
        }

        k++;
    }
}

// Take a pair's references to preferences and calculate the strength of the preference
double calc_pref(pair x)
{
    int winner = x.winner; // How many preferred the winner
    int loser = x.loser; // How many preferred the loser

    double strength = preferences[winner][loser] / preferences[loser][winner];
    return strength;
}

// Returns true if a can reach b.
bool check_cycle(int x, int y)
{
    if (locked[y][x] == true)
    {
        return true;
    }


    for (int i = 0; i < candidate_count; i++)
    {
        if(locked[i][x] == true)
        {
            return check_cycle(i, y);
        }
    }

    return false;
}


