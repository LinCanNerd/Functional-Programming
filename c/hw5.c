#include <stdio.h>
#include <stdlib.h>


void checkEdian(){
    int x = 1; 
    int *cp = &x; 
    if(*cp == 1) 
        printf("Little endian\n"); 
    else  
        printf("Big endian\n"); 

}



typedef struct {
    int value;
    int index;
} Item;

// Comparison function for sorting by value
int compare_by_value(const void *a, const void *b) {
    Item *item1 = (Item *)a;
    Item *item2 = (Item *)b;
    return (item1->value - item2->value);
}

// Comparison function for sorting by index
int compare_by_index(const void *a, const void *b) {
    Item *item1 = (Item *)a;
    Item *item2 = (Item *)b;
    return (item1->index - item2->index);
}

// Function to remove duplicates while preserving order
int remove_duplicates(int *arr, int n) {
    if (n == 0) return 0;

    Item *items = malloc(n * sizeof(Item));
    for (int i = 0; i < n; i++) {
        items[i].value = arr[i];
        items[i].index = i;
    }

    // Sort items by value
    qsort(items, n, sizeof(Item), compare_by_value);

    // Remove duplicates
    int j = 0;
    for (int i = 1; i < n; i++) {
        if (items[j].value != items[i].value) {
            items[++j] = items[i];
        }
    }
    int new_size = j + 1;

    // Sort the items back by index
    qsort(items, new_size, sizeof(Item), compare_by_index);

    // Copy back to the original array
    for (int i = 0; i < new_size; i++) {
        arr[i] = items[i].value;
    }

    free(items);
    return new_size;
}

int binomialCoefficient(int n, int k) {
    // Create a temporary array to store the intermediate results
    int *C = (int *)malloc((k + 1) * sizeof(int));
    if (C == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    // Initialize the array with zero
    for (int i = 0; i <= k; i++) {
        C[i] = 0;
    }

    // Base case: C(n, 0) is always 1
    C[0] = 1;

    // Calculate the binomial coefficient iteratively
    for (int i = 1; i <= n; i++) {
        for (int j = (i < k ? i : k); j > 0; j--) {
            C[j] = C[j] + C[j - 1];
        }
    }

    // Store the result before freeing the memory
    int result = C[k];
    free(C);

    return result;
}

typedef struct Node {
    int n, k;
    int value;
    struct Node *left;
    struct Node *right;
} Node;

typedef struct GraphNode {
    int n, k;
    int value;
    struct GraphNode **children;
    int child_count;
} GraphNode;

Node* createNode(int n, int k, int value) {
    Node *node = (Node *)malloc(sizeof(Node));
    node->n = n;
    node->k = k;
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

Node* buildRecursionTree(int n, int k) {
    if (k == 0 || k == n) {
        return createNode(n, k, 1);
    }
    Node *left = buildRecursionTree(n - 1, k - 1);
    Node *right = buildRecursionTree(n - 1, k);
    Node *node = createNode(n, k, left->value + right->value);
    node->left = left;
    node->right = right;
    return node;
}

//Soluzione naive
GraphNode* createGraphNode(int n, int k, int value) {
    GraphNode *node = (GraphNode *)malloc(sizeof(GraphNode));
    node->n = n;
    node->k = k;
    node->value = value;
    node->children = NULL;
    node->child_count = 0;
    return node;
}

GraphNode* addGraphNode(GraphNode **graph, int *graph_size, int n, int k, int value) {
    for (int i = 0; i < *graph_size; i++) {
        if (graph[i]->n == n && graph[i]->k == k) {
            return graph[i];
        }
    }
    GraphNode *node = createGraphNode(n, k, value);
    graph[(*graph_size)++] = node;
    return node;
}

//Soluzione più efficiente
GraphNode* buildDAG(int n, int k, GraphNode **graph, int *graph_size) {
    if (k == 0 || k == n) {
        return addGraphNode(graph, graph_size, n, k, 1);
    }
    GraphNode *left = buildDAG(n - 1, k - 1, graph, graph_size);
    GraphNode *right = buildDAG(n - 1, k, graph, graph_size);
    GraphNode *node = addGraphNode(graph, graph_size, n, k, left->value + right->value);
    node->children = (GraphNode **)realloc(node->children, sizeof(GraphNode *) * (node->child_count + 2));
    node->children[node->child_count++] = left;
    node->children[node->child_count++] = right;
    return node;
}


void printNode(Node *node) {
    printf("Node(n=%d, k=%d, value=%d)\n", node->n, node->k, node->value);
}

void printGraphNode(GraphNode *node) {
    printf("GraphNode(n=%d, k=%d, value=%d)\n", node->n, node->k, node->value);
}



//ESERCIZIO 4 CRIVELLA EULERO


typedef struct {
    int succ; 
    int prec; 
} Pair;


Pair* initializeVector(int n) {
    Pair* pairs = (Pair*)malloc((n + 1) * sizeof(Pair));
    for (int i = 0; i <= n; ++i) {
        pairs[i].succ = 1;
        pairs[i].prec = 1;
    }
    return pairs;
}


Pair* eulerSieve(int n) {
    Pair* pairs = initializeVector(n);
    pairs[0].succ = pairs[0].prec = 0; 
    pairs[1].succ = pairs[1].prec = 0; 

    for (int i = 2; i <= n; ++i) {
        if (pairs[i].succ != 0) { 
            for (int j = 2 * i; j <= n; j += i) {
                pairs[j].succ = 0; 
            }
        }
    }


    int prev = -1;
    for (int i = 0; i <= n; ++i) {
        if (pairs[i].succ != 0) {
            if (prev != -1) {
                pairs[prev].succ = i - prev;
                pairs[i].prec = i - prev;
            }
            prev = i;
        }
    }

    return pairs;
}


void printPrimes(Pair* pairs, int n) {
    for (int i = 2; i <= n; ++i) {
        if (pairs[i].succ != 0) {
            printf("%d ", i);
        }
    }
    printf("\n");
}


int main() {
    char str[100];
    sprintf(str, "%d", binomialCoefficient(40,30));
    printf("Binomial Coefficient C(40, 35) = %s\n", str);
    return 0;
}