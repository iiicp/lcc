//#define NAMESIZE 31
//
//#define DOUBLE(x) (2 * (x))
//#include <stdio.h>
//#include "string.h"
char c = '\xf8';

typedef struct TreeNode
{
    char vorname[62]; //DOUBLE(NAMESIZE)
    char nachname[62]; //DOUBLE(NAMESIZE)
    char anrede[5];
    float umsatz;
    char wohnort[62]; //DOUBLE(NAMESIZE)
    int bankleitzahl;
    unsigned char height;
    struct TreeNode* right;
    struct TreeNode* left;

} TreeNode;

TreeNode* rotateLeft(TreeNode* pivot, TreeNode* parent);

TreeNode* rotateRight(TreeNode* pivot, TreeNode* parent);

int getBalance(TreeNode* h);

unsigned char height(TreeNode* h);

void recursiveAdd(TreeNode* h, TreeNode* p, TreeNode* parent, TreeNode** root);

void addNode(TreeNode* root, TreeNode* p);

void Rotate(TreeNode* h, TreeNode** parent, TreeNode** root);

void updateHeight(TreeNode* h);

int strcmp(const char* lhs, const char* rhs)
{
    int i = 0;
    while (lhs[i] != '\0' || rhs[i] != '\0')
    {
        if (lhs[i] < rhs[i])
        {
            return -1;
        }
        else if (lhs[i] > rhs[i])
        {
            return 1;
        }
        i++;
    }
    return 0;
}

int abs(int number)
{
    return number < 0 ? -number : number;
}

TreeNode* rotateLeft(TreeNode* pivot, TreeNode* parent)
{
    TreeNode* h = pivot->right;
    pivot->right = h->left;
    h->left = pivot;
    updateHeight(pivot);
    updateHeight(h);
    if (parent)
    {
        if (parent->left == pivot)
        {
            parent->left = h;
        }
        else
        {
            parent->right = h;
        }
        updateHeight(parent);
    }

    return h;
}

TreeNode* rotateRight(TreeNode* pivot, TreeNode* parent)
{
    TreeNode* h = pivot->left;
    pivot->left = h->right;
    h->right = pivot;
    updateHeight(pivot);
    updateHeight(h);
    if (parent)
    {
        if (parent->left == pivot)
        {
            parent->left = h;
        }
        else
        {
            parent->right = h;
        }
        updateHeight(parent);
    }

    return h;
}

void updateHeight(TreeNode* h)
{
    if (height(h->right) > height(h->left))
    {
        h->height = height(h->right);
    }
    else
    {
        h->height = height(h->left);
    }
}

void Rotate(TreeNode* h, TreeNode** parent, TreeNode** root)
{
    //Neue Höhe bekommen
    updateHeight(h);
    //balanz checken
    int balance = getBalance(h);
    if (abs(balance) == 2)
    {
        //korrigiern
        if (balance > 0)
        {
            if (getBalance(h->left) < 0)
            {
                rotateLeft(h->left, h);
            }
            TreeNode* newparent = rotateRight(h, *parent);
            if (h == *root)
            {
                *root = newparent;
            }
        }
        else
        {
            if (getBalance(h->right) > 0)
            {
                rotateRight(h->right, h);
            }
            TreeNode* newparent = rotateLeft(h, *parent);
            if (h == *root)
            {
                *root = newparent;
            }
        }
    }
}

int getBalance(TreeNode* h)
{
    //Balance = höhe des linkensubtree minus höhe des rechten subtree
    //0 ist voll balanziert, 1 ist linkslehnend, -1 ist rechtslehnen, -2 und 2 sind unbalanziert
    return height(h->left) - height(h->right);
}

unsigned char height(TreeNode* h)
{
    //height = Maximale Anzahl der Kanten von der Note zur untersten Ebene (Kanten...Verbindung zwischen zwei Noten)
    return (unsigned char)(h ? h->height + 1 : 0);
}

void recursiveAdd(TreeNode* h, TreeNode* p, TreeNode* parent, TreeNode** root)
{
    //suchen nach Platz für neue Note, durch rekursiver Aufruf wird kein Elternpointer gebraucht
    if (strcmp(p->nachname, h->nachname) >= 0)
    {
        if (h->right)
        {
            recursiveAdd(h->right, p, h, root);
        }
        else
        {
            h->right = p;
        }
    }
    else
    {
        if (h->left)
        {
            recursiveAdd(h->left, p, h, root);
        }
        else
        {
            h->left = p;
        }
    }
    //Balancieren
    Rotate(h, &parent, root);
}

void addNode(TreeNode* root, TreeNode* p)
{
    recursiveAdd(root, p, 0, &root);
}

int main()
{
    TreeNode root = {"Markus", "Boeck","Herr",10000ull,"Kammersdorf",2033llu};
    TreeNode newNode = {"Lukas","Damianschitz"};
    TreeNode newNode2 = {"Philip", "Oberndorfer"};
    addNode(&root, &newNode);
    addNode(&root, &newNode2);
    return newNode.height;}