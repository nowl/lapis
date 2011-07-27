/* See http://en.wikipedia.org/wiki/AA_tree for a description of AA
 * Trees */

#include "lapis.h"

aatree_node_t *
aatree_first(aatree_node_t *root)
{
    if(!root) return NULL;       

    if (!root->left) return NULL;
    while (root->left) root = root->left;
    return root;
}

aatree_node_t *
aatree_next(aatree_node_t *n)
{
    if(!n) return NULL;

    if (n->right) {
        n = n->right;
        while (n->left) n = n->left;
    }
    else {
        if (!n->parent) return NULL;

        while (n->parent && n->parent->right == n) n = n->parent;
        n = n->parent;
    }
    return n;
}

aatree_node_t *
aatree_find(aatree_node_t *root, unsigned long hash)
{
    while(root != NULL)
    {
        if(root->hash < hash) root = root->right;
        else if(root->hash > hash) root = root->left;
        else return root;
    }

    return NULL;
}

static aatree_node_t *
skew(aatree_node_t *T)
{
    if(T && T->left && (T->left->level == T->level))
    {
        aatree_node_t *L = T->left;
        L->parent = T->parent;
        if(L->parent)
        {
            if(L->parent->left == T)
                L->parent->left = L;
            else
                L->parent->right = L;
        }
        T->left = L->right;
        T->parent = L;
        if(T->left) T->left->parent = T;
        L->right = T;
        return L;
    }
    else
        return T;
}

static aatree_node_t *
split (aatree_node_t *T)
{
    if(T && T->right && T->right->right &&
       (T->right->right->level == T->level))
    {
        aatree_node_t *R = T->right;
        R->parent = T->parent;
        if(R->parent)
        {
            if(R->parent->left == T)
                R->parent->left = R;
            else
                R->parent->right = R;
        }
        T->right = R->left;
        T->parent = R;
        if(T->right) T->right->parent = T;
        R->left = T;       
        R->level++;
        return R;
    }
    else
        return T;
}

aatree_node_t *
aatree_insert(aatree_node_t *T, aatree_node_t *n)
{
    n->level = 1;
    n->left = n->right = n->parent = NULL;
   
    if(!T) return n;

    /* find spot to insert */
    aatree_node_t *leaf = T, *tmp;
    while(leaf)
    {
        tmp = leaf;
        if(n->hash < leaf->hash)
            leaf = leaf->left;
        else
            leaf = leaf->right;
    }

    n->parent = tmp;

    /* insert here */
    if(n->hash < tmp->hash)
        tmp->left = n;
    else
        tmp->right = n;
   
    /* rebalance */
    while(tmp)
    {
        tmp = skew(tmp);
        tmp = split(tmp);
        n = tmp;
        tmp = tmp->parent;
    }

    return n;
}

#ifndef MIN
# define MIN(a, b) ( (a) <= (b) ? (a) : (b) )
#endif

static aatree_node_t *
decrease_level(aatree_node_t *T)
{
    if(!T) return T;
    int l = 0, r = 0;
    if(T->left) l = T->left->level;
    if(T->right) r = T->right->level;
    int should_be = MIN(l, r) + 1;
    if(should_be < T->level)
    {
        T->level = should_be;
        if(T->right && (should_be < T->right->level))
            T->right->level = should_be;
    }
    return T;
}

static aatree_node_t *
succ(aatree_node_t *T)
{
    aatree_node_t *n, *p;
    for(n=T->right, p=n; n; p=n, n=n->left) {}
    return p;
}

static aatree_node_t *
pred(aatree_node_t *T)
{
    aatree_node_t *n, *p;
    for(n=T->left, p=n; n; p=n, n=n->right) {}
    return p;
}

aatree_node_t *
aatree_delete(aatree_node_t *T, aatree_node_t *n)
{
    if(n && n->owns_data) free(n->data);

    if(!T) return T;
   
    aatree_node_t *tmp;

    if(!n->right && !n->left)
    {
        /* this is a leaf node */
        tmp = n->parent;

        if(n->parent)
        {
            if(n->parent->left == n)
                n->parent->left = NULL;
            else
                n->parent->right = NULL;
        }
        else
            return NULL;
    }
    else if(!n->left)
    {
        /* find successor and replace */

        aatree_node_t *L = succ(n);
        if(L->right)
            L->right->parent = L->parent;

        if(L->parent->left == L)
            L->parent->left = L->right;
        else
            L->parent->right = L->right;
       
        tmp = L->parent;
        n->hash = L->hash;
        n->data = L->data;
    }
    else
    {
        /* find predecessor and replace */

        aatree_node_t *L = pred(n);
        if(L->left)
            L->left->parent = L->parent;
       
        if(L->parent->left == L)
            L->parent->left = L->right;
        else
            L->parent->right = L->right;
           
        tmp = L->parent;
        n->hash = L->hash;
        n->data = L->data;
    }
   
    /* rebalance */
    while(tmp)
    {
        tmp = decrease_level(tmp);
        tmp = skew(tmp);
        if(tmp->right) tmp->right = skew(tmp->right);
        if(tmp->right && tmp->right->right) tmp->right->right = skew(tmp->right->right);
        tmp = split(tmp);
        if(tmp->right) tmp->right = split(tmp->right);

        n = tmp;
        tmp = tmp->parent;
    }

    return n;
}

#if 0
static void
print_tree(aatree_node_t *n, int depth)
{
    if(!n) return;

    int i;
    for(i=0; i<depth; i++)
        printf(" ");
    printf("%d %d\n", n->hash, n->level);
    print_tree(n->left, depth+1);
    print_tree(n->right, depth+1);
}

//#define MAX 1000000
#define MAX 10000

int main (void)
{
    aatree_node_t *t, *s;
    t = malloc(sizeof(*t) * MAX);
    int i, j, maxdepth = 0;
    aatree_node_t *root = NULL; 
 
    for (i = 0; i < MAX; i++)
    {
        t[i].hash = (i * 523 + 342) % 1601;
        t[i].data = malloc(32);
        t[i].owns_data = 1;
        root = aatree_insert(root, t + i);
    }

    while(i-- > 0)
        root = aatree_delete(root, root);
    
    free(t);
}
#endif
