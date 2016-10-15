/*
** list.h for Mara
**
** Made by Pierre Surply
** <pierre.surply@gmail.com>
**
** Started on  Tue Aug 20 10:30:42 2013 Pierre Surply
** Last update Tue Aug 20 10:38:50 2013 Pierre Surply
*/

#ifndef _MARA_LIST_
#define _MARA_LIST_

#include <mara/types.h>
#include <stdlib.h>

typedef struct s_Mlist_node*    Mlist_node;

struct s_Mlist_node
{
        Mundef          value;
        Mlist_node      next;
};

typedef struct s_Mlist*         Mlist;

struct s_Mlist
{
        size_t          length;
        Mlist_node      first;
};

#endif /* _MARA_LIST_ */
