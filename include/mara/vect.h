/*
** vect.h for Mara
**
** Made by Pierre Surply
** <pierre.surply@gmail.com>
**
** Started on  Mon Aug 19 23:29:58 2013 Pierre Surply
** Last update Tue Aug 20 10:38:54 2013 Pierre Surply
*/

#ifndef _MARA_VECT_
#define _MARA_VECT_

#include <mara/types.h>
#include <stdlib.h>

struct s_Mvect
{
        size_t          length;
        Minteger        v[1];
};

typedef struct s_Mvect*         Mvect;

#endif /* _MARA_VECT_ */
