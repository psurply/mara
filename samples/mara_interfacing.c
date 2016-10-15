#include <mara/types.h>
#include <mara/vect.h>
#include <mara/list.h>


Minteger list_sum(Mlist l)
{
        Mlist_node      e;
        Minteger        s;

        e = l->first;
        s = 0;

        while (e)
        {
                s += e->value;
                e = e->next;
        }

        return s;
}


Minteger vect_sum(Mvect v)
{
        size_t          i;
        Minteger        s;

        s = 0;

        for (i = 0; i < v->length; ++i)
                s += v->v[i];

        return s;
}
