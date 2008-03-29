#include <stdlib.h>

#include "hd.h"

struct ABC {
	int a;
};

Abc *
construct(int a)
{
	Abc *m = malloc(sizeof(Abc));
	m->a = a;
	return m;
}	
