
#include "hd.h"

int
main()
{
	Abc *abc;

	abc = construct(123);
	printf("%d\n", abc->a);
	return 0;
}
