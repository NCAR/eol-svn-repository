#include <sys/types.h>
#include <sys/dir.h>

extern int scandir();
extern int alphasort();
extern qsort();

choose(d)
struct direct *d;
    {
    return(!strncmp("adq", d->d_name, 3));
    }

main(argc, argv)
int argc;
char **argv;
    {
    struct direct **dlist;
    int nfiles;

    if ((nfiles=scandir(argv[1], &dlist, choose, alphasort))
	== -1)
	{
	puts("AIEEEEEE");
	exit(1);
	}
    printf("%d files matched.\n", nfiles);
    }
