struct point {
	int x, y;
};

struct fops {
	int open, read, write, close;
};

struct fam {
    int i;
    int arr[];
};

void foo( struct point p1, struct point p2)
{
    int a[6] = { [4] = 29, [2] = 15 };
    struct { int x,y; } ar[ 4] = { [1].x=23, [3].y=34, [1].y=-1, [1].x=12};
    struct fops f2 = { .open=0, .close=1, .read=2};

    struct fam *s = malloc(sizeof(struct fam) + 17ul*sizeof(int));
    s->arr[5] = 12;
}

void copy2( char* restrict s1, char* restrict s2, int n)
{
	while (n--)
		*s1++ = *s2++;
}

int main() {
    struct point p1 = { 2, 4}; // this is standard
	p1 = (struct point){ 1, 3}; // this is new

	// passing to the function
	foo( (struct point){ 10, 11}, (struct point){ 1, 2});

	// constructing an array
	char **sx = (char *[]){ "Adam", "Eva", "Simon"};

    return 0;
}
