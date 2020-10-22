#define hidden __attribute__((__visibility__("hidden")))
hidden int x;
int main()
{
    x = 17;

	return 0;

}
