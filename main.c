int scheme();

int main() {
	int* iwram = (int*)0x03000000;

	*(iwram + (0x100/sizeof(int))) = scheme();

	while(1);

	return 0;
}
