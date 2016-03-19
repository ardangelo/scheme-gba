int scheme_entry();

int main() {
	int* iwram = (int*)0x03000000;

	*iwram = scheme_entry();

	while(1);

	return 0;
}
