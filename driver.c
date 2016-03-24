int scheme_entry();

int main() {
	int* wram = (int*)0x02000000;

	*(wram) = scheme_entry();

	while(1);

	return 0;
}
