int scheme_entry();

int main() {
	char* iwram = (char*)0x03000000;

	*(iwram + 0x100) = scheme_entry();

	while(1);

	return 0;
}
