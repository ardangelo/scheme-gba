int scheme();

int main() {
	char* iwram = (char*)0x03000000;

	*(iwram + 0x100) = scheme();

	while(1);

	return 0;
}
