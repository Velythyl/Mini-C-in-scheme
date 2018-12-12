{
	a = 7;
	if (a < 8) {
		a = a + 1;
		print(a);
	} else {
		a = a - 1;
		print(a);
	}
	if (a < 8) {
		a = a + 1;
		print(a);
	} else {
		a = a - 1;
		print(a);
	}

	a = 1;
	b = 2;
	while (a < 10) {
		print(a);
		print(b);
		a = a + b + 1;
                b = b * a;
	}

	a = 1;
	do {
		print(a);
		a = a + 1;
	} while (a < 5);
}
