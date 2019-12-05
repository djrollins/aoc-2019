#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

bool test_part1(unsigned int x) {
	bool has_double_digit = false;
	unsigned int d1 = UINT_MAX;

	while (x) {
		unsigned int d0 = x % 10;

		if (d0 > d1) {
			return false;
		}

		has_double_digit = has_double_digit || (d0 == d1);

		d1 = d0;
		x = x / 10;
	}

	return has_double_digit;
}

bool test_part2(unsigned int x) {
	bool has_double_digit = false;
	unsigned int d3 = UINT_MAX;
	unsigned int d2 = UINT_MAX;
	unsigned int d1 = UINT_MAX;

	while (x) {
		unsigned int d0 = x % 10;

		if (d0 > d1) {
			return false;
		}

		/* 
		 * check that the two centre digits in the window (d1, d2) are the equal to each other,
		 * but different to the two outer digits of the window (d0, d2).
		 */
		has_double_digit = has_double_digit || (d1 == d2) && (d1 != d0) && (d2 != d3);

		d3 = d2;
		d2 = d1;
		d1 = d0;
		x = x / 10;
	}

	/* check that the front two digits are equal to eachother but different from the third */
	return has_double_digit || (d1 == d2) && (d2 != d3);
}

int main() {
	unsigned int start;
	unsigned int end;

	unsigned int part1 = 0;
	unsigned int part2 = 0;

	if (scanf("%u-%u", &start, &end) != 2) {
		fprintf(stderr, "ERROR: failed to read stdin");
		return EXIT_FAILURE;
	}

	for (unsigned int i = start; i <= end; ++i) {
		if (test_part1(i)) {
			++part1;
		}
		if (test_part2(i)) {
			++part2;
		}
	}

	printf("part 1: %d\n", part1);
	printf("part 2: %d\n", part2);
}
