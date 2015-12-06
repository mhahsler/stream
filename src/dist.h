// Dist helpers

#ifndef LT_POS
#define LT_POS(n, i, j)					\
    (i)<(j) ? (n*(i) - ((i)+1)*(i)/2 + (j)-(i)) -1	\
	    : (n*(j) - ((j+1))*(j)/2 + (i)-(j)) -1
#define LT_SIZE(n) n*(n-1)/2
#endif
