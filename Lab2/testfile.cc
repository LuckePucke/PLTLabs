int main() {
  printInt(fac(5));
  return 0 ;
}

int fac (int a) {
  printInt(1);
  int r;
  int n;
  printInt(2);
  r = 1;
  n = a;
  printInt(3);
  while (n > 0)
  {
	printInt(4);
    r = r * n;
    n = n - 1;
  }
  printInt(5);
  return r;
  printInt(6);
}

