// Fibonacci computation in C--

int main() {
  int n = 10;
  int i = 0;
  int j = 1;
  int k = 0;
  while (k < n) {
    int h = j;
    j = j + i;
    i = h;
    k = k + 1;
  }
  printInt(i);
  return 0;
}

// Stms: SInit SExp SReturn SWhile SBlock
// Exps: EInt EId EAss ELt EAdd EApp
// builtin: printInt
