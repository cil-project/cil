
double distv(double * v)
{
  // Funny stuff
  double * v = v;
  
  
  return *v;
}


int main() {
  double v = 1.0;

  double v = 2.0;
  
  if(1.0 != distv(&v)) {
    return 1;
  }
  return 0;
}
