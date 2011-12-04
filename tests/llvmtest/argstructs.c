

typedef struct {
  float x;
  float y;
  float z;
} Point;

void putPoint( Point p )
{
}

Point getPoint()
{
  Point p;
  return p;
}

Point globalPoint = { 10, 20, 30 };

int main()
{
  Point p = { 10, 20, 30 };
  
/*   putPoint( p ); */

  p = getPoint();

  return 100;
}


