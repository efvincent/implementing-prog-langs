/*
Not intended to be an actual C program - this is to test the 
parser / lexer / type checker exercieses from the book
*/
int fib(int i)
{
    if (i <= 1)
        return 1;
    else
        return (i * fib(i) - 1);
}

double blocks()
{
    {
        int x;
        {
            x = 3;    // x : int
            double x; // x : double
            x = 3.14;
            int z;
        }
        x = x + 1; // x : int, receives the value 3 + 1
        z = 8;     // ILLEGAL! z is no more in scope
        double x;  // ILLEGAL! x may not be declared again
        int z;     // legal, since z is no more in scope
    }
}

int main(int argc, int argv)
{
    int i;
    int j = 0;
    double name, address;

    if (i < 0)
    {
        printf("i was less than zero");
        while (false != true)
        {
            j = fib(i);
            printfn(1, 23, "eric");
        }
    }
    else
    {
        printf("i was zero or more %i", i);
    }
}