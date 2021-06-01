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