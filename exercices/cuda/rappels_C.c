#include <stdlib.h>
//Contains general purpose funcitons
#include <stdio.h>
//Contains usual Input/Output functions

//Main function of a C program, the "main" name is mandatory
//the program will start with this function
int main()
{
	//an instruction end with a ";" character
	int i;//declaration of a integer type
	int i2 = 0;//declaration and affectation of an integer type
	
	float f;
	float f2 = 3.6f; //the f specify the simple precision of the real 3.6
	//f is not mandatory, an automatic conversion will be done from double to float
	
	double d;
	double d2 = 3.6; //double is the default presion of real numbers
	
	//only affectations
	i = 1;
	f = 4.5f; d = 4.5;
	//Note that multiple instruction can be given on one line
	
	//usual function to print something on the standard output
	printf("My integers: %d %d, my floats: %f %.2f, my doubles: %lf %g \n", i, i2, f, f2, d, d2);
	// the % are "specifiers" allowing to print numbers "d" is integer, "f" is float, "d" is double
	// "lf" is double, "g" is any real with automatic format, "[.precision]f" specify the printed precision   
	// The function adapt the number of argument based on the number if specifers given.
	// "\n" specify a line break, which is not automated
	
	//basic loop
	for(i = 0; i < 20; i++)
	{
		printf("i value : %d\n", i);
	}
	//Instruction block are marked with "{" and "}"
	//note that there is no ";" at the end of the "for" instruction line !

	//basic condition
	if(f2 > 1.0)
	{
		printf("Condition > 1.0\n");
	}
	else if(f2 >= 0.0) 	//else if can be stacked any number of time
	{
		printf("Condition < 1.0 AND >= 0.0\n");
	}
	//all remaining cases
	else
	{
		printf("No matching case\n");
	}
	
	//Combined condition
	if(f > 2.4 && d < 12.6)
	{
		printf("Double condition checked successfully\n");
	}
	
	//Or statement
	if(f > 2.4 || d < 12.6)
	{
		printf("One of the combined condition is fulfilled\n");
	}
	

	exit(EXIT_SUCCESS);
	//Usual way of exiting properly the program, not mandatory
}
